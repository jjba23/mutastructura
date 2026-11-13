# Vision

Mutastructura runs plain SQL migration files against SQLite3, PostgreSQL,
CockroachDB and MySQL/MariaDB, and keeps a checksummed ledger of what ran. The
direction is not to grow into Alembic or Liquibase: it is to finish the
operational story a *small* SQL-first tool needs — adopting an existing database,
handing a script to a DBA, knowing why a rollback refused — while keeping the two
properties that make it worth having: SQL is the source of truth, and the tool
refuses rather than guesses.

## Where Mutastructura is today

Seven commands, one plan. `migrate` applies what is pending; `status` and
`validate` report the same plan, the second with an exit status a CI pipeline can
trust; `rollback` walks back with `*.undo.sql` scripts; `repair` forgets a recorded
failure or realigns a checksum; `clean` drops what the catalogue reports; `help`
prints the bundled text. ~4,200 lines of Guile across seven modules:
`mutastructura.scm` (1,368 lines) for the engine registry, connection specs,
discovery and planning, `main.scm` (780) for the CLI, then
`persistence/dialect.scm`, `migration-runner.scm`, `sql-parser.scm`, `util.scm` and
`runners.scm`. Four Veritas unit files, 119 `test` forms between them, including the
SQL runner exercised against a real database.

The engine layer is the part that is hard to copy, so it is worth naming: one
transaction per forward run where the engine can undo DDL, and an honest sentence
on MySQL where it cannot; a lock that fits each engine — `BEGIN IMMEDIATE` on
SQLite, `pg_advisory_lock` on PostgreSQL, `GET_LOCK` on MySQL, and the transaction
plus the ledger's unique name key on CockroachDB; SHA-256 checksums with `changed`,
`missing`, `superseded` and `beyond-target` detection; `*.repeatable.sql` rerun
when their contents change; `${key}` placeholders; `--target` and `--out-of-order`;
and a real SQL lexer — a finite-state machine over strings, quoted identifiers,
nested block comments, PostgreSQL dollar quoting, `E'...'`, MySQL `DELIMITER` and
trigger bodies — that reports the line a failing statement starts on.

Four decisions constrain everything that follows.

**SQL is the source of truth.** Mutastructura never generates a schema change from
a model. That rules out autogeneration, and with it the whole class of bug where a
diff is half-right.

**The ledger is the record.** `migrations_history` carries a name, a checksum, a
kind, a timestamp, an executor, an execution time and a success flag, and the
runner asks the catalogue which columns exist rather than assuming them, so an
older deployment's ledger is upgraded by being read. Every command is that ledger
read in a different way.

**Refuse rather than guess.** `--steps` and `--to` are all-or-nothing: a range
containing one irreversible migration runs nothing and names every blocker.
`--all` is the smart boundary — it undoes the reversible suffix, names what
stopped it, and exits 0 for the work it did. A checksum change is never silently
accepted; `repair` is the deliberate act.

**A functional core, an imperative shell.** Planning is a pure function of files
and ledger rows; only `main.scm` and the runner touch a database, which is why
`--dry-run` and the unit tests exercise the same decisions a real run makes.

## The vision

1. **Adopting an existing database takes one command.** Today a team pointing the
   tool at a database that already has a schema must hand-write ledger rows —
   exactly the guessing this project refuses everywhere else. `baseline` records
   the state and says plainly that it did not verify it.
2. **A run can be reviewed without a connection.** `--dry-run` prints a report; a
   DBA wants a script. `--sql` emitting executable statements, with placeholders
   substituted and a header naming each migration, makes the tool reviewable by
   people who never install Guile.
3. **Every operational fact is machine-readable.** `status --format json`, with a
   documented schema, because a human table is a rendering and not an interface.
4. **The undo scripts are known to invert.** A `*.undo.sql` is today an article of
   faith: nothing proves running it after its migration returns the schema to where
   it was. A round-trip mode — migrate, roll back, migrate again in a scratch
   database — turns that faith into a test.
5. **Adding an engine is a line in a registry.** The registry already separates
   dialect from driver, which is why `--database crdb` reaches CockroachDB through
   the PostgreSQL driver. A protocol-compatible database should cost one entry and
   no branch in any command, with per-engine differences confined to `dialect.scm`.

## Features that are in scope and not built yet

- **`baseline` / `stamp`.** Record every migration at or below a version as
  applied, with its real checksum and the current timestamp, run no SQL, and
  refuse when the ledger already has entries unless `--force`. The hard part is
  prose, not code: the command must say it records state and does not verify the
  schema, because a user who believes otherwise will baseline a database that does
  not match its files.
- **`--sql`.** The statements are already computed by `runner-statements` for
  `--dry-run`; what is missing is a rendering mode that suppresses banner, log and
  report. The hard part is the honesty the header owes: whether the engine can
  wrap the run in a transaction, and that the script was never validated against a
  live server.
- **Hooks.** SQL files for `before-migrate`, `after-migrate`, `before-each`,
  `after-each` and `after-clean`, for grants, extensions and post-migration checks.
  The hard part is keeping a hook out of discovery and out of the ledger, so a hook
  is never mistaken for a migration.
- **`--lock-timeout`.** MySQL's `GET_LOCK` waits ten seconds today and PostgreSQL's
  advisory lock waits forever. The hard part is that the timeout is a different
  primitive on every engine — `pg_try_advisory_lock` in a loop, MySQL's fourth
  argument, SQLite's busy timeout — which is what `dialect.scm` exists for.
- **Per-migration transaction control.** A `-- mutastructura: no-transaction`
  directive, recognised the way `-- mutastructura: irreversible` already is, so
  `CREATE INDEX CONCURRENTLY` becomes possible. The hard part is that the planner
  and the lock stay coherent when the run is no longer one unit.
- **`status --format json` and a configuration file.** An array of
  `{name, version, kind, state, checksum, applied_on, success}`, and a file so `-m`,
  `-d`, `-c` and `-r` are not repeated. The hard part is a tiny JSON writer with no
  new dependency, and a precedence rule stated once and tested.

## New features that would be worth having

- **`mutastructura lint`**, a pure check of the migration directory with no
  database at all: duplicate versions, an `*.undo.sql` with no migration, an
  `irreversible` directive alongside an undo script, a placeholder nobody
  defined. Speculative in that no code anticipates it, and it fits the project's
  shape exactly — pure, unit-testable, and it turns policy into an exit status.
- **`mutastructura explain VERSION`**, printing *why* a migration is in the state
  it is in: superseded by which version, blocked by which directive, changed in
  which way. The planner computes every one of those reasons and discards them.

## Contributions that would move the needle

1. **`baseline`.** Small because recording reuses `runner-record!`, and it is the
   first thing a team adopting the tool on an existing database asks for. A pure
   `plan-baseline` selecting files at or below a version, plus a `--dry-run` naming
   the rows, is a day's work with a test per refusal.
2. **`--sql`.** Smaller still, since `log-dry-run` already computes the statements;
   the work is a second rendering and the header that tells the truth about
   transactionality. It makes the tool reviewable, which is what converts a
   sceptical team.
3. **`status --format json`.** Pure, testable, and the one gap that makes CI parse
   a human table. The `dialect.scm` suite shows how a rendering decision that
   depends on an engine is tested here.
4. **`mutastructura lint`.** A new command that touches no database, so it can be
   written test-first in `test/veritas/unit/` exactly as the planners are — and the
   cheapest way to make policy enforceable rather than documented.
5. **The round-trip test.** No production code at all, and it would find real bugs
   in the existing undo path, which a user runs when they are already having a bad
   day.
6. **`--lock-timeout`.** Medium effort, confined to `dialect.scm` — the module the
   project already points at for exactly this kind of difference.

## Compared to related projects

| Project | What it is | Where Mutastructura differs |
| --- | --- | --- |
| Flyway | The closest relative: versioned and repeatable SQL files, a checksum-bearing schema history table, `migrate`/`info`/`validate`/`repair`/`baseline`/`clean`, placeholders, callbacks, `target` and `outOfOrder`, with `undo` in a paid tier. | It matches Flyway's migration model — versioned plus repeatable, checksums, placeholders, target, out-of-order, validate, repair, rollback — and trails on the operational edges: baseline, callbacks, offline SQL. Against that it is 4,200 lines of readable Guile with no JVM, and `rollback` is in the free tool. |
| Liquibase | Changesets in XML, YAML, JSON or formatted SQL, with `update`, `status`, `validate`, `rollback`, `changelog-sync`, `update-sql`, a checksum per changeset, a lock table, preconditions, contexts and labels. | Liquibase owns a changelog format; this owns a directory of SQL files and a naming convention. Liquibase generates rollback only for modelled changesets — for arbitrary SQL it cannot, which is why undo files are authored here and said to be. |
| Alembic | Python/SQLAlchemy migrations over an explicit revision graph: `upgrade`, `downgrade`, `stamp`, `history`, `heads`, `branches`, `merge`, `check`, and connection-free SQL generation. | Alembic's revision graph solves parallel development against one database; a linear history is a deliberate simplification for the deployments this targets, with dependencies as the pragmatic middle not yet built. Alembic has `stamp` and `--sql`; this does not, yet. |
| Django migrations | Migrations generated from model state, applied with `migrate`, inspected with `showmigrations`, printed with `sqlmigrate`, squashed with `squashmigrations`, baselined with `--fake-initial`, with `pre_migrate` and `post_migrate` signals. | Django owns the model, so the migration is a consequence; here the SQL is the input and nothing is generated. Squashing and lifecycle signals have no counterpart, and `--fake-initial` is the feature this project most needs. |
| yoyo | Python, reversible steps with per-migration transactions and savepoints, a `post-apply` hook, `mark`/`unmark` for baselining, `break-lock`, and declared dependencies. | The other small tool that thinks about rollback seriously. It has per-migration transactions and `break-lock`, which this lacks; it has no checksum column, so a file edited after it was applied is invisible, which is where this is ahead. |
| Sqitch | A change-based system in Perl: a change is a directory of `deploy`, `revert` and `verify` scripts, with a plan file, dependency declarations and provider locks. | The only one here with a `verify` script — an idea worth stealing for the round-trip task. Its plan file is a second format to maintain; ordering here is the file name. |
| Goose | A Go migration tool with up/down sections in one file, `NO TRANSACTION` annotations for statements an engine cannot run in a transaction, and an embedded package API. | Goose's `NO TRANSACTION` is the model for a per-migration transaction directive here. One file per direction suits Goose; separate `*.undo.sql` files are what let `--all` walk a boundary. |
| Prisma Migrate | A TypeScript model-first workflow with a shadow database that replays history to detect drift, plus documented baselining and squashing. | A different product: the shadow database exists because there is a model to drift from. With no model, checksums plus `validate` are the history integrity, at a fraction of the machinery. |
| dbmate | A dependency-free Go migration runner: numbered files with optional `-- migrate:down` sections, one command, environment-variable configuration. | The closest peer in spirit — small, SQL-first, one binary. dbmate has no repeatables, no placeholders and no rollback policy beyond what the file says; the ledger and its refusal rules are the difference. |

What the others do that Mutastructura could still add: Flyway's `baseline` and
callbacks, Alembic's `--sql` and `stamp`, Django's `--fake-initial`, yoyo's
`break-lock` and per-migration transactions, Goose's `NO TRANSACTION`, Sqitch's
`verify`. That list is almost exactly `ROADMAP.org`'s P0 and P1, and that is the
point: the gaps are known, named and small, and none of them changes what a
migration is. What must not be imported is a changelog format, preconditions,
contexts or labels — each is a second language for saying what SQL already says.

Where Mutastructura is ahead and should stay ahead: a real SQL lexer rather than a
split on semicolons, so a trigger body or a dollar-quoted function survives being
applied and a failure names the line it started on; a rollback that treats
irreversibility as a first-class fact with three causes and an explicit override,
rather than as a missing file; a ledger that adapts to the columns a deployment
actually has instead of demanding a schema upgrade; and an engine registry that
separates dialect from driver.

## In the long run

**The ledger is the contract.** `migrations_history` is not an internal detail: it
is the artefact an operator queries at 3am, and the surface another tool could
read. A stable, additive ledger — columns discovered rather than assumed, meaning
never repurposed — is worth more than any single command, and keeping it readable
is what makes the tool trustworthy in an incident.

**Refusal compounds.** Every convenience that guesses — autogeneration, drift
detection, automatic rollback synthesis, a best-effort range undo — is declined
here, and the accumulation of those refusals is the product's character. A tool that
has never silently done the wrong thing is one an operator will run against
production, and that permission is the whole asset.

**Small is a maintenance strategy.** Four thousand lines of Guile, one language, one
dependency family, a declared Guix environment and a container image: someone can
read the whole program in a week and change the part that is wrong. A migration tool
people can inspect outlives one they can only configure.

## Getting involved

`maak test` runs the Veritas unit suite — the pure planners and the SQL runner
against a real SQLite database — inside `guix time-machine
--channels=channels.scm -- shell -m manifest.scm`. When you fix a bug, add the case
that would have caught it; the tests are the specification, and `ROADMAP.org` is
where the argument about what to build next already lives. `AGENTS.md` is the
repository's rulebook and wins over anything argued here.

*This document describes intent, not commitments: it names directions the project
would like to take, and no item in it is promised for any release.*

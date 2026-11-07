-- MUTASTRUCTURA - Relational Schema Migrations Powered by Lisp

-- SPDX-License-Identifier: MIT
-- Copyright © Josep Bigorra <jjbigorra@gmail.com>

-- The ledger a deployment starts with.  It has to carry the same columns as the
-- definition this program generates for itself (`dialect-history-ddl'), because
-- a runner writes the columns it finds and reads the ones that are there: a
-- ledger created from here and one created from the code must be the same
-- ledger.  A deployment that has been migrating since before a column existed is
-- not altered -- the runner writes what its own ledger has, and reads every
-- entry as the shape it is.

CREATE TABLE IF NOT EXISTS migrations_history (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    checksum VARCHAR(64) NOT NULL,
    description VARCHAR(512),
    kind VARCHAR(32) NOT NULL DEFAULT 'versioned',
    installed_on DATETIME DEFAULT CURRENT_TIMESTAMP,
    installed_by VARCHAR(255),
    execution_time INTEGER DEFAULT 0,
    success TINYINT NOT NULL DEFAULT 1,
    UNIQUE KEY migrations_history_name_idx (name)
) ENGINE=InnoDB;

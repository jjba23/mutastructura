#
# spec file for package mutastructura
#
# Copyright (c) 2026 SUSE LLC and contributors
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

# Please submit bugfixes or comments via http://bugs.opensuse.org/
#

# Scheme sources plus a bash launcher: there is no compiled binary to
# debug, so the empty -debugsource package is skipped.
%global debug_package %{nil}

Name:           mutastructura
Version:        0.9.4
Release:        0
Summary:        Relational database schema migrations in Guile Scheme
License:        MIT
URL:            https://github.com/jjba23/mutastructura
Source0:        %{name}-%{version}.tar.gz
BuildRequires:  pkgconfig(guile-3.0)
BuildRequires:  guile-gcrypt
BuildRequires:  guile-dbi
Requires:       guile
Requires:       bash
Requires:       coreutils
Requires:       guile-gcrypt
Requires:       guile-dbi

# NOTE: guile-dbi is not packaged in openSUSE yet.  Build it first
# (from a devel project if needed) before this package.

%description
Mutastructura applies relational database schema migrations written as
S-expressions. It tracks which migrations have run, supports SQLite, MySQL
and PostgreSQL through guile-dbi, and can run forwards, backwards or to a
named target.

%prep
%autosetup -n %{name}-%{version}

# The launcher is a bash script; point it at the interpreter path
# openSUSE packages are expected to use instead of /usr/bin/env.
sed -i '1s|^#!%{_bindir}/env \(.*\)$|#!%{_bindir}/\1|' scripts/mutastructura

%build
# Nothing to build: the program is Scheme sources plus a launcher, and
# the sources are byte-compiled during %install so that they land in
# the interpreter's compiled-module directory.

%install
# Both directories are asked of Guile's pkg-config file rather than
# assumed, so the modules land where this distribution's Guile looks
# for them.
SITE_DIR="%{buildroot}$(pkg-config --variable=sitedir guile-3.0)"
CCACHE_DIR="%{buildroot}$(pkg-config --variable=siteccachedir guile-3.0)"

mkdir -p "$SITE_DIR/mutastructura" "$CCACHE_DIR/mutastructura"
cp -r src/mutastructura/. "$SITE_DIR/mutastructura/"

# Byte-compile every module so that loading it does not compile it
# again on first use.
find "$SITE_DIR/mutastructura" -name '*.scm' | while read -r scm_file; do
  rel_path="${scm_file#$SITE_DIR/}"
  go_file="$CCACHE_DIR/${rel_path%.scm}.go"
  mkdir -p "$(dirname "$go_file")"
  guild compile -L "$SITE_DIR" -o "$go_file" "$scm_file"
done

install -D -m 0755 scripts/mutastructura %{buildroot}%{_bindir}/mutastructura

# Upstream reads resources from the generic share/resources; several of
# these packages would clash there, so it moves under share/mutastructura/.
sed -i 's|share/resources|share/mutastructura/resources|g' %{buildroot}%{_bindir}/mutastructura

install -d "%{buildroot}%{_datadir}/mutastructura/resources"
cp -r resources/. "%{buildroot}%{_datadir}/mutastructura/resources/"

# Upstream's SQLite runner tests fail against guile-dbi 3.2.0, and Debian and openSUSE do not package guile-dbi; the suite is not run.
# %check omitted.

%files
%license LICENSE
%doc README.org
%{_bindir}/mutastructura
%dir %{_datadir}/guile
%dir %{_datadir}/guile/site
%dir %{_datadir}/guile/site/3.0
%{_datadir}/guile/site/3.0/mutastructura
%dir %{_libdir}/guile
%dir %{_libdir}/guile/3.0
%dir %{_libdir}/guile/3.0/site-ccache
%{_libdir}/guile/3.0/site-ccache/mutastructura
%dir %{_datadir}/mutastructura
%{_datadir}/mutastructura/resources

%changelog

#!/usr/bin/env bash

# Lenkesamler

# Copyright © Josep Jesus Bigorra Algaba

# lenkesamler is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# lenkesamler is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.

# You should have received a copy of the GNU Affero General Public License
# along with lenkesamler. If not, see <https://www.gnu.org/licenses/>.

set -euo pipefail

red_text='\033[0;31m'
reset_text='\033[0m'

log_error() {
  if [ -z "${1}" ]; then
    log_error "log_error was called without a message!"
    return 1
  fi

  echo -e "${red_text}[error] $1${reset_text}" >&2
}

log_info() {
  if [ -z "${1}" ]; then
    log_error "log_info was called without a message!"
    return 1
  fi

  echo -e "$1"
}

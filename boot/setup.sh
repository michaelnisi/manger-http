#!/usr/bin/env bash

set -o xtrace

SOURCE="${BASH_SOURCE[0]}"
if [[ -h $SOURCE ]]; then
  SOURCE="$(readlink "$SOURCE")"
fi

DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

main() {
  local name=$(uname -s)
  case $name in
  "Darwin")
    . "${DIR}/darwin.sh"
    exit $(darwin)
    ;;
  "SunOS")
    . "${DIR}/smartos.sh"
    exit $(smartos)
    ;;
  *)
    echo "${name} not supported"
    exit 1
    ;;
  esac
}

main

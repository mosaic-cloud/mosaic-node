#!/bin/bash

set -e -E -u -o pipefail || exit 1
test "${#}" -eq 0

cd -- "$( dirname -- "$( readlink -e -- "${0}" )" )"

set -x

rm -Rf ./generated
mkdir ./generated

cp -T ./repositories/mochiweb/src/mochiweb.app.src ./generated/mochiweb.app

exit 0

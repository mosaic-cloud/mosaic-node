#!/bin/bash

set -e -E -u -o pipefail || exit 1
test "${#}" -eq 0

cd -- "$( dirname -- "$( readlink -e -- "${0}" )" )"

set -x

rm -Rf ./generated
mkdir ./generated

cp -T ./repositories/webmachine/ebin/webmachine.app ./generated/webmachine.app

exit 0

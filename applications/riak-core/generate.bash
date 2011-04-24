#!/bin/bash

set -e -E -u -o pipefail || exit 1
test "${#}" -eq 0

cd -- "$( dirname -- "$( readlink -e -- "${0}" )" )"

set -x

rm -Rf ./generated
mkdir ./generated

cp -T ./repositories/riak-core/ebin/riak_core.app ./generated/riak_core.app
cp -T ./repositories/basho-stats/ebin/basho_stats.app ./generated/basho_stats.app

exit 0

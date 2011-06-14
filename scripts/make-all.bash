#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

./scripts/make
../mosaic-components-rabbitmq/scripts/make
../mosaic-components-riak-kv/scripts/make
../mosaic-components-httpg/scripts/make

exit 0

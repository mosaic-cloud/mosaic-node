#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

"${_workbench}/../mosaic-erlang-dependencies/scripts/make"
"${_workbench}/scripts/make"
"${_workbench}/../mosaic-components-rabbitmq/scripts/make"
"${_workbench}/../mosaic-components-riak-kv/scripts/make"
"${_workbench}/../mosaic-components-httpg/scripts/make"

#"${_workbench}/../mosaic-examples-realtime-feeds/frontend/scripts/make"

exit 0

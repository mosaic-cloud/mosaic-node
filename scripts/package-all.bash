#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

"${_workbench}/scripts/package"

"${_workbench}/../mosaic-components-rabbitmq/scripts/package"
"${_workbench}/../mosaic-components-riak-kv/scripts/package"
"${_workbench}/../mosaic-components-httpg/scripts/package"

"${_workbench}/../mosaic-java-components/components-container/scripts/package"
"${_workbench}/../mosaic-examples-realtime-feeds/backend/scripts/package"

"${_workbench}/scripts/package-boot"

exit 0

#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

"${_workbench}/scripts/publish"

"${_workbench}/../mosaic-components-rabbitmq/scripts/publish"
"${_workbench}/../mosaic-components-riak-kv/scripts/publish"
"${_workbench}/../mosaic-components-httpg/scripts/publish"

"${_workbench}/../mosaic-java-components/components-container/scripts/publish"
"${_workbench}/../mosaic-java-platform/mosaic-mvn/mosaic-cloudlet/scripts/publish"
"${_workbench}/../mosaic-java-platform/mosaic-mvn/mosaic-driver/scripts/publish"

"${_workbench}/../mosaic-examples-realtime-feeds/backend/scripts/publish"
#"${_workbench}/../mosaic-examples-realtime-feeds-java/scripts/publish"

"${_workbench}/scripts/publish-boot"

exit 0

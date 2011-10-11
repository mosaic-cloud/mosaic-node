#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

"${_workbench}/scripts/publish"
"${_workbench}/scripts/publish-boot"

"${_workbench}/../mosaic-node-wui/scripts/publish"

"${_workbench}/../mosaic-components-rabbitmq/scripts/publish"
"${_workbench}/../mosaic-components-riak-kv/scripts/publish"
"${_workbench}/../mosaic-components-httpg/scripts/publish"

if test "${_mosaic_do_all_java:-${_mosaic_do_all:-false}}" == true ; then
	"${_workbench}/../mosaic-java-platform/infrastructure/components-container/scripts/publish"
	"${_workbench}/../mosaic-java-platform/cloudlets/scripts/publish"
	"${_workbench}/../mosaic-java-platform/drivers/scripts/publish"
fi

if test "${_mosaic_do_all_examples:-${_mosaic_do_all:-false}}" == true ; then
	"${_workbench}/../mosaic-examples-realtime-feeds/backend/scripts/publish"
	# "${_workbench}/../mosaic-examples-realtime-feeds-java/scripts/publish"
fi

exit 0

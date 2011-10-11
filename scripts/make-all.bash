#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

# "${_workbench}/../mosaic-erlang-dependencies/scripts/make"

"${_workbench}/scripts/make"

"${_workbench}/../mosaic-components-rabbitmq/scripts/make"
"${_workbench}/../mosaic-components-riak-kv/scripts/make"
"${_workbench}/../mosaic-components-httpg/scripts/make"

if test "${_mosaic_do_all_java:-${_mosaic_do_all:-false}}" == true ; then
	"${_workbench}/../mosaic-java-platform/infrastructure/components-container/scripts/make"
	"${_workbench}/../mosaic-java-platform/cloudlets/scripts/make"
	"${_workbench}/../mosaic-java-platform/drivers/scripts/make"
fi

if test "${_mosaic_do_all_examples:-${_mosaic_do_all:-false}}" == true ; then
	"${_workbench}/../mosaic-examples-realtime-feeds/frontend/scripts/make"
	# "${_workbench}/../mosaic-examples-realtime-feeds-java/scripts/make"
fi

exit 0

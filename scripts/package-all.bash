#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

"${_repositories}/mosaic-erlang-tools/scripts/package"

"${_repositories}/mosaic-node/scripts/package"
"${_repositories}/mosaic-node/scripts/package-boot"

"${_repositories}/mosaic-node-wui/scripts/package"

"${_repositories}/mosaic-components-rabbitmq/scripts/package"
"${_repositories}/mosaic-components-riak-kv/scripts/package"
"${_repositories}/mosaic-components-httpg/scripts/package"

"${_repositories}/mosaic-erlang-drivers/scripts/package"

if test "${_mosaic_do_all_java:-${_mosaic_do_all:-false}}" == true ; then
	"${_repositories}/mosaic-java-platform/components-container/scripts/package"
	"${_repositories}/mosaic-java-platform/cloudlets/scripts/package"
	"${_repositories}/mosaic-java-platform/drivers-stubs/amqp/scripts/package"
	"${_repositories}/mosaic-java-platform/drivers-stubs/riak/scripts/package"
fi

if test "${_mosaic_do_all_examples:-${_mosaic_do_all:-false}}" == true ; then
	"${_repositories}/mosaic-examples-realtime-feeds/backend/scripts/package"
	"${_repositories}/mosaic-examples-realtime-feeds/frontend/scripts/package"
	"${_repositories}/mosaic-java-platform/examples/realtime-feeds-indexer/scripts/package"
fi

exit 0

#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

"${_repositories}/mosaic-erlang-tools/scripts/compile"

"${_repositories}/mosaic-node/scripts/compile"

"${_repositories}/mosaic-components-rabbitmq/scripts/compile"
"${_repositories}/mosaic-components-riak-kv/scripts/compile"
"${_repositories}/mosaic-components-httpg/scripts/compile"

"${_repositories}/mosaic-erlang-drivers/scripts/compile"

if test "${_mosaic_do_all_java:-${_mosaic_do_all:-false}}" == true ; then
	"${_repositories}/mosaic-java-platform/components-container/scripts/compile"
	"${_repositories}/mosaic-java-platform/cloudlets/scripts/compile"
	"${_repositories}/mosaic-java-platform/drivers-stubs/amqp/scripts/compile"
	"${_repositories}/mosaic-java-platform/drivers-stubs/riak/scripts/compile"
fi

if test "${_mosaic_do_all_examples:-${_mosaic_do_all:-false}}" == true ; then
	"${_repositories}/mosaic-examples-realtime-feeds/frontend/scripts/compile"
	"${_repositories}/mosaic-java-platform/examples/realtime-feeds-indexer/scripts/compile"
fi

exit 0

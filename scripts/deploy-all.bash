#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

"${_repositories}/mosaic-erlang-tools/scripts/deploy"

"${_repositories}/mosaic-node/scripts/deploy"
"${_repositories}/mosaic-node/scripts/deploy-boot"

"${_repositories}/mosaic-node-wui/scripts/deploy"

"${_repositories}/mosaic-components-rabbitmq/scripts/deploy"
"${_repositories}/mosaic-components-riak-kv/scripts/deploy"
"${_repositories}/mosaic-components-httpg/scripts/deploy"

"${_repositories}/mosaic-erlang-drivers/scripts/deploy"

if test "${_mosaic_do_all_java:-${_mosaic_do_all:-false}}" == true ; then
	"${_repositories}/mosaic-java-platform/components-container/scripts/deploy"
	"${_repositories}/mosaic-java-platform/cloudlets/scripts/deploy"
	"${_repositories}/mosaic-java-platform/drivers/scripts/deploy"
fi

if test "${_mosaic_do_all_examples:-${_mosaic_do_all:-false}}" == true ; then
	"${_repositories}/mosaic-examples-realtime-feeds/backend/scripts/deploy"
	"${_repositories}/mosaic-examples-realtime-feeds/frontend/scripts/deploy"
	"${_repositories}/mosaic-java-platform/examples/realtime-feeds-indexer/scripts/deploy"
fi

exit 0

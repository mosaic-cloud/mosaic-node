#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

echo "[ii] packaging ${_package_name}-boot..." >&2

if test -e "${_outputs}/package-boot" ; then
	rm -R "${_outputs}/package-boot"
fi
if test -e "${_outputs}/package-boot.tar.gz" ; then
	rm "${_outputs}/package-boot.tar.gz"
fi

mkdir "${_outputs}/package-boot"
mkdir "${_outputs}/package-boot/bin"

sed \
		-r -e 's|@\{_package_name\}|'"${_package_name}"'|g' \
	>"${_outputs}/package-boot/bin/run" <<'EOS'
#!/bin/bash

set -e -E -u -o pipefail || exit 1

_self_basename="$( basename -- "${0}" )"
_self_realpath="$( readlink -e -- "${0}" )"
cd "$( dirname -- "${_self_realpath}" )"
cd ..
_package="$( readlink -e -- . )"
cmp -s -- "${_package}/bin/run" "${_self_realpath}"

export PATH="$(
		find /opt -maxdepth 1 -exec test -d {} -a -d {}/bin \; -print \
		| sed -r -e 's|^.*$|&/bin|' \
		| tr '\n' ':'
):${PATH:-}"

export mosaic_node_fqdn="$( hostname -f | tr ' ' '\n' | head -n 1 || true )"
export mosaic_node_ip="$( hostname -i | tr ' ' '\n' | head -n 1 || true )"
export mosaic_temporary=/tmp/mosaic
export mosaic_log="${mosaic_temporary}/log.txt"

if test ! -e "${mosaic_temporary}" ; then
	mkdir "${mosaic_temporary}"
fi

exec </dev/null >/dev/null 2>|"${mosaic_log}" 1>&2

if test "${#}" -eq 0 ; then
	exec @{_package_name}--run-node
else
	exec @{_package_name}--run-node "${@}"
fi

exit 1
EOS

chmod +x -- "${_outputs}/package-boot/bin/run"

cat >"${_outputs}/package-boot/bin/upgrade" <<'EOS'
#!/bin/bash

set -e -E -u -o pipefail || exit 1

if test "${0}" != /tmp/mosaic-node-boot--upgrade ; then
	cp "${0}" /tmp/mosaic-node-boot--upgrade
	exec /tmp/mosaic-node-boot--upgrade
fi

tazpkg recharge mosaic-repository
tazpkg get-install mosaic-node --forced
tazpkg get-install mosaic-node-wui --forced
tazpkg get-install mosaic-components-rabbitmq --forced
tazpkg get-install mosaic-components-riak-kv --forced
tazpkg get-install mosaic-components-httpg --forced
tazpkg get-install mosaic-components-java-container --forced
tazpkg get-install mosaic-components-java-drivers --forced
tazpkg get-install mosaic-components-java-cloudlet-container --forced
tazpkg get-install mosaic-examples-realtime-feeds --forced
tazpkg get-install mosaic-examples-realtime-feeds-java --forced
tazpkg get-install mosaic-node-boot --forced

exit 0
EOS

chmod +x -- "${_outputs}/package-boot/bin/upgrade"

cat >"${_outputs}/package-boot/pkg.json" <<EOS
{
	"package" : "${_package_name}-boot",
	"version" : "${_package_version}.$( date '+%Y%m%d.%H%M%S' )",
	"maintainer" : "mosaic-developers@lists.info.uvt.ro",
	"description" : "mOSAIC Component: ${_package_name}-boot",
	"directories" : [ "bin" ],
	"depends" : [
		"bash",
		"coreutils",
		"coreutils-character",
		"coreutils-command",
		"coreutils-conditions",
		"coreutils-context-system",
		"coreutils-context-user",
		"coreutils-context-working",
		"coreutils-directory",
		"coreutils-disk",
		"coreutils-file-attributes",
		"coreutils-file-format",
		"coreutils-file-output-full",
		"coreutils-file-output-part",
		"coreutils-file-sort",
		"coreutils-file-special",
		"coreutils-file-summarize",
		"coreutils-line",
		"coreutils-numeric",
		"coreutils-operations",
		"coreutils-path",
		"coreutils-print",
		"coreutils-redirection",
		"socat",
		"mosaic-node",
		"mosaic-node-wui",
		"mosaic-components-rabbitmq",
		"mosaic-components-riak-kv",
		"mosaic-components-httpg",
		"mosaic-components-java-container",
		"mosaic-components-java-drivers",
		"mosaic-components-java-cloudlet-container",
		"mosaic-examples-realtime-feeds",
		"mosaic-examples-realtime-feeds-java"
	]
}
EOS

tar -czf "${_outputs}/package-boot.tar.gz" -C "${_outputs}/package-boot" .

exit 0

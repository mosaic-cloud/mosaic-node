
-module (mosaic_console_wm).

-export ([init/1, allowed_methods/2, content_types_provided/2, handle_as_html/2, ping/2]).


-dispatch ({[], defaults}).
-dispatch ({["console"], defaults}).


-define (content,
<<"<!DOCTYPE html>
<html id=\"jsconsole\">
	<head>
		<title>", (mosaic_webmachine:format_atom (erlang:node ())) / binary, "</title>
		<meta id=\"meta\" name=\"viewport\" content=\"width=device-width; height=device-height; user-scalable=no; initial-scale=1.0\" />
		<link rel=\"stylesheet\" href=\"http://jsconsole.com/console.css\" type=\"text/css\" />
	</head>
	<body>
		<div>
			<form><textarea id=\"exec\" autofocus=\"true\" autocapitalize=\"off\" rows=\"1\" spellcheck=\"false\"></textarea></form>
			<div id=\"console\"><ul id=\"output\"></ul></div>
		</div>
		<script src=\"http://data.volution.ro/ciprian/41503a0aa1c2167edc98b3dba50d1d52/jquery.js\"></script>
		<script src=\"http://data.volution.ro/ciprian/41503a0aa1c2167edc98b3dba50d1d52/json2.js\"></script>
		<script src=\"http://data.volution.ro/ciprian/41503a0aa1c2167edc98b3dba50d1d52/jsconsole-prettify.js\"></script>
		<script src=\"http://data.volution.ro/ciprian/41503a0aa1c2167edc98b3dba50d1d52/jsconsole-console.js\"></script>
		<script>
			
			var _info = this.info
			
			function _get (_url, _query) {
				var _settings = {};
				_settings.url = _url;
				if (_query != undefined)
					_settings.data = _query;
				return (_execute_ajax (_settings));
			}
			
			function _execute_ajax (_settings) {
				_settings.async = false;
				var _ajax = $.ajax (_settings);
				return (_handle_ajax (_ajax));
			}
			
			function _handle_ajax (_ajax) {
				var _status = _ajax.status;
				var _content_type = _ajax.getResponseHeader (\"Content-Type\");
				var _content_data = _ajax.responseText;
				if (_content_type == \"application/json\")
					_content_data = $.parseJSON (_content_data);
				if ((_status != 200) || (_content_type != \"application/json\")) {
					if ((_content_type == \"application/json\") && (_content_data.error != undefined))
						_info (\"error: \" + _content_data.error);
					else
						_info (_ajax.responseText);
					return (undefined);
				}
				if (_content_data.error != undefined)
					_info (\"error: \" + _content_data.error);
				return (_content_data);
			}
			
			function _wrapped (_function) {
				var _lambda = function () {
					try {
						return (_function.apply (this, arguments));
					} catch (_exception) {
						return (undefined);
					}
				};
				_lambda.toString = function () { return (\"proxy\"); };
				return (_lambda);
			}
			
			var _mosaic = _wrapped (function () {});
			
			_mosaic.cluster = _wrapped (function () {});
			_mosaic.cluster.nodes = _wrapped (function () {
				return (_get (\"/cluster/nodes\") .nodes);
			});
			_mosaic.cluster.nodes.self = _wrapped (function () {
				return (_get (\"/cluster/nodes\") .self);
			});
			_mosaic.cluster.nodes.self.peers = _wrapped (function () {
				return (_get (\"/cluster/nodes\") .peers);
			});
			_mosaic.cluster.nodes.self.activate = _wrapped (function () {
				return (_get (\"/cluster/nodes/self/activate\") .ok);
			});
			_mosaic.cluster.nodes.self.deactivate = _wrapped (function () {
				return (_get (\"/cluster/nodes/self/deactivate\") .ok);
			});
			_mosaic.cluster.ring = _wrapped (function () {
				var _outcome = _get (\"/cluster/ring\");
				return ({nodes : _outcome.nodes, partitions : _outcome.partitions});
			});
			_mosaic.cluster.ring.nodes = _wrapped (function () {
				return (_get (\"/cluster/ring\") .nodes);
			});
			_mosaic.cluster.ring.nodes.include = _wrapped (function (_node) {
				return (_get (\"/cluster/ring/include\", {node : _node}) .ok);
			});
			_mosaic.cluster.ring.nodes.exclude = _wrapped (function (_node) {
				if (_node == undefined)
					_node = _get (\"/cluster/nodes\") .self;
				return (_get (\"/cluster/ring/exclude\", {node : _node}) .ok);
			});
			_mosaic.cluster.ring.partitions = _wrapped (function () {
				return (_get (\"/cluster/ring\") .partitions);
			});
			_mosaic.cluster.ring.reboot = _wrapped (function () {
				return (_get (\"/cluster/ring/reboot\") .ok);
			});
			
			_mosaic.executor = _wrapped (function () {});
			_mosaic.executor.nodes = _wrapped (function () {
				return (_get (\"/executor/nodes\") .nodes);
			});
			_mosaic.executor.nodes.self = function () {
				return (_get (\"/cluster/nodes\") .self);
			};
			_mosaic.executor.nodes.self.activate = _wrapped (function () {
				return (_get (\"/executor/nodes/self/activate\") .ok);
			});
			_mosaic.executor.nodes.self.deactivate = _wrapped (function () {
				return (_get (\"/executor/nodes/self/deactivate\") .ok);
			});
			_mosaic.executor.ping = _wrapped (function (_count) {
				if (_count == undefined)
					_count = 4;
				var _outcome = _get (\"/executor/ping\", {count : _count});
				return ({pongs : _outcome.pongs, pangs : _outcome.pangs});
			});
			_mosaic.executor.processes = _wrapped (function () {
				return (_get (\"/executor/processes\") .keys);
			});
			_mosaic.executor.processes.create = _wrapped (function (_type, _arguments, _count) {
				if (_arguments == undefined)
					_arguments = null;
				if (_count == undefined)
					_count = 1;
				_arguments = JSON.stringify (_arguments);
				var _outcome = _get (\"/executor/processes/create\", {type : _type, arguments : _arguments, count : _count});
				if (_count == 1)
					return (_outcome.keys[0]);
				else
					return (_outcome.keys);
			});
			_mosaic.executor.processes.stop = _wrapped (function (_key) {
				return (_get (\"/executor/processes/stop\", {key : _key}) .ok);
			});
			_mosaic.executor.processes.call = _wrapped (function (_key, _arguments) {
				if (_arguments == undefined)
					_arguments = null;
				_arguments = JSON.stringify (_arguments);
				return (_get (\"/executor/processes/call\", {key : _key, arguments : _arguments}));
			});
			_mosaic.executor.processes.cast = _wrapped (function (_key, _arguments) {
				if (_arguments == undefined)
					_arguments = null;
				_arguments = JSON.stringify (_arguments);
				return (_get (\"/executor/processes/cast\", {key : _key, arguments : _arguments}));
			});
			
			$(\"#sandbox\")[0].contentWindow.mosaic = _mosaic;
			
		</script>
	</body>
</html>">>).


init (defaults) ->
	{ok, void}.

ping(Request, State = void) ->
    {pong, Request, State}.

allowed_methods (Request, State = void) ->
	{['GET'], Request, State}.

content_types_provided (Request, State = void) ->
	{[{"text/html", handle_as_html}], Request, State}.

handle_as_html (Request, State = void) ->
	{?content, Request, State}.

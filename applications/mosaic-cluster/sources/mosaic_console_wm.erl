
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
		<form><textarea autofocus id=\"exec\" spellcheck=\"false\" autocapitalize=\"off\" autofocus rows=\"1\"></textarea></form>
		<div id=\"console\"><ul id=\"output\"></ul></div>
		<script src=\"http://code.jquery.com/jquery-1.5.2.min.js\"></script>
		<script src=\"https://github.com/douglascrockford/JSON-js/raw/8e0b15cb492f63067a88ad786e4d5fc0fa89a241/json2.js\"></script>
		<script src=\"http://jsconsole.com/prettify.js\"></script>
		<script src=\"http://jsconsole.com/console.js\"></script>
		<script>$(\"ul#output>li\").remove();</script>
		<script>
			
			$info = this.info
			
			function $get (_url, _query) {
				_settings = {};
				_settings.url = _url;
				if (_query != undefined)
					_settings.data = _query;
				return (_execute_ajax (_settings));
			}
			
			function _execute_ajax (_settings) {
				_settings.async = false;
				_ajax = $.ajax (_settings);
				return (_handle_ajax (_ajax));
			}
			
			function _handle_ajax (_ajax) {
				_status = _ajax.status;
				_content_type = _ajax.getResponseHeader (\"Content-Type\");
				_content_data = _ajax.responseText;
				if (_content_type == \"application/json\")
					_content_data = $.parseJSON (_content_data);
				if ((_status != 200) || (_content_type != \"application/json\")) {
					if ((_content_type == \"application/json\") && (_content_data.error != undefined))
						$info (\"error: \" + _content_data.error);
					else
						$info (_ajax.responseText);
					return (undefined);
				}
				if (_content_data.error != undefined)
					$info (\"error: \" + _content_data.error);
				return (_content_data);
			}
			
			function _wrapped_0 (_function) {
				return (function () {
					try {
						return (_function ());
					} catch (_exception) {
						return (undefined);
					}
				});
			}
			
			function _wrapped_1 (_function) {
				return (function (_argument_1) {
					try {
						return (_function (_argument_1));
					} catch (_exception) {
						return (undefined);
					}
				});
			}
			
			function _wrapped_2 (_function) {
				return (function (_argument_1, _argument_2) {
					try {
						return (_function (_argument_1, _argument_2));
					} catch (_exception) {
						return (undefined);
					}
				});
			}
			
			var $mosaic = function () {};
			
			$mosaic.cluster = function () {};
			$mosaic.cluster.nodes = _wrapped_0 (function () {
				_outcome = $get (\"/cluster/nodes\");
				_nodes = _outcome.nodes;
				_nodes.push (_outcome.self);
				return (_nodes);
			});
			$mosaic.cluster.nodes.self = _wrapped_0 (function () {
				return ($get (\"/cluster/nodes\") .self);
			});
			$mosaic.cluster.nodes.self.peers = _wrapped_0 (function () {
				return ($get (\"/cluster/nodes\") .peers);
			});
			$mosaic.cluster.nodes.self.activate = _wrapped_0 (function () {
				return ($get (\"/cluster/nodes/self/activate\") .ok);
			});
			$mosaic.cluster.nodes.self.deactivate = _wrapped_0 (function () {
				return ($get (\"/cluster/nodes/self/deactivate\") .ok);
			});
			$mosaic.cluster.ring = _wrapped_0 (function () {
				_outcome = $get (\"/cluster/ring\");
				return ({nodes : _outcome.nodes, partitions : _outcome.partitions});
			});
			$mosaic.cluster.ring.nodes = _wrapped_0 (function () {
				return ($get (\"/cluster/ring\") .nodes);
			});
			$mosaic.cluster.ring.nodes.include = _wrapped_1 (function (_node) {
				return ($get (\"/cluster/ring/include\", {node : _node}) .ok);
			});
			$mosaic.cluster.ring.nodes.exclude = _wrapped_1 (function (_node) {
				if (_node == undefined)
					_node = $get (\"/cluster/nodes\") .self;
				return ($get (\"/cluster/ring/exclude\", {node : _node}) .ok);
			});
			$mosaic.cluster.ring.partitions = _wrapped_0 (function () {
				return ($get (\"/cluster/ring\") .partitions);
			});
			$mosaic.cluster.ring.reboot = _wrapped_0 (function () {
				return ($get (\"/cluster/ring/reboot\") .ok);
			});
			
			$mosaic.executor = function () {};
			$mosaic.executor.nodes = _wrapped_0 (function () {
				return ($get (\"/executor/nodes\") .nodes);
			});
			$mosaic.executor.nodes.self = function () {};
			$mosaic.executor.nodes.self.activate = _wrapped_0 (function () {
				return ($get (\"/executor/nodes/self/activate\") .ok);
			});
			$mosaic.executor.nodes.self.deactivate = _wrapped_0 (function () {
				return ($get (\"/executor/nodes/self/deactivate\") .ok);
			});
			$mosaic.executor.ping = _wrapped_1 (function (_count) {
				if (_count == undefined)
					_count = 4;
				_outcome = $get (\"/executor/ping\", {count : _count});
				return ({pongs : _outcome.pongs, pangs : _outcome.pangs});
			});
			$mosaic.executor.processes = _wrapped_0 (function () {
				return ($get (\"/executor/processes\") .keys);
			});
			$mosaic.executor.processes.create = _wrapped_2 (function (_type, _arguments) {
				_arguments = JSON.stringify (_arguments);
				return ($get (\"/executor/processes/create\", {type : _type, arguments : _arguments}) .key);
			});
			$mosaic.executor.processes.stop = _wrapped_1 (function (_key) {
				return ($get (\"/executor/processes/stop\", {key : _key}) .ok);
			});
			
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

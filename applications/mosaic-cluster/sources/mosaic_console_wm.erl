
-module (mosaic_console_wm).

-export ([init/1, allowed_methods/2, content_types_provided/2, handle_as_html/2, ping/2]).


-dispatch ({["console"], defaults}).


-define (content,
<<"<!DOCTYPE html>
<html id=\"jsconsole\">
	<head>
		<title>jsconsole</title>
		<meta id=\"meta\" name=\"viewport\" content=\"width=device-width; height=device-height; user-scalable=no; initial-scale=1.0\" />
		<link rel=\"stylesheet\" href=\"http://jsconsole.com/console.css\" type=\"text/css\" />
	</head>
	<body>
		<form><textarea autofocus id=\"exec\" spellcheck=\"false\" autocapitalize=\"off\" autofocus rows=\"1\"></textarea></form>
		<div id=\"console\"><ul id=\"output\"></ul></div>
		<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/1.5.2/jquery.js\"></script>
		<script src=\"http://jsconsole.com/prettify.js\"></script>
		<script src=\"http://jsconsole.com/console.js\"></script>
		<script>$(\"ul#output>li\").remove();</script>
		<script>
			
			function $get (_url) {
				return (_execute_ajax ({url : _url}));
			}
			
			function _execute_ajax (_settings) {
				try {
					_settings.async = false;
					_ajax = $.ajax (_settings);
					return (_handle_ajax (_ajax));
				} catch (exception) {
					return (exception);
				}
			}
			
			function _handle_ajax (_ajax) {
				_status = _ajax.status;
				_content_type = _ajax.getResponseHeader (\"Content-Type\");
				_content_data = _ajax.responseText;
				if (_content_type == \"application/json\")
					_content_data = $.parseJSON (_content_data);
				if ((_status != 200) || (_content_type != \"application/json\"))
					throw ([_status, _content_type, _content_data]);
				return (_content_data);
			}
			
			var $mosaic = {
				cluster : {
					nodes : {
						get : function () {
							return ($get (\"cluster/nodes\"));
						},
					},
					ring : {
						get : function () {
							return ($get (\"cluster/ring\"));
						},
						include : function (_node) {
							return ($get (\"cluster/ring/include/\" + _node));
						},
						exclude : function (_node) {
							return ($get (\"cluster/ring/exclude/\" + _node));
						},
					},
				},
			}
			
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


var backend = require ("./mosaic_component_backend.js");

_component = backend.initialize ();

_component.on ("call", function (_operation, _correlation, _inputs, _data) {
	console.error ("[%d][ii] call `%s`/`%s` `%j`...", process.pid, _operation, _correlation, _inputs);
	if (_data == "")
		if (_operation == "+") {
			var _outcome = _inputs[0] + _inputs[1];
			this.call_return (_correlation, true, _outcome, "");
		} else
			this.call_return (_correlation, false, "invalid operator", "");
	else
		this.call_return (_correlation, false, "unexpected data", "");
});

_component.on ("cast", function (_operation, _inputs, _data) {
	console.error ("[%d][ii] cast `%s` `%j`...", process.pid, _operation, _inputs);
});

_component.on ("call-return", function (_correlation, _ok, _outputs_or_error, _data) {
	console.error ("[%d][ii] call-return `%s` `%s` `%j`...", process.pid, _correlation, _ok, _outputs_or_error);
});

_component.on ("exchange", function (_metaData, _data) {
	console.error ("[%d][ii] echo `%j`...", process.pid, _metaData);
	_component.exchange (_metaData, _data);
});

_component.on ("terminate", function () {
	console.error ("[%d][ii] terminating...", process.pid);
});

_component.on ("error", function (_error) {
	console.error ("[%d][ee] failed: %s...%s", process.pid, _error, _error.stack)
	_component.terminate ()
});

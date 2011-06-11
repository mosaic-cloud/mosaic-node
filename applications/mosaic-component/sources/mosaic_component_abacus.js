
var backend = require ("./mosaic_component_backend.js");

_component = backend.initialize ();

_component.on ("call", function (_correlation, _metaData, _data) {
	console.error ("[%d][ii] call `%j` `%s` `%s`...", process.pid, _metaData, _correlation, _data);
	if (_metaData["operator"] == "+") {
		var _outcome = _metaData["operands"][0] + _metaData["operands"][1];
		this.return (_correlation, {ok : true, outcome : _outcome}, _data);
	}
});

_component.on ("cast", function (_metaData, _data) {
	console.error ("[%d][ii] call `%j` `%s`...", process.pid, _metaData, _data);
});

_component.on ("return", function (_correlation, _metaData, _data) {
	console.error ("[%d][ii] call `%j` `%s` `%s`...", process.pid, _metaData, _correlation, _data);
});

_component.on ("exchange", function (_metaData, _data) {
	console.error ("[%d][ii] exchange `%j` `%s`...", process.pid, _metaData, _data);
	_component.exchange (_metaData, _data);
});

_component.on ("terminate", function () {
	console.error ("[%d][ii] terminating...", process.pid);
});

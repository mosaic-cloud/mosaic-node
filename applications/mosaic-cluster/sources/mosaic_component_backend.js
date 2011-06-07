
// ---------------------------------------

var events = require ("events");
var sys = require ("sys");

// ---------------------------------------

var Component = function (_harness) {
	events.EventEmitter.call (this);
	this._harness = _harness;
	this._harness.on ("input", _boundedMethod (this, this._onInput));
	this._harness.on ("terminate", _boundedMethod (this, this._onTerminate));
	this._harness.on ("error", _boundedMethod (this, this._onError));
};

sys.inherits (Component, events.EventEmitter);

Component.prototype.call = function (_component, _correlation, _metaData, _data) {
	if ((typeof (_component) == "string") && (typeof (_correlation) == "string") && (typeof (_metaData) == "object") && (_data instanceof Buffer)) {
		var _exchangeMetaData = {
			"action" : "call",
			"component" : _component,
			"correlation" : _correlation,
			"meta-data" : _metaData
		};
		this.exchange (_exchangeMetaData, _data);
	}
};

Component.prototype.cast = function (_component, _metaData, _data) {
	if ((typeof (_component) == "string") && (typeof (_metaData) == "object") && (_data instanceof Buffer)) {
		var _exchangeMetaData = {
			"action" : "cast",
			"component" : _component,
			"meta-data" : _metaData
		};
		this.exchange (_exchangeMetaData, _data);
	}
};

Component.prototype.return = function (_correlation, _metaData, _data) {
	if ((typeof (_correlation) == "string") && (typeof (_metaData) == "object") && (_data instanceof Buffer)) {
		var _exchangeMetaData = {
			"action" : "return",
			"correlation" : _correlation,
			"meta-data" : _metaData
		};
		this.exchange (_exchangeMetaData, _data);
	}
};

Component.prototype.exchange = function (_metaData, _data) {
	if (typeof (_metaData) == "object") {
		_metaData["__type__"] = "exchange";
		this._harness.output (_metaData, _data);
	} else
		this.emit ("error", new Error ("invalid meta-data value type `" + typeof (_metaData) + "`"));
};

Component.prototype._onInput = function (_metaData, _data) {
	if (typeof (_metaData) == "object") {
		var _type = _metaData["__type__"];
		if (_type == "exchange") {
			delete _metaData["__type__"];
			var _action = _metaData["action"];
			if (_action == "call") {
				var _correlation = _metaData["correlation"];
				var _callMetaData = _metaData["meta-data"];
				if ((typeof (_correlation) == "string") && (typeof (_callMetaData) == "object"))
					this.emit ("call", _correlation, _callMetaData, _data);
				else
					this.emit ("error", new Error ("invalid call meta-data `" + _metaData + "`"));
			} else if (_action == "cast") {
				var _castMetaData = _metaData["meta-data"];
				if (typeof (_castMetaData) == "object")
					this.emit ("cast", _castMetaData, _data);
				else
					this.emit ("error", new Error ("invalid cast meta-data `" + _metaData + "`"));
			} else if (_action == "return") {
				var _correlation = _metaData["correlation"];
				var _returnMetaData = _metaData["meta-data"];
				if ((typeof (_correlation) == "string") && (typeof (_returnMetaData) == "object"))
					this.emit ("return", _correlation, _returnMetaData, _data);
				else
					this.emit ("error", new Error ("invalid return meta-data `" + _metaData + "`"));
			} else
				this.emit ("exchange", _metaData, _data);
		} else
			this.emit ("error", new Error ("invalid meta-data type `" + _type + "`"));
	} else
		this.emit ("error", new Error ("invalid meta-data value type `" + typeof (_metaData) + "`"));
};

Component.prototype._onTerminate = function () {
	this.emit ("terminate");
};

Component.prototype._onError = function (_error) {
	this.emit ("error", _error);
};

// ---------------------------------------

var Harness = function (_inputStream, _outputStream) {
	events.EventEmitter.call (this);
	this._inputer = new PacketInputer (_inputStream);
	this._inputer.on ("input", _boundedMethod (this, this._onInboundPacket));
	this._inputer.on ("close", _boundedMethod (this, this._onInboundClose));
	this._inputer.on ("error", _boundedMethod (this, this._onError));
	this._outputer = new PacketOutputer (_outputStream);
	this._outputer.on ("close", _boundedMethod (this, this._onOutboundClose));
	this._outputer.on ("error", _boundedMethod (this, this._onError));
	this._terminateEmitted = false;
};

sys.inherits (Harness, events.EventEmitter);

Harness.prototype.output = function (_metaData, _data) {
	this._outputer.output (_metaData, _data)
};

Harness.prototype._onInboundPacket = function (_metaData, _data) {
	this.emit ("input", _metaData, _data);
};

Harness.prototype._onInboundClose = function () {
	this._maybe_terminate (false);
};

Harness.prototype._onOutboundClose = function () {
	this._maybe_terminate (false);
};

Harness.prototype._onError = function (_error) {
	this.emit ("error", _error);
};

Harness.prototype._onExit = function () {
	this._terminate_maybe ();
};

Harness.prototype._maybe_terminate = function (_inline) {
	if (!this._terminateEmitted) {
		this._terminateEmitted = true;
		if (_inline) {
			var _listeners = this.listeners ("terminate");
			for (var _index in _listeners)
				try {
					_listeners[_index] ();
				} catch (_error) {}
		} else
			this.emit ("terminate");
	}
};

Harness.prototype.terminate = function () {
	this._inputer.close ();
	this._outputer.close ();
};

// ---------------------------------------

var PacketInputer = function (_stream) {
	events.EventEmitter.call (this);
	this._stream = _stream;
	this._stream.on ("data", _boundedMethod (this, this._onStreamData));
	this._stream.on ("end", _boundedMethod (this, this._on_stream_end));
	this._stream.on ("close", _boundedMethod (this, this._on_stream_end));
	this._stream.on ("error", _boundedMethod (this, this._onStreamError));
	this._buffer = null;
	this._bufferOffset = null;
	this._bufferInitialSize = 1024;
	this._pendingSize = null;
	this.opened = true;
};

sys.inherits (PacketInputer, events.EventEmitter);

PacketInputer.prototype.close = function () {
	this._stream.destroy ();
};

PacketInputer.prototype._onStreamData = function (_data) {
	if (this._buffer == null) {
		this._buffer = new Buffer (Math.ceil (_data.length / this._bufferInitialSize) * this._bufferInitialSize);
		this._bufferOffset = 0;
	}
	if ((this._bufferOffset + _data.length) > this._buffer.length) {
		var _old_buffer = this._buffer;
		this._buffer = new Buffer (Math.ceil ((this._bufferOffset + _data.length) / this._bufferInitialSize) * this._bufferInitialSize * 2);
		_old_buffer.copy (this._buffer);
	}
	_data.copy (this._buffer, this._bufferOffset);
	this._bufferOffset += _data.length;
	if ((this._pendingSize == null) && (this._bufferOffset >= 4))
		this._pendingSize = (this._buffer[0] << 24) | (this._buffer[1] << 16) | (this._buffer[2] << 8) | (this._buffer[3] << 0);
	if ((this._pendingSize != null) && (this._bufferOffset >= (this._pendingSize + 4))) {
		var _packet = new Buffer (this._pendingSize);
		this._buffer.copy (_packet, 0, 4, this._pendingSize + 4);
		this._buffer.copy (this._buffer, 0, this._pendingSize + 4, this._bufferOffset);
		this._bufferOffset = 0;
		this._pendingSize = null;
		this._onPacket (_packet);
	}
};

PacketInputer.prototype._onPacket = function (_buffer) {
	for (var _split = 0; _split < _buffer.length; _split++)
		if (_buffer[_split] == 0)
			break;
	if (_split == _buffer.length)
		throw (new Error ());
	var _metaData = JSON.parse (_buffer.slice (0, _split));
	var _data = _buffer.slice (_split + 1);
	this.emit ("input", _metaData, _data);
};

PacketInputer.prototype._on_stream_end = function () {
	this._stream.destroy ();
	this.opened = false;
	this.emit ("close");
};

PacketInputer.prototype._onStreamError = function (_error) {
	this.opened = false;
	this.emit ("error", _error);
};

// ---------------------------------------

var PacketOutputer = function (_stream) {
	events.EventEmitter.call (this);
	this._stream = _stream;
	this._stream.on ("end", _boundedMethod (this, this._on_stream_end));
	this._stream.on ("close", _boundedMethod (this, this._on_stream_end));
	this._stream.on ("error", _boundedMethod (this, this._onStreamError));
	this.opened = true;
};

sys.inherits (PacketOutputer, events.EventEmitter);

PacketOutputer.prototype.output = function (_metaData, _data) {
	_metaData = JSON.stringify (_metaData);
	_metaData = new Buffer (_metaData);
	var _size = _metaData.length + 1 + _data.length;
	var _buffer = new Buffer (_size + 4);
	_buffer[0] = (_size >> 24) & 0xff;
	_buffer[1] = (_size >> 16) & 0xff;
	_buffer[2] = (_size >> 8) & 0xff;
	_buffer[3] = (_size >> 0) & 0xff;
	_metaData.copy (_buffer, 4, 0, _metaData.length);
	_buffer[_metaData.length + 4] = 0;
	_data.copy (_buffer, _metaData.length + 1 + 4, 0, _data.length);
	this._stream.write (_buffer);
};

PacketOutputer.prototype.close = function () {
	this._stream.destroy ();
};

PacketOutputer.prototype._on_stream_end = function () {
	this._stream.destroy ();
	this.opened = false;
	this.emit ("close");
};

PacketOutputer.prototype._onStreamError = function (_error) {
	this.opened = false;
	this.emit ("error", _error);
};

// ---------------------------------------

function _boundedMethod (_this, _function) {
	if (_this == undefined)
		throw (new Error ());
	if (_function == undefined)
		throw (new Error ());
	return (function () {
		return (_function.apply (_this, arguments));
	});
};

// ---------------------------------------

var _initialized = false;

function _initialize () {
	if (_initialized)
		throw (new Error ());
	var _harness = new Harness (process.stdin, process.stdout);
	var _component = new Component (_harness);
	process.on ("exit", function () {
		_harness._maybe_terminate (true);
	});
	process.stdin = undefined;
	process.stdout = undefined;
	console.log = console.warn;
	console.info = console.warn;
	_harness._inputer._stream.resume ();
	_initialized = true;
	return (_component);
}

module.exports.initialize = _initialize;

// ---------------------------------------

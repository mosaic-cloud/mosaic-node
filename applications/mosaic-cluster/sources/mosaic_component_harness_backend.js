
// ---------------------------------------

var events = require ("events");
var sys = require ("sys");

// ---------------------------------------

var Harness = function (_input_stream, _output_stream) {
	events.EventEmitter.call (this);
	this._inputer = new PacketInputer (_input_stream);
	this._inputer.on ("input", _bounded_method (this, this._on_inbound_packet));
	this._inputer.on ("close", _bounded_method (this, this._on_inbound_close));
	this._inputer.on ("error", _bounded_method (this, this._on_error));
	this._outputer = new PacketOutputer (_output_stream);
	this._outputer.on ("close", _bounded_method (this, this._on_outbound_close));
	this._outputer.on ("error", _bounded_method (this, this._on_error));
	this._emited_terminate = false;
};

sys.inherits (Harness, events.EventEmitter);

Harness.prototype.output = function (_meta_data, _data) {
	this._outputer.output (_meta_data, _data)
};

Harness.prototype._on_inbound_packet = function (_meta_data, _data) {
	this.emit ("input", _meta_data, _data);
};

Harness.prototype._on_inbound_close = function () {
	this._close ();
};

Harness.prototype._on_outbound_close = function () {
	this._close ();
};

Harness.prototype._close = function () {
	this.emit ("terminate");
};

Harness.prototype._on_error = function (_error) {
	this.emit ("error", _error);
};

Harness.prototype._on_exit = function () {
	var _listeners = this.listeners ("terminated");
	for (var _index in _listeners)
		try {
			_listeners[_index] ();
		} catch (_error) {}
};

Harness.prototype.terminate = function () {
	this._inputer.close ();
	this._outputer.close ();
};

// ---------------------------------------

var PacketInputer = function (_stream) {
	events.EventEmitter.call (this);
	this._stream = _stream;
	this._stream.on ("data", _bounded_method (this, this._on_stream_data));
	this._stream.on ("end", _bounded_method (this, this._on_stream_end));
	this._stream.on ("close", _bounded_method (this, this._on_stream_end));
	this._stream.on ("error", _bounded_method (this, this._on_stream_error));
	this._buffer = null;
	this._buffer_offset = null;
	this._buffer_initial_size = 1024;
	this._pending_size = null;
	this.opened = true;
};

sys.inherits (PacketInputer, events.EventEmitter);

PacketInputer.prototype.close = function () {
	this._stream.destroy ();
};

PacketInputer.prototype._on_stream_data = function (_data) {
	if (this._buffer == null) {
		this._buffer = new Buffer (Math.ceil (_data.length / this._buffer_initial_size) * this._buffer_initial_size);
		this._buffer_offset = 0;
	}
	if ((this._buffer_offset + _data.length) > this._buffer.length) {
		var _old_buffer = this._buffer;
		this._buffer = new Buffer (Math.ceil ((this._buffer_offset + _data.length) / this._buffer_initial_size) * this._buffer_initial_size * 2);
		_old_buffer.copy (this._buffer);
	}
	_data.copy (this._buffer, this._buffer_offset);
	this._buffer_offset += _data.length;
	if ((this._pending_size == null) && (this._buffer_offset >= 4))
		this._pending_size = (this._buffer[0] << 24) | (this._buffer[1] << 16) | (this._buffer[2] << 8) | (this._buffer[3] << 0);
	if ((this._pending_size != null) && (this._buffer_offset >= (this._pending_size + 4))) {
		var _packet = new Buffer (this._pending_size);
		this._buffer.copy (_packet, 0, 4, this._pending_size + 4);
		this._buffer.copy (this._buffer, 0, this._pending_size + 4, this._buffer_offset);
		this._buffer_offset = 0;
		this._pending_size = null;
		this._on_packet (_packet);
	}
};

PacketInputer.prototype._on_packet = function (_buffer) {
	for (var _split = 0; _split < _buffer.length; _split++)
		if (_buffer[_split] == 0)
			break;
	if (_split == _buffer.length)
		throw (new Error ());
	var _meta_data = JSON.parse (_buffer.slice (0, _split));
	var _data = _buffer.slice (_split + 1);
	this.emit ("input", _meta_data, _data);
};

PacketInputer.prototype._on_stream_end = function () {
	this._stream.destroy ();
	this.opened = false;
	this.emit ("close");
};

PacketInputer.prototype._on_stream_error = function (_error) {
	this.opened = false;
	this.emit ("error", _error);
};

// ---------------------------------------

var PacketOutputer = function (_stream) {
	events.EventEmitter.call (this);
	this._stream = _stream;
	// this._stream.on ("drain", _bounded_method (this, this._on_stream_drain));
	this._stream.on ("end", _bounded_method (this, this._on_stream_end));
	this._stream.on ("close", _bounded_method (this, this._on_stream_end));
	this._stream.on ("error", _bounded_method (this, this._on_stream_error));
	this.opened = true;
};

sys.inherits (PacketOutputer, events.EventEmitter);

PacketOutputer.prototype.output = function (_meta_data, _data) {
	_meta_data = JSON.stringify (_meta_data);
	_meta_data = new Buffer (_meta_data);
	_size = _meta_data.length + 1 + _data.length;
	_buffer = new Buffer (_size + 4);
	_buffer[0] = (_size >> 24) & 0xff;
	_buffer[1] = (_size >> 16) & 0xff;
	_buffer[2] = (_size >> 8) & 0xff;
	_buffer[3] = (_size >> 0) & 0xff;
	_meta_data.copy (_buffer, 4, 0, _meta_data.length);
	_buffer[_meta_data.length + 4] = 0;
	_data.copy (_buffer, _meta_data.length + 1 + 4, 0, _data.length);
	this._stream.write (_buffer);
};

PacketOutputer.prototype.close = function () {
	this._stream.destroy ();
};

// PacketOutputer.prototype._on_stream_drain = function () {};

PacketOutputer.prototype._on_stream_end = function () {
	this._stream.destroy ();
	this.opened = false;
	this.emit ("close");
};

PacketOutputer.prototype._on_stream_error = function (_error) {
	this.opened = false;
	this.emit ("error", _error);
};

// ---------------------------------------

function _bounded_method (_this, _function) {
	if (_this === undefined)
		throw (new Error ());
	if (_function === undefined)
		throw (new Error ());
	return (function () {
		return (_function.apply (_this, arguments));
	});
};

// ---------------------------------------

var _harness = new Harness (process.stdin, process.stdout);
_harness.on ("input", function (_meta_data, _data) {
	console.error ("[%d][ii] received `%j` `%s`...", process.pid, _meta_data, _data);
	_harness.output (_meta_data, _data);
});

_harness.on ("terminate", function () {
	console.error ("[%d][ii] terminating...", process.pid);
});

process.stdin.resume ();

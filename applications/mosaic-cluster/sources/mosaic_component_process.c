

#include <jansson.h>


enum _exit_code {
	_exit_code_succeeded = 0,
	_exit_code_invalid_arguments,
	_exit_code_invalid_action,
	_exit_code_invalid_json,
	_exit_code_eio,
	_exit_code_enomem,
	_exit_code_fatal,
};

struct _buffer {
	unsigned int capacity;
	unsigned int offset;
	unsigned int available;
	unsigned char * data;
};

enum _packet_state {
	_packet_state_undefined = 0,
	_packet_state_input_ready,
	_packet_state_input_waiting_size,
	_packet_state_input_waiting_data,
	_packet_state_inputed,
	_packet_state_parsed,
	_packet_state_output_ready,
	_packet_state_output_waiting,
	_packet_state_outputed,
	_packet_state_stream_ended,
	_packet_state_stream_error,
};

enum _packet_action {
	_packet_action_undefined = 0,
	_packet_action_exchange,
	_packet_action_exec,
	_packet_action_kill,
};

struct _packet {
	struct _packet * chained;
	enum _packet_state state;
	unsigned int size;
	json_t * json;
	enum _packet_action action;
	struct _buffer * buffer;
};

struct _packet_stream {
	unsigned int descriptor;
	unsigned int poll;
	struct _packet * head;
	struct _packet * tail;
	struct _packet * pending;
};

struct _process_execution {
	unsigned int identifier;
	unsigned int input_descriptor;
	unsigned int output_descriptor;
};


static void _packet_router (struct _packet_stream * const _streams[4]);
static void _packet_parse (struct _packet * const _packet);
static void _packet_parse_json (struct _packet * const _packet);
static void _packet_parse_action (struct _packet * const _packet);
static void _packet_streams_poll (struct _packet_stream * const * const _streams, unsigned int count);
static void _packet_stream_input (struct _packet_stream * _stream);
static void _packet_stream_output (struct _packet_stream * _stream);
static void _packet_stream_enqueue (struct _packet_stream * _stream, struct _packet * * const _packet);
static void _packet_stream_dequeue (struct _packet_stream * _stream, struct _packet * * const _packet);
static void _packet_stream_initialize (struct _packet_stream * _stream, unsigned int const descriptor);
static void _packet_input (struct _packet * const _packet, unsigned int _stream);
static void _packet_output (struct _packet * const _packet, unsigned int _stream);
static void _packet_allocate (struct _packet * * const _packet);
static void _packet_deallocate (struct _packet * * const _packet);
static void _buffer_parse (json_t * * const _json, struct _buffer * const _buffer);
static void _buffer_allocate (struct _buffer * * const _buffer, unsigned int _capacity);
static void _buffer_reallocate (struct _buffer * * const _buffer, unsigned int _capacity);
static void _buffer_deallocate (struct _buffer * * const _buffer);
static void _finish (enum _exit_code const _code);
static void _fail (unsigned char const * const _reason, enum _exit_code const _code);


#define __paste___(__value__) #__value__
#define __paste__(__value__) __paste___(__value__)
#define _assert(_condition) do { if (!(_condition)) _fail ("assertion failed ( " __FILE__ " : " __paste__(__LINE__) " ) `" #_condition "`", _exit_code_fatal); } while (0)


unsigned int main (
		unsigned int const _argument_count,
		unsigned char const * const * const _argument_values)
{
	struct _packet_stream _controller_input_stream;
	struct _packet_stream _controller_output_stream;
	struct _packet_stream _component_input_stream;
	struct _packet_stream _component_output_stream;
	struct _packet_stream * _streams[] = {
			&_controller_input_stream, &_controller_output_stream,
			&_component_input_stream, &_component_output_stream};
	signed int _pipe_descriptors[2];
	signed int _pipe_outcome;
	
	if (_argument_count != 1)
		_fail ("invalid arguments", _exit_code_invalid_arguments);
	
	_pipe_outcome = pipe (_pipe_descriptors);
	if (_pipe_outcome != 0)
		_fail ("pipe creation failed", _exit_code_eio);
	
	_packet_stream_initialize (&_controller_input_stream, 0);
	_packet_stream_initialize (&_controller_output_stream, 1);
	_packet_stream_initialize (&_component_input_stream, _pipe_descriptors[0]);
	_packet_stream_initialize (&_component_output_stream, _pipe_descriptors[1]);
	
	while (1)
		_packet_router (_streams);
	
	_finish (0);
}


#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <jansson.h>


static void _packet_router (struct _packet_stream * const _streams[4]) {
	
	struct _packet_stream * const _controller_input_stream = _streams[0];
	struct _packet_stream * const _controller_output_stream = _streams[1];
	struct _packet_stream * const _component_input_stream = _streams[2];
	struct _packet_stream * const _component_output_stream = _streams[3];
	
	unsigned int _should_finish = 0;
	
	if ((_controller_input_stream->pending != 0) || (_controller_input_stream->head == 0))
		_controller_input_stream->poll = POLLIN;
	else
		_controller_input_stream->poll = 0;
	if ((_component_input_stream->pending != 0) || (_component_input_stream->head == 0))
		_component_input_stream->poll = POLLIN;
	else
		_component_input_stream->poll = 0;
	
	if (_controller_input_stream->head != 0) {
		struct _packet * _packet = 0;
		_packet_stream_dequeue (_controller_input_stream, &_packet);
		switch (_packet->state) {
			case _packet_state_inputed :
				_packet_parse (_packet);
				_packet->state = _packet_state_output_ready;
				switch (_packet->action) {
					case _packet_action_exchange :
						_packet_stream_enqueue (_component_output_stream, &_packet);
						break;
					default :
						_assert (_packet->action & 0);
						break;
				}
				break;
			case _packet_state_stream_ended :
				_should_finish += 1;
				break;
			case _packet_state_stream_error :
				_fail ("controller input stream broken", _exit_code_eio);
				break;
			default :
				_assert (_packet->state & 0);
				break;
		}
	}
	
	if (_component_input_stream->head != 0) {
		struct _packet * _packet = 0;
		_packet_stream_dequeue (_component_input_stream, &_packet);
		switch (_packet->state) {
			case _packet_state_inputed :
				_packet_parse (_packet);
				_packet->state = _packet_state_output_ready;
				switch (_packet->action) {
					case _packet_action_exchange :
						_packet_stream_enqueue (_controller_output_stream, &_packet);
						break;
					default :
						_assert (_packet->action & 0);
						break;
				}
				break;
			case _packet_state_stream_ended :
				_should_finish += 1;
				break;
			case _packet_state_stream_error :
				_fail ("controller input stream broken", _exit_code_eio);
				break;
			default :
				_assert (_packet->state & 0);
				break;
		}
	}
	
	if ((_controller_output_stream->pending != 0) || (_controller_output_stream->head != 0))
		_controller_output_stream->poll = POLLOUT;
	else
		_controller_output_stream->poll = 0;
	if ((_component_output_stream->pending != 0) || (_component_output_stream->head != 0))
		_component_output_stream->poll = POLLOUT;
	else
		_component_output_stream->poll = 0;
	
	_packet_streams_poll (_streams, 4);
	
	if (_controller_input_stream->poll)
		_packet_stream_input (_controller_input_stream);
	if (_controller_output_stream->poll)
		_packet_stream_output (_controller_output_stream);
	if (_component_input_stream->poll)
		_packet_stream_input (_component_input_stream);
	if (_component_output_stream->poll)
		_packet_stream_output (_component_output_stream);
	
	if ((_controller_output_stream->pending != 0) && (_controller_output_stream->pending->state == _packet_state_stream_error))
		_fail ("controller output stream broken", _exit_code_eio);
	if ((_component_output_stream->pending != 0) && (_component_output_stream->pending->state == _packet_state_stream_error))
		_fail ("component output stream broken", _exit_code_eio);
	
	if ((_should_finish > 0) && !(
			(_controller_input_stream->pending != 0) || (_controller_input_stream->head != 0) ||
			(_controller_output_stream->pending != 0) || (_controller_output_stream->head != 0) ||
			(_component_input_stream->pending != 0) || (_component_input_stream->head != 0) ||
			(_component_output_stream->pending != 0) || (_component_output_stream->head != 0)))
		_finish (_exit_code_succeeded);
}


static void _packet_parse (struct _packet * const _packet)
{
	_assert (_packet != 0); _assert (_packet->state == _packet_state_inputed);
	_packet_parse_json (_packet);
	_packet_parse_action (_packet);
	_packet->state == _packet_state_parsed;
}

static void _packet_parse_json (struct _packet * const _packet)
{
	struct _buffer _buffer;
	_assert (_packet != 0); _assert (_packet->state == _packet_state_inputed); _assert (_packet->json == 0); _assert (_packet->buffer != 0);
	_assert (_packet->buffer->available >= 4); _assert ((_packet->size + 4) == _packet->buffer->available); _assert (_packet->buffer->offset == 0);
	_buffer = *_packet->buffer;
	_buffer.offset = 4;
	_buffer_parse (&_packet->json, &_buffer);
}


static void _packet_parse_action (struct _packet * const _packet)
{
	json_error_t _json_error;
	signed int _json_outcome;
	char * const _action_name;
	_assert (_packet != 0); _assert (_packet->state == _packet_state_inputed); _assert (_packet->json != 0);
	_assert (_packet->action == _packet_action_undefined);
	_json_outcome = json_unpack_ex (_packet->json, &_json_error, JSON_STRICT, "{s:s}", "__action__", &_action_name);
	if (_json_outcome != 0)
		_fail (_json_error.text, _exit_code_invalid_action);
	if (_action_name == 0)
		_fail ("action is null", _exit_code_invalid_action);
	if (strcmp (_action_name, "exchange") == 0)
		_packet->action = _packet_action_exchange;
	else if (strcmp (_action_name, "execute") == 0)
		_packet->action = _packet_action_exec;
	else if (strcmp (_action_name, "signal") == 0)
		_packet->action = _packet_action_kill;
	else
		_fail ("action is unknown", _exit_code_invalid_action);
}


static void _packet_streams_poll (struct _packet_stream * const * const _streams, unsigned int _count)
{
	struct pollfd _poll[16];
	signed int _poll_outcome;
	unsigned int _index;
	_assert (_count <= 16);
	for (_index = 0; _index < _count; _index++) {
		_poll[_index].fd = _streams[_index]->descriptor;
		_poll[_index].events = _streams[_index]->poll;
		_poll[_index].revents = 0;
		_assert ((_poll[_index].events == 0) || (_poll[_index].events == POLLIN) || (_poll[_index].events == POLLOUT));
	}
	_poll_outcome = poll (_poll, _count, 100);
	for (_index = 0; _index < _count; _index++) {
		_assert ((_poll[_index].revents & !(POLLIN | POLLOUT | POLLERR | POLLHUP)) == 0);
		_streams[_index]->poll = (_poll[_index].events != 0) && (_poll[_index].revents != 0);
	}
}


static void _packet_stream_input (struct _packet_stream * const _stream)
{
	_assert (_stream != 0);
	if (_stream->pending == 0) {
		_packet_allocate (&_stream->pending);
		_stream->pending->state = _packet_state_input_ready;
	}
	_packet_input (_stream->pending, _stream->descriptor);
	struct _packet * _packet = _stream->pending;
	switch (_stream->pending->state) {
		case _packet_state_inputed :
		case _packet_state_stream_ended :
		case _packet_state_stream_error :
			_packet_stream_enqueue (_stream, &_stream->pending);
			break;
		default :
			break;
	}
}

static void _packet_stream_output (struct _packet_stream * const _stream)
{
	_assert (_stream != 0);
	if (_stream->pending == 0) {
		_packet_stream_dequeue (_stream, &_stream->pending);
		_stream->pending->state = _packet_state_output_ready;
	}
	_packet_output (_stream->pending, _stream->descriptor);
	if (_stream->pending->state == _packet_state_outputed)
		_packet_deallocate (&_stream->pending);
}


static void _packet_stream_enqueue (struct _packet_stream * const _stream, struct _packet * * _packet_)
{
	struct _packet * _packet;
	_assert (_stream != 0); _assert (_packet_ != 0); _assert (*_packet_ != 0);
	_packet = *_packet_;
	*_packet_ = 0;
	_assert (_packet->chained == 0);
	if (_stream->tail == 0) {
		_assert (_stream->head == 0);
		_stream->tail = _packet;
		_stream->head = _packet;
	} else {
		_assert (_stream->head != 0);
		_stream->tail->chained = _packet;
		_stream->tail = _packet;
	}
	_packet->chained = 0;
}

static void _packet_stream_dequeue (struct _packet_stream * const _stream, struct _packet * * _packet_)
{
	struct _packet * _packet;
	_assert (_stream != 0); _assert (_packet_ != 0); _assert (*_packet_ == 0);
	_assert (_stream->head != 0); _assert (_stream->tail != 0);
	_packet = _stream->head;
	_stream->head = _packet->chained;
	if (_stream->head == 0)
		_stream->tail = 0;
	_packet->chained = 0;
	*_packet_ = _packet;
}


static void _packet_stream_initialize (struct _packet_stream * const _stream, unsigned int const descriptor)
{
	_assert (_stream != 0);
	_stream->descriptor = descriptor;
	_stream->poll = 0;
	_stream->head = 0;
	_stream->tail = 0;
	_stream->pending = 0;
}


static void _packet_input (struct _packet * const _packet, unsigned int _stream)
{
	size_t _read_window;
	ssize_t _read_outcome;
	_assert (_packet != 0); _assert (_packet->buffer != 0); _assert (_stream >= 0);
	switch (_packet->state) {
		case _packet_state_input_ready :
			_packet->buffer->offset = 0;
			_packet->buffer->available = 0;
			_packet->state = _packet_state_input_waiting_size;
		case _packet_state_input_waiting_size :
			_assert (_packet->size == 0); _assert (_packet->buffer->offset < 4);
			_read_window = 4;
			break;
		case _packet_state_input_waiting_data :
			_assert (_packet->size >= 0); _assert (_packet->buffer->offset >= 4); _assert (_packet->size <= _packet->buffer->capacity);
			_read_window = _packet->size + (_packet->buffer->offset - 4);
			break;
		default :
			_assert (_packet->state && 0);
			break;
	}
	_assert (_packet->buffer->offset == _packet->buffer->available);
	_read_outcome = read (_stream, _packet->buffer->data + _packet->buffer->offset, _read_window);
	switch (_read_outcome) {
		case 0 :
			_packet->state = _packet_state_stream_ended;
			break;
		case -1 :
			_packet->state = _packet_state_stream_error;
			break;
		default :
			_assert (_read_outcome > 0);
			_packet->buffer->offset += _read_outcome;
			_packet->buffer->available = _packet->buffer->offset;
			break;
	}
	switch (_packet->state) {
		case _packet_state_input_waiting_size :
			_assert (_packet->size == 0); _assert (_packet->buffer->offset <= 4);
			if (_packet->buffer->offset == 4) {
				_packet->size = ntohl (*((unsigned int *) _packet->buffer->data));
				if ((_packet->size + 4) > _packet->buffer->capacity)
					_buffer_reallocate (&_packet->buffer, _packet->size + 4);
				_packet->state = _packet_state_input_waiting_data;
			}
			break;
		case _packet_state_input_waiting_data :
			_assert (_packet->buffer->offset >= 4);
			if (_packet->buffer->offset == (_packet->size + 4)) {
				_packet->buffer->offset = 0;
				_packet->state = _packet_state_inputed;
			}
			break;
		case _packet_state_stream_ended :
		case _packet_state_stream_error :
			break;
		default :
			_assert (_packet->state && 0);
			break;
	}
}


static void _packet_output (struct _packet * const _packet, unsigned int _stream)
{
	size_t _write_window;
	ssize_t _write_outcome;
	_assert (_packet != 0); _assert (_packet->buffer != 0); _assert (_stream >= 0);
	switch (_packet->state) {
		case _packet_state_output_ready :
			_packet->buffer->offset = 0;
			_packet->buffer->available = _packet->size + 4;
			_packet->state = _packet_state_output_waiting;
		case _packet_state_output_waiting :
			_assert (_packet->size >= 0); _assert ((_packet->size + 4) == _packet->buffer->available);
			_assert (_packet->buffer->offset < _packet->buffer->available); _assert (_packet->buffer->available <= _packet->buffer->capacity);
			break;
		default :
			_assert (_packet->state && 0);
			break;
	}
	_write_window = _packet->buffer->available - _packet->buffer->offset;
	_write_outcome = write (_stream, _packet->buffer->data, _write_window);
	switch (_write_outcome) {
		case 0 :
		case -1 :
			_packet->state = _packet_state_stream_error;
			break;
		default :
			_assert (_write_outcome > 0);
			_packet->buffer->offset += _write_outcome;
			break;
	}
	if (_packet->buffer->offset == _packet->buffer->available)
		_packet->state = _packet_state_outputed;
}


static void _packet_allocate (struct _packet * * const _packet_)
{
	void * _memory;
	struct _packet * _packet;
	_assert (_packet_ != 0); _assert (*_packet_ == 0);
	_memory = malloc (sizeof (struct _packet));
	if (_memory == 0)
		_fail ("malloc failed", _exit_code_enomem);
	_packet = (struct _packet *) _memory;
	_packet->chained = 0;
	_packet->state = _packet_state_input_ready;
	_packet->size = 0;
	_packet->json = 0;
	_packet->action = _packet_action_undefined;
	_packet->buffer = 0;
	_buffer_allocate (&(_packet->buffer), 1024);
	*_packet_ = _packet;
}

static void _packet_deallocate (struct _packet * * const _packet_)
{
	struct _packet * _packet;
	_assert (_packet_ != 0); _assert (*_packet_ != 0);
	_packet = *_packet_;
	*_packet_ = 0;
	if (_packet->buffer != 0)
		_buffer_deallocate (&(_packet->buffer));
	free (_packet);
}


static void _buffer_parse (json_t * * const _json, struct _buffer * const _buffer)
{
	json_t * _json_;
	json_error_t _json_error;
	_assert (_json != 0); _assert (*_json == 0); _assert (_buffer != 0);
	_assert (_buffer->offset < _buffer->available);
	_buffer->data[_buffer->available] = '\0';
	_json_ = json_loads (_buffer->data + _buffer->offset, 0, &_json_error);
	if (_json_ == 0)
		_fail (_json_error.text, _exit_code_invalid_json);
	*_json = _json_;
}


static void _buffer_allocate (struct _buffer * * const _buffer_, unsigned int _capacity)
{
	void * _memory;
	struct _buffer * _buffer;
	_assert (_buffer_ != 0); _assert (*_buffer_ == 0);
	_memory = malloc (sizeof (struct _buffer) + _capacity + 1);
	if (_memory == 0)
		_fail ("malloc failed", _exit_code_enomem);
	_buffer = (struct _buffer *) _memory;
	_buffer->data = (unsigned char *) _memory + sizeof (struct _buffer);
	_buffer->capacity = _capacity;
	_buffer->offset = 0;
	_buffer->available = 0;
	*_buffer_ = _buffer;
}

static void _buffer_reallocate (struct _buffer * * const _buffer_, unsigned int _capacity)
{
	void * _memory;
	struct _buffer * _buffer;
	_assert (_buffer_ != 0); _assert (*_buffer_ != 0);
	_buffer = *_buffer_;
	*_buffer_ = 0;
	_assert (_buffer->data != 0); _assert (_buffer->capacity < _capacity);
	_memory = realloc (_buffer, sizeof (struct _buffer) + _capacity + 1);
	if (_memory == 0)
		_fail ("malloc failed", _exit_code_enomem);
	_buffer = (struct _buffer *) _memory;
	_buffer->data = (unsigned char *) _memory + sizeof (struct _buffer);
	_buffer->capacity = _capacity;
	*_buffer_ = _buffer;
}

static void _buffer_deallocate (struct _buffer * * const _buffer_)
{
	struct _buffer * _buffer;
	_assert (_buffer_ != 0); _assert (*_buffer_ != 0);
	_buffer = *_buffer_;
	*_buffer_ = 0;
	free (_buffer);
}


static void _finish (enum _exit_code const _code)
{
	exit (_code);
}

static void _fail (unsigned char const * const _reason, enum _exit_code const _code)
{
	fprintf (stderr, "[ee] aborting with %d: %s!\n", _code, _reason);
	exit (_code);
}


// ----------------------------------------

#define TRACE_LEVEL _trace_event_information
#define TRACE_SOURCE 0

enum _exit_status {
	_exit_status_succeeded = 0,
	_exit_status_failed_assertion,
	_exit_status_invalid_arguments,
	_exit_status_invalid_packet_type,
	_exit_status_invalid_packet_json,
	_exit_status_error_eio,
	_exit_status_error_enomem,
	_exit_status_max,
};

enum _context_state {
	_context_state_undefined = 0,
	_context_state_active,
	_context_state_flushing,
	_context_state_flushed,
	_context_state_max,
};

enum _process_state {
	_process_state_undefined = 0,
	_process_state_running,
	_process_state_exited,
	_process_state_max,
};

enum _process_signal {
	_process_signal_undefined = 0,
	_process_signal_invalid,
	_process_signal_terminate,
	_process_signal_destroy,
	_process_signal_max,
};

enum _packet_stream_state {
	_packet_stream_state_undefined = 0,
	_packet_stream_state_initialized,
	_packet_stream_state_inputing,
	_packet_stream_state_outputing,
	_packet_stream_state_closed,
	_packet_stream_state_failed,
	_packet_stream_state_max,
};

enum _packet_state {
	_packet_state_undefined = 0,
	_packet_state_initialized,
	_packet_state_input_ready,
	_packet_state_input_waiting_size,
	_packet_state_input_waiting_data,
	_packet_state_inputed,
	_packet_state_parsed,
	_packet_state_output_ready,
	_packet_state_output_waiting,
	_packet_state_outputed,
	_packet_state_stream_closed,
	_packet_state_stream_failed,
	_packet_state_max,
};

enum _packet_type {
	_packet_type_undefined = 0,
	_packet_type_invalid,
	_packet_type_terminate,
	_packet_type_exchange,
	_packet_type_execute,
	_packet_type_signal,
	_packet_type_max,
};

enum _trace_event {
	_trace_event_undefined,
	_trace_event_debugging,
	_trace_event_information,
	_trace_event_warning,
	_trace_event_error,
	_trace_event_max,
};

enum _cleanup_action {
	_cleanup_action_undefined = 0,
	_cleanup_action_canceled,
	_cleanup_action_close,
	_cleanup_action_kill,
	_cleanup_action_max,
};

unsigned int const _timeout_slice = 100;
unsigned int const _timeout_waitpid = 2 * 100;

// ----------------------------------------

#include <jansson.h>

// ----------------------------------------

struct _context;
struct _process;
struct _process_configuration;
struct _packet_stream;
struct _packet;
struct _buffer;

// ----------------------------------------

struct _context {
	enum _context_state state;
	struct _packet_stream * controller_input_stream;
	struct _packet_stream * controller_output_stream;
	struct _process * process;
};

struct _process {
	enum _process_state state;
	struct _process_configuration * configuration;
	struct _packet_stream * input_stream;
	struct _packet_stream * output_stream;
	unsigned int descriptor;
	unsigned int exit_status;
	struct _cleanup * cleanup;
};

struct _process_configuration {
	unsigned char * executable;
	unsigned int argument_count;
	unsigned char * * argument_values;
	unsigned int environment_count;
	unsigned char * * environment_values;
	unsigned char * working_directory;
	struct _buffer * malloc;
};

struct _packet_stream {
	enum _packet_stream_state state;
	struct _packet * queue_head;
	struct _packet * queue_tail;
	struct _packet * pending;
	unsigned int descriptor;
	unsigned int poll_waiting;
	unsigned int poll_pending;
	struct _cleanup * cleanup;
};

struct _packet {
	struct _packet * chained;
	enum _packet_state state;
	enum _packet_type type;
	unsigned int size;
	struct _buffer * buffer;
	json_t * json;
};

struct _buffer {
	unsigned int capacity;
	unsigned int offset;
	unsigned int available;
	unsigned char * data;
};

struct _cleanup {
	enum _cleanup_action action;
	union {
		struct {
			unsigned int descriptor;
		} close;
		struct {
			unsigned int descriptor;
		} kill;
	} arguments;
};

// ----------------------------------------

static void _context_handle_execute_packet (
		struct _context * const _context,
		struct _packet * * const _packet);

static void _context_handle_signal_packet (
		struct _context * const _context,
		struct _packet * * const _packet);

static void _context_check (
		struct _context * const _context);

static void _context_loop (
		struct _context * const _context);

static void _context_handle_controller_packet (
		struct _context * const _context,
		struct _packet * * const _packet);

static void _context_handle_component_packet (
		struct _context * const _context,
		struct _packet * * const _packet);

static void _context_handle_packets (
		struct _context * const _context,
		unsigned int * const _reschedule);

static void _context_poll_packets (
		struct _context * const _context,
		unsigned int const _timeout,
		unsigned int * const _reschedule);

static void _context_create (
		struct _context * * const _context,
		unsigned int const _controller_input_descriptor,
		unsigned int const _controller_output_descriptor);

static void _context_destroy (
		struct _context * * const _context);

// ----------------------------------------

static void _process_poll (
		struct _process * const _process);

static void _process_signal (
		struct _process * const _process,
		enum _process_signal const _signal);

static void _process_create (
		struct _process * * const _process,
		struct _process_configuration * * const _configuration);

static void _process_destroy (
		struct _process * * const _process);

// ----------------------------------------

static void _process_configuration_create (
		struct _process_configuration * * const _configuration,
		json_t * const _json);

static void _process_configuration_destroy (
		struct _process_configuration * * const _configuration);

static void _process_signal_parse (
		enum _process_signal * const _signal,
		json_t * const _json);

// ----------------------------------------

static void _packet_streams_poll (
		struct _packet_stream * const * const _streams,
		unsigned int const _timeout,
		unsigned int const _pollable_count,
		unsigned int * const _polled_count);

static void _packet_stream_input (
		struct _packet_stream * const _stream);

static void _packet_stream_output (
		struct _packet_stream * const _stream);

static void _packet_stream_enqueue (
		struct _packet_stream * const _stream,
		struct _packet * * const _packet);

static void _packet_stream_dequeue (
		struct _packet_stream * const _stream,
		struct _packet * * const _packet);

static void _packet_stream_create (
		struct _packet_stream * * const _stream,
		unsigned int const descriptor);

static void _packet_stream_destroy (
		struct _packet_stream * * const _stream);

// ----------------------------------------

static void _packet_parse (
		struct _packet * const _packet);

static void _packet_parse_json (
		struct _packet * const _packet);

static void _packet_parse_type (
		struct _packet * const _packet);

static void _packet_input (
		struct _packet * const _packet,
		unsigned int _stream);

static void _packet_output (
		struct _packet * const _packet,
		unsigned int _stream);

static void _packet_allocate (
		struct _packet * * const _packet,
		unsigned int const _size);

static void _packet_deallocate (
		struct _packet * * const _packet);

// ----------------------------------------

static void _buffer_malloc_block (
		void * * const _block,
		unsigned int const _size,
		struct _buffer * const _buffer);

static void _buffer_malloc_strdup (
		unsigned char * * const _destination,
		unsigned char const * const _source,
		struct _buffer * const _buffer);

// ----------------------------------------

static void _buffer_parse_json (
		json_t * * const _json,
		struct _buffer * const _buffer);

static void _buffer_allocate (
		struct _buffer * * const _buffer,
		unsigned int const _capacity);

static void _buffer_reallocate (
		struct _buffer * * const _buffer,
		unsigned int const _capacity);

static void _buffer_deallocate (
		struct _buffer * * const _buffer);

// ----------------------------------------

static void __terminate (
		enum _exit_status const _exit_status,
		unsigned char const * const _source_file,
		unsigned int const _source_line);

static void __terminate_with_reason_1 (
		enum _exit_status const _exit_status,
		unsigned char const * const _source_file,
		unsigned int const _source_line,
		unsigned char const * const _reason);

static void __terminate_with_reason_2 (
		enum _exit_status const _exit_status,
		unsigned char const * const _source_file,
		unsigned int const _source_line,
		unsigned char const * const _reason_1,
		unsigned char const * const _reason_2);

static void __enforce (
		unsigned long int _condition_value,
		unsigned char const * const _condition_expression,
		unsigned char const * const _source_file,
		unsigned int const _source_line);

static void __assert (
		unsigned long int _condition_value,
		unsigned char const * const _condition_expression,
		unsigned char const * const _source_file,
		unsigned int const _source_line);

static void __trace (
		enum _trace_event const _event,
		unsigned char const * const _message,
		unsigned char const * const _source_file,
		unsigned int const _source_line);

// ----------------------------------------

static void _cleanup_register (
		struct _cleanup * * const _cleanup);

static void _cleanup_register_close (
		struct _cleanup * * const _cleanup,
		unsigned int _descriptor);

static void _cleanup_register_kill (
		struct _cleanup * * const _cleanup,
		unsigned int _descriptor);

static void _cleanup_execute (void);

static void _cleanup_cancel (void);

static unsigned int _pid;

// ----------------------------------------

unsigned int main (
		unsigned int const _argument_count,
		unsigned char const * const * const _argument_values)
{
	struct _cleanup * _input_cleanup;
	struct _cleanup * _output_cleanup;
	struct _context * _context;
	
	_pid = getpid ();
	
	_input_cleanup = 0;
	_cleanup_register_close (&_input_cleanup, 0);
	_output_cleanup = 0;
	_cleanup_register_close (&_output_cleanup, 1);
	
	if (_argument_count != 1)
		__terminate (_exit_status_invalid_arguments, __func__, __LINE__);
	
	_context = 0;
	_context_create (&_context, 0, 1);
	_context->controller_input_stream->cleanup = _input_cleanup;
	_context->controller_output_stream->cleanup = _output_cleanup;
	
	_context_loop (_context);
	
	_context_destroy (&_context);
	
	_cleanup_execute ();
	
	if (TRACE_LEVEL <= _trace_event_debugging)
		__trace (_trace_event_information, "exited!", __func__, __LINE__);
	
	exit (0);
}

// ----------------------------------------

#include <errno.h>
#include <poll.h>
#include <string.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

extern unsigned char const * const * const environ;

// ----------------------------------------

#define _terminate(_exit_status) \
		__terminate_with_reason_1 ((_exit_status), __func__, __LINE__, #_exit_status);
#define _terminate_with_reason_1(_exit_status, _reason_1) \
		__terminate_with_reason_1 ((_exit_status), __func__, __LINE__, (_reason_1))
#define _terminate_with_reason_2(_exit_status, _reason_1, _reason_2) \
		__terminate_with_reason_2 ((_exit_status), __func__, __LINE__, (_reason_1), (_reason_2))

#define _enforce(_condition) \
		__enforce ((_condition), #_condition, __func__, __LINE__)
#define _assert(_condition) \
		__assert ((_condition), #_condition, __func__, __LINE__)

#define ___s(_value) \
		do { fprintf (stderr, "[%5u][##] `%s` := \"%s\" @@ `%s`/%d\n", _pid, #_value, (_value), __func__, __LINE__); fflush (stderr); } while (0);
#define ___p(_value) \
		do { fprintf (stderr, "[%5u][##] `%s` := %p @@ `%s`/%d\n", _pid, #_value, (_value), __func__, __LINE__); fflush (stderr); } while (0);
#define ___ui(_value) \
		do { fprintf (stderr, "[%5u][##] `%s` := %u @@ `%s`/%d\n", _pid, #_value, (_value), __func__, __LINE__); fflush (stderr); } while (0);
#define ___ \
		do { fprintf (stderr, "[%5u][##] @@ `%s`/%d\n", _pid, __func__, __LINE__); fflush (stderr); } while (0);

#define _trace_debugging(_message) \
		do { if (TRACE_LEVEL <= _trace_event_debugging) __trace (_trace_event_debugging, _message, __func__, __LINE__); } while (0)
#define _trace_information(_message) \
		do { if (TRACE_LEVEL <= _trace_event_information) __trace (_trace_event_information, _message, __func__, __LINE__); } while (0)
#define _trace_warning(_message) \
		do { if (TRACE_LEVEL <= _trace_event_warning) __trace (_trace_event_warning, _message, __func__, __LINE__); } while (0)
#define _trace_error(_message) \
		do { if (TRACE_LEVEL <= _trace_event_error) __trace (_trace_event_error, _message, __func__, __LINE__); } while (0)

// ----------------------------------------

static void _context_handle_execute_packet (
		struct _context * const _context,
		struct _packet * * const _packet_)
{
	struct _packet * _packet;
	struct _process_configuration * _configuration;
	_trace_debugging ("handling execute packet...");
	_assert (_context != 0);
	_assert (_packet_ != 0);
	_assert (*_packet_ != 0);
	_packet = *_packet_; *_packet_ = 0;
	_assert ((_context->state == _context_state_active) || (_context->state == _context_state_flushing));
	_assert (_packet->state == _packet_state_parsed);
	_assert (_packet->type == _packet_type_execute);
	
	_enforce (_context->process == 0);
	_enforce (_context->state == _context_state_active);
	
	_configuration = 0;
	_process_configuration_create (&_configuration, _packet->json);
	_packet_deallocate (&_packet);
	
	_process_create (&_context->process, &_configuration);
}

static void _context_handle_signal_packet (
		struct _context * const _context,
		struct _packet * * const _packet_)
{
	struct _packet * _packet;
	enum _process_signal _signal;
	_trace_debugging ("handling signal packet...");
	_assert (_context != 0);
	_assert (_packet_ != 0);
	_assert (*_packet_ != 0);
	_packet = *_packet_; *_packet_ = 0;
	_assert ((_context->state == _context_state_active) || (_context->state == _context_state_flushing));
	_assert (_packet->state == _packet_state_parsed);
	_assert (_packet->type == _packet_type_signal);
	
	_enforce (_context->process != 0);
	
	_signal = 0;
	_process_signal_parse (&_signal, _packet->json);
	_packet_deallocate (&_packet);
	
	if (_signal != _process_signal_invalid) {
		if (_context->process->state == _process_state_running)
			_process_signal (_context->process, _signal);
		else
			_trace_warning ("dropped signal due to exited process...");
	} else
		_trace_warning ("dropped signal due to invalid value...");
}

static void _context_check (
		struct _context * const _context)
{
	_assert (_context != 0);
	switch (_context->state) {
		case _context_state_active :
		case _context_state_flushing :
			if ((_context->controller_output_stream != 0)
					&& (_context->controller_output_stream->state != _packet_stream_state_outputing)) {
				_trace_information ("closing controller output stream...");
				_packet_stream_destroy (&_context->controller_output_stream);
			}
			if ((_context->controller_input_stream != 0)
					&& (_context->controller_input_stream->state != _packet_stream_state_inputing)
					&& (_context->controller_input_stream->pending == 0)
					&& (_context->controller_input_stream->queue_head == 0)) {
				_trace_information ("closing controller input stream...");
				_packet_stream_destroy (&_context->controller_input_stream);
			}
			if ((_context->process != 0) && (_context->process->output_stream != 0)
					&& (_context->process->output_stream->state != _packet_stream_state_outputing)) {
				_trace_information ("closing process input stream...");
				_packet_stream_destroy (&_context->process->output_stream);
			}
			if ((_context->process != 0) && (_context->process->input_stream != 0)
					&& (_context->process->input_stream->state != _packet_stream_state_inputing)
					&& (_context->process->input_stream->pending == 0)
					&& (_context->process->input_stream->queue_head == 0)) {
				_trace_information ("closing process output stream...");
				_packet_stream_destroy (&_context->process->input_stream);
			}
			if ((_context->process != 0)
					&& ((_context->controller_input_stream == 0) || (_context->state == _context_state_flushing))
					&& (_context->process->output_stream != 0)
					&& (_context->process->output_stream->pending == 0)
					&& (_context->process->output_stream->queue_head == 0)) {
				_trace_information ("closing process input stream...");
				_packet_stream_destroy (&_context->process->output_stream);
			}
			if ((_context->process != 0) && (_context->process->state == _process_state_running)) {
				_process_poll (_context->process);
				if (_context->process->state == _process_state_exited) {
					if (_context->controller_output_stream != 0) {
						// !!!! FIXME !!!!
						struct _packet * _packet;
						json_t * _packet_json;
						unsigned char * _packet_payload;
						unsigned int _packet_payload_size;
						_packet_json = json_pack ("{s:s,s:i}", "__type__", "exit", "exit-status", _context->process->exit_status);
						_enforce (_packet_json != 0);
						_packet_payload = json_dumps (_packet_json, JSON_COMPACT);
						_enforce (_packet_payload != 0);
						json_decref (_packet_json);
						_packet_payload_size = strlen (_packet_payload) + 1;
						_packet = 0;
						_packet_allocate (&_packet, _packet_payload_size);
						*((unsigned int *) _packet->buffer->data) = htonl (_packet_payload_size);
						memcpy (_packet->buffer->data + 4, _packet_payload, _packet_payload_size - 1);
						_packet->buffer->data[4 + _packet_payload_size - 1] = '\0';
						free (_packet_payload);
						_packet->size = _packet_payload_size;
						_packet->state = _packet_state_output_ready;
						_packet_stream_enqueue (_context->controller_output_stream, &_packet);
					}
					_process_destroy (&_context->process);
				}
			}
			break;
	}
	switch (_context->state) {
		case _context_state_active :
			if (_context->controller_input_stream == 0)
				_context->state = _context_state_flushing;
			break;
		case _context_state_flushing :
			if (
					(_context->process == 0)
					&& ((_context->controller_input_stream == 0)
						|| ((_context->controller_input_stream->pending == 0)
							&& (_context->controller_input_stream->queue_head == 0)))
					&& ((_context->controller_output_stream == 0)
						|| ((_context->controller_output_stream->pending == 0)
							&& (_context->controller_output_stream->queue_head == 0))))
				_context->state = _context_state_flushed;
			break;
		case _context_state_flushed :
			break;
		default :
			_enforce (0);
			break;
	}
}

static void _context_loop (
		struct _context * const _context)
{
	unsigned int _reschedule;
	unsigned int _handle_packets_reschedule;
	unsigned int _poll_packets_reschedule;
	unsigned int _poll_packets_timeout;
	_assert (_context != 0);
	_assert (_context->state == _context_state_active);
	_reschedule = 1;
	while (_reschedule) {
		_context_check (_context);
		switch (_context->state) {
			case _context_state_active :
			case _context_state_flushing :
				_reschedule = 1;
				_handle_packets_reschedule = 1;
				_poll_packets_reschedule = 1;
				_poll_packets_timeout = _timeout_slice;
				break;
			case _context_state_flushed :
				_reschedule = 0;
				break;
			default :
				_enforce (0);
				break;
		}
		while (_handle_packets_reschedule) {
			_handle_packets_reschedule = 0;
			_context_handle_packets (_context, &_handle_packets_reschedule);
		}
		while (_poll_packets_reschedule) {
			_poll_packets_reschedule = 0;
			_context_poll_packets (_context, _poll_packets_timeout, &_poll_packets_reschedule);
			_poll_packets_timeout = 0;
		}
	}
}

// ----------------------------------------

static void _context_handle_controller_packet (
		struct _context * const _context,
		struct _packet * * const _packet_)
{
	struct _packet * _packet;
	_assert (_context != 0);
	_assert (_packet_ != 0);
	_assert (*_packet_ != 0);
	_packet = *_packet_; *_packet_ = 0;
	_assert ((_context->state == _context_state_active) || (_context->state == _context_state_flushing));
	_assert (_packet->state == _packet_state_inputed);
	_packet_parse (_packet);
	switch (_packet->type) {
		case _packet_type_terminate :
			_trace_information ("terminating...");
			_context->state = _context_state_flushing;
			_packet_deallocate (&_packet);
			break;
		case _packet_type_exchange :
			if ((_context->process != 0) && (_context->process->output_stream != 0)) {
				_trace_debugging ("transferring inbound packet from controller to component...");
				_packet->state = _packet_state_output_ready;
				_packet->buffer->offset = 0;
				_packet_stream_enqueue (_context->process->output_stream, &_packet);
			} else {
				_trace_warning ("dropped inbound packet from controller to component due to closed stream...");
				_packet_deallocate (&_packet);
			}
			break;
		case _packet_type_execute :
			_context_handle_execute_packet (_context, &_packet);
			break;
		case _packet_type_signal :
			_context_handle_signal_packet (_context, &_packet);
			break;
		default :
			_trace_error ("dropped inbound packet from controller due to invalid type...");
			_packet_deallocate (&_packet);
			break;
	}
	_assert (_packet == 0);
}

static void _context_handle_component_packet (
		struct _context * const _context,
		struct _packet * * const _packet_)
{
	struct _packet * _packet;
	_assert (_context != 0);
	_assert (_packet_ != 0);
	_assert (*_packet_ != 0);
	_packet = *_packet_; *_packet_ = 0;
	_assert ((_context->state == _context_state_active) || (_context->state == _context_state_flushing));
	_assert (_packet->state == _packet_state_inputed);
	_packet_parse (_packet);
	switch (_packet->type) {
		case _packet_type_exchange :
			if (_context->controller_output_stream != 0) {
				_trace_debugging ("transferring inbound packet from component to controller...");
				_packet->state = _packet_state_output_ready;
				_packet->buffer->offset = 0;
				_packet_stream_enqueue (_context->controller_output_stream, &_packet);
			} else {
				_trace_warning ("dropped inbound packet from component to controller due to closed stream...");
				_packet_deallocate (&_packet);
			}
			break;
		default :
			_trace_error ("dropped inbound packet from component due to invalid type...");
			_packet_deallocate (&_packet);
			break;
	}
	_assert (_packet == 0);
}

static void _context_handle_packets (
		struct _context * const _context,
		unsigned int * const _reschedule)
{
	struct _packet_stream * _controller_input_stream;
	struct _packet_stream * _component_input_stream;
	struct _packet * _packet;
	
	_trace_debugging ("handling packets...");
	
	_assert (_context != 0);
	_assert (_reschedule != 0);
	_assert ((_context->state == _context_state_active) || (_context->state == _context_state_flushing));
	
	*_reschedule = 0;
	
	if (_context->controller_input_stream != 0)
		_controller_input_stream = _context->controller_input_stream;
	else
		_controller_input_stream = 0;
	if ((_context->process != 0) && (_context->process->input_stream != 0)) {
		_component_input_stream = _context->process->input_stream;
		_assert (_component_input_stream != 0);
	} else
		_component_input_stream = 0;
	
	if (_controller_input_stream != 0) {
		switch (_controller_input_stream->state) {
			case _packet_stream_state_closed :
			case _packet_stream_state_failed :
				if (_controller_input_stream->queue_head == 0)
					break;
			case _packet_stream_state_inputing :
				if (_controller_input_stream->queue_head != 0) {
					_packet = 0;
					_packet_stream_dequeue (_controller_input_stream, &_packet);
					_assert (_packet != 0);
					_context_handle_controller_packet (_context, &_packet);
					_assert (_packet == 0);
				}
				if (_controller_input_stream->queue_head != 0)
					*_reschedule = 1;
				break;
			default :
				_enforce (0);
				break;
		}
	}
	if (_component_input_stream != 0) {
		switch (_component_input_stream->state) {
			case _packet_stream_state_closed :
			case _packet_stream_state_failed :
				if (_component_input_stream->queue_head == 0)
					break;
			case _packet_stream_state_inputing :
				if (_component_input_stream->queue_head != 0) {
					_packet = 0;
					_packet_stream_dequeue (_component_input_stream, &_packet);
					_assert (_packet != 0);
					_context_handle_component_packet (_context, &_packet);
					_assert (_packet == 0);
				}
				if (_component_input_stream->queue_head != 0)
					*_reschedule = 1;
				break;
			default :
				_enforce (0);
				break;
		}
	}
}

// ----------------------------------------

static void _context_poll_packets (
		struct _context * const _context,
		unsigned int const _timeout,
		unsigned int * const _reschedule)
{
	struct _packet_stream * _controller_input_stream;
	struct _packet_stream * _controller_output_stream;
	struct _packet_stream * _component_input_stream;
	struct _packet_stream * _component_output_stream;
	struct _packet_stream * _pollable_streams[4];
	unsigned int _pollable_streams_count;
	unsigned int _polled_streams_count;
	
	_trace_debugging ("polling packets...");
	
	_assert (_context != 0);
	_assert (_reschedule != 0);
	
	_assert ((_context->state == _context_state_active) || (_context->state == _context_state_flushing));
	
	*_reschedule = 0;
	
	_controller_input_stream = 0;
	_controller_output_stream = 0;
	_component_input_stream = 0;
	_component_output_stream = 0;
	if (_context->controller_input_stream != 0)
		switch (_context->controller_input_stream->state) {
			case _packet_stream_state_inputing :
				_controller_input_stream = _context->controller_input_stream;
				break;
			case _packet_stream_state_closed :
			case _packet_stream_state_failed :
				break;
			default :
				_enforce (0);
				break;
		}
	if (_context->controller_output_stream != 0)
		switch (_context->controller_output_stream->state) {
			case _packet_stream_state_outputing :
				_controller_output_stream = _context->controller_output_stream;
				break;
			case _packet_stream_state_closed :
			case _packet_stream_state_failed :
				break;
			default :
				_enforce (0);
				break;
		}
	if ((_context->process != 0) && (_context->process->input_stream != 0))
		switch (_context->process->input_stream->state) {
			case _packet_stream_state_inputing :
				_component_input_stream = _context->process->input_stream;
				break;
			case _packet_stream_state_closed :
			case _packet_stream_state_failed :
				break;
			default :
				_enforce (0);
				break;
		}
	if ((_context->process != 0) && (_context->process->output_stream != 0))
		switch (_context->process->output_stream->state) {
			case _packet_stream_state_outputing :
				_component_output_stream = _context->process->output_stream;
				break;
			case _packet_stream_state_closed :
			case _packet_stream_state_failed :
				break;
			default :
				_enforce (0);
				break;
		}
	
	_pollable_streams_count = 0;
	if (_controller_input_stream != 0) {
		_controller_input_stream->poll_waiting = POLLHUP | POLLERR;
		if ((_controller_input_stream->pending != 0) || (_controller_input_stream->queue_head == 0))
			_controller_input_stream->poll_waiting |= POLLIN;
		_controller_input_stream->poll_pending = 0;
		if (_controller_input_stream->poll_waiting != 0) {
			_pollable_streams[_pollable_streams_count] = _controller_input_stream;
			_pollable_streams_count++;
		}
	}
	if (_controller_output_stream != 0) {
		_controller_output_stream->poll_waiting = POLLHUP | POLLERR;
		if ((_controller_output_stream->pending != 0) || (_controller_output_stream->queue_head != 0))
			_controller_output_stream->poll_waiting |= POLLOUT;
		_controller_output_stream->poll_pending = 0;
		if (_controller_output_stream->poll_waiting != 0) {
			_pollable_streams[_pollable_streams_count] = _controller_output_stream;
			_pollable_streams_count++;
		}
	}
	if (_component_input_stream != 0) {
		_component_input_stream->poll_waiting = POLLHUP | POLLERR;
		if ((_component_input_stream->pending != 0) || (_component_input_stream->queue_head == 0))
			_component_input_stream->poll_waiting |= POLLIN;
		_component_input_stream->poll_pending = 0;
		if (_component_input_stream->poll_waiting != 0) {
			_pollable_streams[_pollable_streams_count] = _component_input_stream;
			_pollable_streams_count++;
		}
	}
	if (_component_output_stream != 0) {
		_component_output_stream->poll_waiting = POLLHUP | POLLERR;
		if ((_component_output_stream->pending != 0) || (_component_output_stream->queue_head != 0))
			_component_output_stream->poll_waiting |= POLLOUT;
		_component_output_stream->poll_pending = 0;
		if (_component_output_stream->poll_waiting != 0) {
			_pollable_streams[_pollable_streams_count] = _component_output_stream;
			_pollable_streams_count++;
		}
	}
	
	_polled_streams_count = 0;
	_packet_streams_poll (_pollable_streams, _timeout, _pollable_streams_count, &_polled_streams_count);
	
	if ((_controller_input_stream != 0) && (_controller_input_stream->poll_pending != 0)) {
		_trace_debugging ("inputing inbound packets from controller...");
		_packet_stream_input (_controller_input_stream);
	}
	if ((_controller_output_stream != 0) && (_controller_output_stream->poll_pending != 0)) {
		_trace_debugging ("outputing outbound packets to controller...");
		_packet_stream_output (_controller_output_stream);
	}
	if ((_component_input_stream != 0) && (_component_input_stream->poll_pending != 0)) {
		_trace_debugging ("inputing inbound packets from component...");
		_packet_stream_input (_component_input_stream);
	}
	if ((_component_output_stream != 0) && (_component_output_stream->poll_pending != 0)) {
		_trace_debugging ("outputing outbound packets to component...");
		_packet_stream_output (_component_output_stream);
	}
	
	*_reschedule = (_polled_streams_count > 0);
}

// ----------------------------------------

static void _context_create (
		struct _context * * const _context_,
		unsigned int const _controller_input_descriptor,
		unsigned int const _controller_output_descriptor)
{
	struct _context * _context;
	_assert (_context_ != 0);
	_assert (*_context_ == 0);
	_context = malloc (sizeof (struct _context));
	_enforce (_context != 0);
	memset (_context, 0x00, sizeof (struct _context));
	_packet_stream_create (&_context->controller_input_stream, _controller_input_descriptor);
	_context->controller_input_stream->state = _packet_stream_state_inputing;
	_packet_stream_create (&_context->controller_output_stream, _controller_output_descriptor);
	_context->controller_output_stream->state = _packet_stream_state_outputing;
	_context->state = _context_state_active;
	signal (SIGPIPE, SIG_IGN);
	*_context_ = _context;
}

static void _context_destroy (
		struct _context * * const _context_)
{
	struct _context * _context;
	_assert (_context_ != 0);
	_assert (*_context_ != 0);
	_context = *_context_; *_context_ = 0;
	if (_context->process != 0)
		_process_destroy (&_context->process);
	if (_context->controller_input_stream != 0)
		_packet_stream_destroy (&_context->controller_input_stream);
	if (_context->controller_output_stream != 0)
		_packet_stream_destroy (&_context->controller_output_stream);
	memset (_context, 0xee, sizeof (struct _context));
	free (_context);
}

// ----------------------------------------

static void _process_poll (
		struct _process * const _process)
{
	signed int _outcome;
	signed int _status;
	_trace_debugging ("polling process...");
	_assert (_process != 0);
	_assert (_process->state == _process_state_running);
	_assert (_process->descriptor > 0);
	_assert (_process->cleanup != 0);
	_outcome = waitpid (_process->descriptor, &_status, WNOHANG);
	if (_outcome == _process->descriptor) {
		_trace_information ("process exited...");
		_process->descriptor = 0;
		_process->exit_status = _status;
		_process->state = _process_state_exited;
		_process->cleanup->action = _cleanup_action_canceled;
		_process->cleanup = 0;
	} else if (_outcome == 0)
		;
	else
		_enforce (0);
}

static void _process_signal (
		struct _process * const _process,
		enum _process_signal const _signal)
{
	signed int _killsig;
	signed int _outcome;
	_assert (_process != 0);
	_assert (_process->state == _process_state_running);
	_assert (_process->descriptor > 0);
	_assert ((_signal > _process_signal_invalid) && (_signal < _process_signal_max));
	_trace_information ("signalling process...");
	switch (_signal) {
		case _process_signal_terminate :
			_killsig = SIGTERM;
			break;
		case _process_signal_destroy :
			_killsig = SIGKILL;
			break;
		default :
			_enforce (0);
			break;
	};
	_outcome = kill (_process->descriptor, _killsig);
}

static void _process_create (
		struct _process * * const _process_,
		struct _process_configuration * * const _configuration_)
{
	struct _process * _process;
	struct _process_configuration * _configuration;
	unsigned int _input_descriptors[2];
	unsigned int _output_descriptors[2];
	struct _cleanup * _close_cleanups[4];
	struct _cleanup * _kill_cleanup;
	signed int _pipe_outcome;
	signed int _fork_outcome;
	signed int _chdir_outcome;
	signed int _exec_outcome;
	
	_trace_information ("process creating...");
	
	_assert (_process_ != 0);
	_assert (*_process_ == 0);
	_assert (_configuration_ != 0);
	_assert (*_configuration_ != 0);
	
	_configuration = *_configuration_;
	*_configuration_ = 0;
	
	_assert (_configuration->executable != 0);
	_assert (_configuration->argument_values != 0);
	_assert (_configuration->argument_count >= 1);
	_assert (_configuration->environment_values != 0);
	
	_process = malloc (sizeof (struct _process));
	_enforce (_process != 0);
	memset (_process, 0x00, sizeof (struct _process));
	
	_process->configuration = _configuration;
	
	_pipe_outcome = pipe (_input_descriptors);
	_enforce (_pipe_outcome == 0);
	_close_cleanups[0] = 0;
	_cleanup_register_close (&_close_cleanups[0], _input_descriptors[0]);
	_close_cleanups[1] = 0;
	_cleanup_register_close (&_close_cleanups[1], _input_descriptors[1]);
	
	_pipe_outcome = pipe (_output_descriptors);
	_enforce (_pipe_outcome == 0);
	_close_cleanups[2] = 0;
	_cleanup_register_close (&_close_cleanups[2], _output_descriptors[0]);
	_close_cleanups[3] = 0;
	_cleanup_register_close (&_close_cleanups[3], _output_descriptors[1]);
	
	_fork_outcome = fork ();
	_enforce (_fork_outcome >= 0);
	
	if (_fork_outcome == 0) {
		
		signal (SIGPIPE, SIG_DFL);
		
		_cleanup_cancel ();
		
		if (_process->configuration->working_directory != 0) {
			_chdir_outcome = chdir (_process->configuration->working_directory);
			_enforce (_chdir_outcome == 0);
		}
		
		_pipe_outcome = dup2 (_input_descriptors[1], 1);
		_enforce (_pipe_outcome == 1);
		_pipe_outcome = dup2 (_output_descriptors[0], 0);
		_enforce (_pipe_outcome == 0);
		_pipe_outcome = 0;
		_pipe_outcome |= close (_input_descriptors[0]);
		_pipe_outcome |= close (_input_descriptors[1]);
		_pipe_outcome |= close (_output_descriptors[0]);
		_pipe_outcome |= close (_output_descriptors[1]);
		_enforce (_pipe_outcome == 0);
		
		_exec_outcome = execve (_process->configuration->executable, (char **) _process->configuration->argument_values, (char **) _process->configuration->environment_values);
		
		_enforce (0);
		
	} else if (_fork_outcome > 0) {
		
		_kill_cleanup = 0;
		_cleanup_register_kill (&_kill_cleanup, _fork_outcome);
		
		_pipe_outcome = 0;
		_pipe_outcome |= close (_input_descriptors[1]);
		_pipe_outcome |= close (_output_descriptors[0]);
		_close_cleanups[1]->action = _cleanup_action_canceled;
		_close_cleanups[2]->action = _cleanup_action_canceled;
		_enforce (_pipe_outcome == 0);
		
		_process->descriptor = _fork_outcome;
		_packet_stream_create (&_process->input_stream, _input_descriptors[0]);
		_process->input_stream->state = _packet_stream_state_inputing;
		_process->input_stream->cleanup = _close_cleanups[0];
		_packet_stream_create (&_process->output_stream, _output_descriptors[1]);
		_process->output_stream->state = _packet_stream_state_outputing;
		_process->output_stream->cleanup = _close_cleanups[3];
		_process->state = _process_state_running;
		_process->cleanup = _kill_cleanup;
		
	} else
		_enforce (0);
	
	*_process_ = _process;
}

static void _process_destroy (
		struct _process * * const _process_)
{
	struct _process * _process;
	signed int _outcome;
	unsigned int _status;
	unsigned int _timeout;
	_assert (_process_ != 0); _assert (*_process_ != 0);
	_process = *_process_;
	*_process_ = 0;
	_assert ((_process->state > _process_state_undefined) && (_process->state < _process_state_max));
	_assert (_process->configuration != 0);
	if (_process->state == _process_state_running) {
		_assert (_process->descriptor > 0);
		_assert (_process->cleanup != 0);
		_trace_information ("closing process...");
		if (_process->input_stream != 0)
			_packet_stream_destroy (&_process->input_stream);
		if (_process->output_stream != 0)
			_packet_stream_destroy (&_process->output_stream);
		_trace_information ("waiting process...");
		for (_timeout = 0; _timeout < 6000; _timeout += _timeout_slice) {
			_outcome = waitpid (_process->descriptor, &_status, WNOHANG);
			if (_outcome == _process->descriptor)
				break;
			else if (_outcome == 0)
				;
			else
				_enforce (0);
			usleep (_timeout_slice);
		}
		if (_outcome == _process->descriptor) {
			_trace_information ("process exited...");
			_process->exit_status = _status;
		} else if (_outcome == 0) {
			_trace_warning ("killing process...");
			kill (_process->descriptor, SIGTERM);
			for (_timeout = 0; _timeout < 6000; _timeout += _timeout_slice) {
				_outcome = waitpid (_process->descriptor, &_status, WNOHANG);
				if (_outcome == _process->descriptor) {
					_process->exit_status = _status;
					break;
				} else if (_outcome == 0)
					;
				else
					_enforce (0);
				usleep (_timeout_slice);
			}
			if (_outcome != _process->descriptor) {
				kill (_process->descriptor, SIGKILL);
				waitpid (_process->descriptor, 0, WNOHANG);
				_process->exit_status = ~0;
			}
			_trace_information ("process killed...");
		} else
			_enforce (0);
		_process->state = _process_state_exited;
		_process->cleanup->action = _cleanup_action_canceled;
	} else if (_process->state == _process_state_exited) {
		_assert (_process->descriptor == 0);
		_assert (_process->cleanup == 0);
	} else
		_enforce (0);
	_process_configuration_destroy (&_process->configuration);
	memset (_process, 0xee, sizeof (struct _process));
	free (_process);
}

// ----------------------------------------

static void _process_configuration_create (
		struct _process_configuration * * const _configuration_,
		json_t * const _json)
{
	struct _process_configuration * _configuration;
	struct _buffer * _malloc;
	json_error_t _json_error;
	json_t * _executable_json;
	json_t * _argument0_json;
	json_t * _arguments_json;
	json_t * _environment_json;
	json_t * _working_directory_json;
	json_t * _temporary_json;
	void * _iterator_json;
	signed int _json_outcome;
	unsigned int _count;
	unsigned int _index;
	
	_assert (_configuration_ != 0);
	_assert (*_configuration_ == 0);
	_assert (_json != 0);
	
	_malloc = 0;
	_buffer_allocate (&_malloc, 1024 * 1024);
	_malloc->available = _malloc->capacity;
	_configuration = 0;
	_buffer_malloc_block ((void **) &_configuration, sizeof (struct _process_configuration), _malloc);
	_configuration->malloc = _malloc;
	
	_json_outcome = json_unpack_ex (_json, &_json_error, JSON_STRICT, "{s:o,s:o,s:o,s:o,s:o}",
			"executable", &_executable_json, "argument0", &_argument0_json, "arguments", &_arguments_json,
			"environment", &_environment_json, "working-directory", &_working_directory_json);
	if (_json_outcome != 0)
		_terminate_with_reason_1 (_exit_status_invalid_packet_json, _json_error.text);
	
	if (json_is_string (_executable_json))
		_buffer_malloc_strdup (&_configuration->executable, json_string_value (_executable_json), _malloc);
	else
		_terminate (_exit_status_invalid_packet_json);
	
	if (json_is_array (_arguments_json)) {
		_count = json_array_size (_arguments_json);
		_buffer_malloc_block ((void **) &_configuration->argument_values, sizeof (unsigned char *) * (_count + 1 + 1), _malloc);
		_configuration->argument_values[0] = 0;
		for (_index = 0; _index < _count; _index++) {
			_temporary_json = json_array_get (_arguments_json, _index);
			if (json_is_string (_temporary_json))
				_buffer_malloc_strdup (&_configuration->argument_values[1 + _index], json_string_value (_temporary_json), _malloc);
			else
				_terminate (_exit_status_invalid_packet_json);
		}
		_configuration->argument_values[1 + _count] = 0;
		_configuration->argument_count = 1 + _count;
	} else if (json_is_null (_arguments_json)) {
		_buffer_malloc_block ((void **) &_configuration->argument_values, sizeof (unsigned char *) * (0 + 1 + 1), _malloc);
		_configuration->argument_values[0] = 0;
		_configuration->argument_values[1] = 0;
		_configuration->argument_count = 1;
	} else
		_terminate (_exit_status_invalid_packet_json);
	
	if (json_is_string (_argument0_json))
		_buffer_malloc_strdup (&_configuration->argument_values[0], json_string_value (_argument0_json), _malloc);
	else if (json_is_null (_argument0_json))
		_configuration->argument_values[0] = _configuration->executable;
	else
		_terminate (_exit_status_invalid_packet_json);
	
	if (json_is_object (_environment_json)) {
		_count = json_object_size (_environment_json);
		_buffer_malloc_block ((void **) &_configuration->environment_values, sizeof (unsigned char *) * (_count + 1), _malloc);
		for (
				_iterator_json = json_object_iter (_environment_json), _index = 0;
				_iterator_json != 0;
				_iterator_json = json_object_iter_next (_environment_json, _iterator_json), _index++)
		{
			_temporary_json = json_object_iter_value (_iterator_json);
			if (json_is_string (_temporary_json)) {
				unsigned int _size;
				_size = strlen (json_object_iter_key (_iterator_json)) + 1 + strlen (json_string_value (_temporary_json)) + 1;
				_buffer_malloc_block ((void **) &_configuration->environment_values[_index], _size, _malloc);
				strncat (_configuration->environment_values[_index], json_object_iter_key (_iterator_json), _size - 1);
				strncat (_configuration->environment_values[_index], "=", _size - 1);
				strncat (_configuration->environment_values[_index], json_string_value (_temporary_json), _size - 1);
				_configuration->environment_values[_index][_size] = 0;
			} else
				_terminate (_exit_status_invalid_packet_json);
		}
		_configuration->environment_values[_count] = 0;
		_configuration->environment_count = _count;
	} else if (json_is_null (_environment_json)) {
		_count = 0;
		while (environ[_count] != 0)
			_count++;
		_buffer_malloc_block ((void **) &_configuration->environment_values, sizeof (unsigned char *) * (_count + 1), _malloc);
		for (_index = 0; _index < _count; _index++)
			_buffer_malloc_strdup (&_configuration->environment_values[_index], environ[_index], _malloc);
		_configuration->environment_values[_count] = 0;
		_configuration->environment_count = _count;
	} else
		_terminate (_exit_status_invalid_packet_json);
	
	if (json_is_string (_working_directory_json))
		_buffer_malloc_strdup (&_configuration->working_directory, json_string_value (_working_directory_json), _malloc);
	else if (json_is_null (_working_directory_json))
		;
	else
		_terminate (_exit_status_invalid_packet_json);
	
	*_configuration_ = _configuration;
}

static void _process_configuration_destroy (
		struct _process_configuration * * const _configuration_)
{
	struct _process_configuration * _configuration;
	struct _buffer * _malloc;
	_assert (_configuration_ != 0);
	_assert (*_configuration_ != 0);
	_configuration = *_configuration_;
	*_configuration_ = 0;
	_malloc = _configuration->malloc;
	_buffer_deallocate (&_malloc);
}

static void _process_signal_parse (
		enum _process_signal * const _signal,
		json_t * const _json)
{
	json_error_t _json_error;
	signed int _json_outcome;
	char * const _signal_name;
	_assert (_signal != 0);
	_assert (*_signal == 0);
	_assert (_json != 0);
	_json_outcome = json_unpack_ex (_json, &_json_error, JSON_STRICT, "{s:s}", "signal", &_signal_name);
	if (_json_outcome != 0)
		_terminate_with_reason_1 (_exit_status_invalid_packet_json, _json_error.text);
	if (_signal_name == 0)
		*_signal = _process_signal_invalid;
	else if (strcmp (_signal_name, "terminate") == 0)
		*_signal = _process_signal_terminate;
	else if (strcmp (_signal_name, "destroy") == 0)
		*_signal = _process_signal_destroy;
	else
		*_signal = _process_signal_invalid;
}

// ----------------------------------------

static void _packet_parse (
		struct _packet * const _packet)
{
	_assert (_packet != 0);
	_assert (_packet->state == _packet_state_inputed);
	_packet_parse_json (_packet);
	_packet_parse_type (_packet);
	_packet->state = _packet_state_parsed;
}

static void _packet_parse_json (
		struct _packet * const _packet)
{
	struct _buffer _buffer;
	_assert (_packet != 0);
	_assert (_packet->state == _packet_state_inputed);
	_assert (_packet->json == 0);
	_assert (_packet->buffer != 0);
	_assert (_packet->buffer->available == (_packet->size + 4));
	_assert (_packet->buffer->offset == 0);
	_packet->buffer->offset = 4;
	_buffer_parse_json (&_packet->json, _packet->buffer);
}

static void _packet_parse_type (
		struct _packet * const _packet)
{
	json_error_t _json_error;
	signed int _json_outcome;
	char * const _type_name;
	_assert (_packet != 0);
	_assert (_packet->state == _packet_state_inputed);
	_assert (_packet->json != 0);
	_assert (_packet->type == _packet_type_undefined);
	_json_outcome = json_unpack_ex (_packet->json, &_json_error, JSON_STRICT, "{s:s*}", "__type__", &_type_name);
	if (_json_outcome != 0)
		_terminate_with_reason_1 (_exit_status_invalid_packet_type, _json_error.text);
	if (_type_name == 0)
		_packet->type = _packet_type_invalid;
	else if (strcmp (_type_name, "terminate") == 0)
		_packet->type = _packet_type_terminate;
	else if (strcmp (_type_name, "exchange") == 0)
		_packet->type = _packet_type_exchange;
	else if (strcmp (_type_name, "execute") == 0)
		_packet->type = _packet_type_execute;
	else if (strcmp (_type_name, "signal") == 0)
		_packet->type = _packet_type_signal;
	else
		_packet->type = _packet_type_invalid;
	_json_outcome = json_object_del (_packet->json, "__type__");
	_assert (_json_outcome == 0);
}

// ----------------------------------------

static void _packet_streams_poll (
		struct _packet_stream * const * const _streams,
		unsigned int const _timeout,
		unsigned int const _pollable_count,
		unsigned int * const _polled_count)
{
	struct _packet_stream * _stream;
	struct pollfd _pollfds[16];
	struct pollfd * _pollfd;
	signed int _poll_outcome;
	unsigned int _index;
	_assert (_streams != 0);
	_assert (_pollable_count <= 16);
	for (_index = 0; _index < _pollable_count; _index++) {
		_stream = _streams[_index];
		_pollfd = &_pollfds[_index];
		_assert (_stream != 0);
		_assert ((_stream->state == _packet_stream_state_inputing) || (_stream->state == _packet_stream_state_outputing));
		_assert ((_stream->poll_waiting & ~(POLLIN | POLLOUT | POLLHUP | POLLERR)) == 0);
		_assert (_stream->poll_pending == 0);
		_pollfd->fd = _stream->descriptor;
		_pollfd->events = _stream->poll_waiting;
		_pollfd->revents = 0;
	}
	_poll_outcome = poll (_pollfds, _pollable_count, _timeout);
	_enforce (_poll_outcome >= 0);
	for (_index = 0; _index < _pollable_count; _index++) {
		_stream = _streams[_index];
		_pollfd = &_pollfds[_index];
		_assert ((_pollfd->revents & ~(POLLIN | POLLOUT | POLLERR | POLLHUP)) == 0);
		if (_pollfd->revents & (POLLIN | POLLOUT))
			_stream->poll_pending = 1;
		else if (_pollfd->revents & POLLERR)
			_stream->state = _packet_stream_state_failed;
		else if (_pollfd->revents & POLLHUP)
			_stream->state = _packet_stream_state_closed;
		else if (_pollfd->revents == 0)
			_stream->poll_pending = 0;
		else
			_enforce (0);
	}
	*_polled_count = _poll_outcome;
}

// ----------------------------------------

static void _packet_stream_input (
		struct _packet_stream * const _stream)
{
	_assert (_stream != 0);
	_assert (_stream->state == _packet_stream_state_inputing);
	if (_stream->pending == 0) {
		_packet_allocate (&_stream->pending, 1024);
		_stream->pending->state = _packet_state_input_ready;
	}
	_packet_input (_stream->pending, _stream->descriptor);
	switch (_stream->pending->state) {
		case _packet_state_input_waiting_size :
		case _packet_state_input_waiting_data :
			break;
		case _packet_state_inputed :
			_packet_stream_enqueue (_stream, &_stream->pending);
			break;
		case _packet_state_stream_closed :
			_stream->state = _packet_stream_state_closed;
			_packet_deallocate (&_stream->pending);
			break;
		case _packet_state_stream_failed :
			_stream->state = _packet_stream_state_failed;
			_packet_deallocate (&_stream->pending);
			break;
		default :
			_enforce (0);
			break;
	}
}

static void _packet_stream_output (
		struct _packet_stream * const _stream)
{
	_assert (_stream != 0);
	_assert (_stream->state == _packet_stream_state_outputing);
	if (_stream->pending == 0) {
		_packet_stream_dequeue (_stream, &_stream->pending);
		_assert (_stream->pending->state == _packet_state_output_ready);
	}
	_packet_output (_stream->pending, _stream->descriptor);
	switch (_stream->pending->state) {
		case _packet_state_output_waiting :
			break;
		case _packet_state_outputed :
			_packet_deallocate (&_stream->pending);
			break;
		case _packet_state_stream_closed :
			_stream->state = _packet_stream_state_closed;
			_packet_deallocate (&_stream->pending);
			break;
		case _packet_state_stream_failed :
			_stream->state = _packet_stream_state_failed;
			_packet_deallocate (&_stream->pending);
			break;
		default :
			_enforce (0);
			break;
	}
}

// ----------------------------------------

static void _packet_stream_enqueue (
		struct _packet_stream * const _stream,
		struct _packet * * _packet_)
{
	struct _packet * _packet;
	_assert (_stream != 0);
	_assert (_packet_ != 0);
	_assert (*_packet_ != 0);
	_packet = *_packet_;
	_assert ((_stream->state > _packet_stream_state_undefined) && (_stream->state < _packet_stream_state_max));
	_assert ((_packet->state > _packet_state_undefined) && (_packet->state < _packet_state_max));
	_assert (_packet->chained == 0);
	*_packet_ = 0;
	if (_stream->queue_tail == 0) {
		_assert (_stream->queue_head == 0);
		_stream->queue_tail = _packet;
		_stream->queue_head = _packet;
	} else {
		_assert (_stream->queue_head != 0);
		_stream->queue_tail->chained = _packet;
		_stream->queue_tail = _packet;
	}
	_packet->chained = 0;
}

static void _packet_stream_dequeue (
		struct _packet_stream * const _stream,
		struct _packet * * _packet_)
{
	struct _packet * _packet;
	_assert (_stream != 0);
	_assert (_packet_ != 0);
	_assert (*_packet_ == 0);
	_assert ((_stream->state > _packet_stream_state_undefined) && (_stream->state < _packet_stream_state_max));
	_assert (_stream->queue_head != 0);
	_packet = _stream->queue_head;
	_stream->queue_head = _packet->chained;
	if (_stream->queue_head == 0) {
		_assert (_stream->queue_tail == _packet);
		_stream->queue_tail = 0;
	} else
		_assert (_stream->queue_tail != 0);
	_packet->chained = 0;
	*_packet_ = _packet;
}

static void _packet_stream_create (
		struct _packet_stream * * const _stream_,
		unsigned int const _descriptor)
{
	struct _packet_stream * _stream;
	_assert (_stream_ != 0);
	_assert (*_stream_ == 0);
	_stream = malloc (sizeof (struct _packet_stream));
	_enforce (_stream != 0);
	_stream->queue_head = 0;
	_stream->queue_tail = 0;
	_stream->pending = 0;
	_stream->descriptor = _descriptor;
	_stream->poll_waiting = 0;
	_stream->poll_pending = 0;
	_stream->state = _packet_stream_state_initialized;
	*_stream_ = _stream;
}

static void _packet_stream_destroy (
		struct _packet_stream * * const _stream_)
{
	struct _packet_stream * _stream;
	_assert (_stream_ != 0);
	_assert (*_stream_ != 0);
	_stream = *_stream_;
	*_stream_ = 0;
	_assert ((_stream->state > _packet_stream_state_undefined) && (_stream->state < _packet_stream_state_max));
	if (_stream->pending != 0)
		_packet_deallocate (&_stream->pending);
	while (_stream->queue_head != 0) {
		_stream->queue_tail = _stream->queue_head->chained;
		_packet_deallocate (&_stream->queue_head);
		_stream->queue_head = _stream->queue_tail;
	}
	close (_stream->descriptor);
	if (_stream->cleanup != 0)
		_stream->cleanup->action = _cleanup_action_canceled;
	memset (_stream, 0xee, sizeof (struct _packet_stream));
	free (_stream);
}

// ----------------------------------------

static void _packet_input (
		struct _packet * const _packet,
		unsigned int _stream)
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
			_assert (0);
			break;
	}
	_assert (_packet->buffer->offset == _packet->buffer->available);
	_read_outcome = read (_stream, _packet->buffer->data + _packet->buffer->offset, _read_window);
	switch (_read_outcome) {
		case 0 :
			_packet->state = _packet_state_stream_closed;
			break;
		case -1 :
			_packet->state = _packet_state_stream_failed;
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
		case _packet_state_stream_closed :
		case _packet_state_stream_failed :
			break;
		default :
			_assert (0);
			break;
	}
}

static void _packet_output (
		struct _packet * const _packet,
		unsigned int _stream)
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
			_assert (0);
			break;
	}
	_write_window = _packet->buffer->available - _packet->buffer->offset;
	_write_outcome = write (_stream, _packet->buffer->data, _write_window);
	fdatasync (_stream);
	switch (_write_outcome) {
		case 0 :
		case -1 :
			_packet->state = _packet_state_stream_failed;
			break;
		default :
			_assert (_write_outcome > 0);
			_packet->buffer->offset += _write_outcome;
			break;
	}
	if (_packet->buffer->offset == _packet->buffer->available)
		_packet->state = _packet_state_outputed;
}

// ----------------------------------------

static void _packet_allocate (
		struct _packet * * const _packet_,
		unsigned int const _size)
{
	struct _packet * _packet;
	_assert (_packet_ != 0); _assert (*_packet_ == 0); _assert (_size > 0);
	_packet = malloc (sizeof (struct _packet));
	_enforce (_packet != 0);
	memset (_packet, 0x00, sizeof (struct _packet));
	_packet->state = _packet_state_input_ready;
	_packet->type = _packet_type_undefined;
	_buffer_allocate (&_packet->buffer, _size + 4);
	*_packet_ = _packet;
}

static void _packet_deallocate (
		struct _packet * * const _packet_)
{
	struct _packet * _packet;
	_assert (_packet_ != 0);
	_assert (*_packet_ != 0);
	_packet = *_packet_;
	*_packet_ = 0;
	_assert ((_packet->state > _packet_state_undefined) && (_packet->state < _packet_state_max));
	if (_packet->buffer != 0)
		_buffer_deallocate (&_packet->buffer);
	if (_packet->json != 0)
		json_decref (_packet->json);
	memset (_packet, 0xee, sizeof (struct _packet));
	free (_packet);
}

// ----------------------------------------

static void _buffer_parse_json (
		json_t * * const _json,
		struct _buffer * const _buffer)
{
	unsigned int _limit;
	json_t * _json_;
	json_error_t _json_error;
	_assert (_json != 0); _assert (*_json == 0); _assert (_buffer != 0);
	_assert (_buffer->offset < _buffer->available);
	for (_limit = _buffer->offset; ((_limit < _buffer->available) && (_buffer->data[_limit] != '\0')); _limit++);
	if (_limit == _buffer->available)
		_terminate_with_reason_1 (_exit_status_invalid_packet_json, "missing null terminator");
	_json_ = json_loads (_buffer->data + _buffer->offset, 0, &_json_error);
	if (_json_ == 0)
		_terminate_with_reason_1 (_exit_status_invalid_packet_json, _json_error.text);
	*_json = _json_;
}

// ----------------------------------------

static void _buffer_malloc_block (
		void * * const _block_,
		unsigned int _unaligned_size,
		struct _buffer * const _buffer)
{
	void * _block;
	unsigned int _aligned_size;
	_assert (_block_ != 0);
	_assert (*_block_ == 0);
	_assert (_unaligned_size > 0);
	_assert (_buffer != 0);
	_assert (_buffer->data != 0);
	_assert (_buffer->offset <= _buffer->available);
	_assert (_buffer->available == _buffer->capacity);
	_assert ((_buffer->offset & 0x07) == 0);
	if ((_unaligned_size | 0x07) == 0)
		_aligned_size = _unaligned_size;
	else
		_aligned_size = (_unaligned_size | 0x07) + 1;
	_enforce (_aligned_size <= (_buffer->available - _buffer->offset));
	_block = _buffer->data + _buffer->offset;
	_buffer->offset += _aligned_size;
	memset (_block, 0x00, _aligned_size);
	*_block_ = _block;
}

static void _buffer_malloc_strdup (
		unsigned char * * const _destination_,
		unsigned char const * const _source,
		struct _buffer * const _buffer)
{
	unsigned char * _destination;
	unsigned int _size;
	_assert (_destination_ != 0); _assert (*_destination_ == 0); _assert (_source != 0); _assert (_buffer != 0);
	_size = strlen (_source);
	_destination = 0;
	_buffer_malloc_block ((void **) &_destination, _size + 1, _buffer);
	strncpy (_destination, _source, _size);
	_destination[_size] = '\0';
	*_destination_ = _destination;
}

// ----------------------------------------

static void _buffer_allocate (
		struct _buffer * * const _buffer_,
		unsigned int _capacity)
{
	struct _buffer * _buffer;
	_assert (_buffer_ != 0);
	_assert (*_buffer_ == 0);
	_buffer = malloc (((sizeof (struct _buffer) | 0x07) + 1) + ((_capacity | 0x07) + 1));
	_enforce (_buffer != 0);
	memset (_buffer, 0x00, sizeof (struct _buffer));
	_buffer->data = (unsigned char *) _buffer + ((sizeof (struct _buffer) | 0x07) + 1);
	_buffer->capacity = _capacity;
	*_buffer_ = _buffer;
}

static void _buffer_reallocate (
		struct _buffer * * const _buffer_,
		unsigned int _capacity)
{
	struct _buffer * _buffer;
	_assert (_buffer_ != 0);
	_assert (*_buffer_ != 0);
	_buffer = *_buffer_;
	*_buffer_ = 0;
	_assert (_buffer->data != 0);
	_assert (_buffer->offset <= _buffer->available);
	_assert (_buffer->available <= _buffer->capacity);
	_assert (_buffer->capacity < _capacity);
	_buffer = realloc (_buffer, ((sizeof (struct _buffer) | 0x07) + 1) + ((_capacity | 0x07) + 1));
	_enforce (_buffer != 0);
	_buffer->data = (unsigned char *) _buffer + ((sizeof (struct _buffer) | 0x07) + 1);
	_buffer->capacity = _capacity;
	*_buffer_ = _buffer;
}

static void _buffer_deallocate (
		struct _buffer * * const _buffer_)
{
	struct _buffer * _buffer;
	_assert (_buffer_ != 0);
	_assert (*_buffer_ != 0);
	_buffer = *_buffer_;
	*_buffer_ = 0;
	_assert (_buffer->data != 0);
	_assert (_buffer->offset <= _buffer->available);
	_assert (_buffer->available <= _buffer->capacity);
	memset (_buffer, 0xee, sizeof (struct _buffer));
	free (_buffer);
}

// ----------------------------------------

static void __terminate (
		enum _exit_status const _exit_status,
		unsigned char const * const _source_file,
		unsigned int const _source_line)
{
	if (TRACE_SOURCE)
		fprintf (stderr, "[%5u][!!] terminated with %d @@ `%s`/%d\n", _pid, _exit_status, _source_file, _source_line);
	else
		fprintf (stderr, "[%5u][!!] terminated with %d\n", _pid, _exit_status);
	fflush (stderr);
	_cleanup_execute ();
	exit (_exit_status);
}

static void __terminate_with_reason_1 (
		enum _exit_status const _exit_status,
		unsigned char const * const _source_file,
		unsigned int const _source_line,
		unsigned char const * const _reason)
{
	if (TRACE_SOURCE)
		fprintf (stderr, "[%5u][!!] terminated with %d: %s @@ `%s`/%d\n", _pid, _exit_status, _reason, _source_file, _source_line);
	else
		fprintf (stderr, "[%5u][!!] terminated with %d: %s\n", _pid, _exit_status, _reason);
	fflush (stderr);
	_cleanup_execute ();
	exit (_exit_status);
}

static void __terminate_with_reason_2 (
		enum _exit_status const _exit_status,
		unsigned char const * const _source_file,
		unsigned int const _source_line,
		unsigned char const * const _reason_1,
		unsigned char const * const _reason_2)
{
	if (TRACE_SOURCE)
		fprintf (stderr, "[%5u][!!] terminated with %d: %s / %s @@ `%s`/%d\n", _pid, _exit_status, _reason_1, _reason_2, _source_file, _source_line);
	else
		fprintf (stderr, "[%5u][!!] terminated with %d: %s / %s\n", _pid, _exit_status, _reason_1, _reason_2);
	fflush (stderr);
	_cleanup_execute ();
	exit (_exit_status);
}

static void __enforce (
		unsigned long int _condition_value,
		unsigned char const * const _condition_expression,
		unsigned char const * const _source_file,
		unsigned int const _source_line)
{
	if (!_condition_value) {
		fprintf (stderr, "[%5u][!!] enforcement failed for `%s` @@ `%s`/%d\n", _pid, _condition_expression, _source_file, _source_line);
		fflush (stderr);
		_cleanup_execute ();
		exit (_exit_status_failed_assertion);
	}
}

static void __assert (
		unsigned long int _condition_value,
		unsigned char const * const _condition_expression,
		unsigned char const * const _source_file,
		unsigned int const _source_line)
{
	if (!_condition_value) {
		fprintf (stderr, "[%5u][!!] assertion failed for `%s` @@ `%s`/%d\n", _pid, _condition_expression, _source_file, _source_line);
		fflush (stderr);
		_cleanup_execute ();
		exit (_exit_status_failed_assertion);
	}
}

static void __trace (
		enum _trace_event const _event,
		unsigned char const * const _message,
		unsigned char const * const _source_file,
		unsigned int const _source_line)
{
	switch (_event) {
		case _trace_event_debugging :
			if (TRACE_SOURCE)
				fprintf (stderr, "[%5u][  ] %s @@ `%s`/%d\n", _pid, _message, _source_file, _source_line);
			else
				fprintf (stderr, "[%5u][  ] %s\n", _pid, _message);
			fflush (stderr);
			break;
		case _trace_event_information :
			if (TRACE_SOURCE)
				fprintf (stderr, "[%5u][ii] %s @@ `%s`/%d\n", _pid, _message, _source_file, _source_line);
			else
				fprintf (stderr, "[%5u][ii] %s\n", _pid, _message);
			fflush (stderr);
			break;
		case _trace_event_warning :
			if (TRACE_SOURCE)
				fprintf (stderr, "[%5u][ww] %s @@ `%s`/%d\n", _pid, _message, _source_file, _source_line);
			else
				fprintf (stderr, "[%5u][ww] %s\n", _pid, _message);
			fflush (stderr);
			break;
		case _trace_event_error :
			if (TRACE_SOURCE)
				fprintf (stderr, "[%5u][ee] %s @@ `%s`/%d\n", _pid, _message, _source_file, _source_line);
			else
				fprintf (stderr, "[%5u][ee] %s\n", _pid, _message);
			fflush (stderr);
			break;
		default :
			fprintf (stderr, "[%5u][??] %s @@ `%s`/%d\n", _pid, _message, _source_file, _source_line);
			fflush (stderr);
			break;
	}
}

// ----------------------------------------

static struct _cleanup _cleanup_closures[1024];
static unsigned int _cleanup_closures_count = 0;

static void _cleanup_register (
		struct _cleanup * * const _cleanup_)
{
	_assert (_cleanup_ != 0);
	_assert (*_cleanup_ == 0);
	_enforce (_cleanup_closures_count < (sizeof (_cleanup_closures) / sizeof (struct _cleanup)));
	memset (&_cleanup_closures[_cleanup_closures_count], 0x00, sizeof (struct _cleanup));
	_cleanup_closures[_cleanup_closures_count].action = _cleanup_action_canceled;
	*_cleanup_ = &_cleanup_closures[_cleanup_closures_count];
	_cleanup_closures_count++;
}

static void _cleanup_register_close (
		struct _cleanup * * const _cleanup_,
		unsigned int const _descriptor)
{
	struct _cleanup * _cleanup;
	_cleanup_register (_cleanup_);
	_cleanup = *_cleanup_;
	_cleanup->action = _cleanup_action_close;
	_cleanup->arguments.close.descriptor = _descriptor;
}

static void _cleanup_register_kill (
		struct _cleanup * * const _cleanup_,
		unsigned int const _descriptor)
{
	struct _cleanup * _cleanup;
	_cleanup_register (_cleanup_);
	_cleanup = *_cleanup_;
	_cleanup->action = _cleanup_action_kill;
	_cleanup->arguments.kill.descriptor = _descriptor;
}

static void _cleanup_execute (void)
{
	signed int _outcome;
	unsigned int _index;
	unsigned int _timeout;
	struct _cleanup * _cleanup;
	for (_index = 0; _index < _cleanup_closures_count; _index++) {
		_cleanup = &_cleanup_closures[_index];
		switch (_cleanup->action) {
			case _cleanup_action_canceled :
				break;
			case _cleanup_action_close :
				fprintf (stderr, "[%5u][!!] executing cleanup action `close` for %d...\n", _pid, _cleanup->arguments.close.descriptor);
				fflush (stderr);
				_outcome = close (_cleanup->arguments.close.descriptor);
				if (_outcome != 0) {
					fprintf (stderr, "[%5u][!!]  -> `close` failed with %d: %s...\n", _pid, errno, strerror (errno));
					fflush (stderr);
				}
				break;
			case _cleanup_action_kill :
				fprintf (stderr, "[%5u][!!] executing cleanup action `kill` for %d...\n", _pid, _cleanup->arguments.kill.descriptor);
				fflush (stderr);
				_outcome = waitpid (_cleanup->arguments.kill.descriptor, 0, WNOHANG);
				if (_outcome == _cleanup->arguments.kill.descriptor)
					;
				else if (_outcome == 0) {
					_outcome = kill (_cleanup->arguments.kill.descriptor, SIGTERM);
					if (_outcome != 0) {
						fprintf (stderr, "[%5u][!!]  -> `kill (SIGTERM)` failed with %d: %s...\n", _pid, errno, strerror (errno));
						fflush (stderr);
					}
					for (_timeout = 0; _timeout <= _timeout_waitpid; _timeout += _timeout_slice) {
						_outcome = waitpid (_cleanup->arguments.kill.descriptor, 0, WNOHANG);
						if (_outcome == _cleanup->arguments.kill.descriptor)
							break;
						else if (_outcome == 0)
							;
						else {
							fprintf (stderr, "[%5u][!!]  -> `wait` failed with %d: %s...\n", _pid, errno, strerror (errno));
							fflush (stderr);
							break;
						}
						usleep (_timeout_slice);
					}
					if (_outcome != _cleanup->arguments.kill.descriptor) {
						_outcome = kill (_cleanup->arguments.kill.descriptor, SIGKILL);
						if (_outcome != 0) {
							fprintf (stderr, "[%5u][!!]  -> `kill (SIGKILL)` failed with %d: %s...\n", _pid, errno, strerror (errno));
							fflush (stderr);
						}
						waitpid (_cleanup->arguments.kill.descriptor, 0, WNOHANG);
					}
				} else {
					fprintf (stderr, "[%5u][!!]  -> `wait` failed with %d: %s...\n", _pid, errno, strerror (errno));
					fflush (stderr);
				}
				break;
			default :
				fprintf (stderr, "[%5u][!!] invalid cleanup action %d...\n", _pid, _cleanup->action);
				fflush (stderr);
				break;
		}
	}
	_cleanup_closures_count = 0;
}

static void _cleanup_cancel (void)
{
	_cleanup_closures_count = 0;
}

// ----------------------------------------

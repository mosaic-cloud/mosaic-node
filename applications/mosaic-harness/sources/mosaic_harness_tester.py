
## ----------------------------------------

if __name__ != "__main__" :
	raise Exception ()

## ----------------------------------------

_trace_level = 2 # 1 -> debugging; 2 -> information; 3 -> warning; 4 -> error

## ----------------------------------------

def _backend_parrot (_identifier) :
	while True :
		_inbound_packet = _input ()
		if _inbound_packet is None : break
		_inbound_message, _inbound_payload = _inbound_packet
		if _inbound_message["__type__"] != "exchange" : raise Exception ()
		_action = _inbound_message["action"]
		if _action == "call" :
			_operation = _inbound_message["operation"]
			_correlation = _inbound_message["correlation"]
			_inputs = _inbound_message["inputs"]
		elif _action == "cast" :
			_operation = _inbound_message["operation"]
			_inputs = _inbound_message["inputs"]
		else :
			raise Exception ()
		if _operation == "call-return" :
			_outbound_message = {
					"__type__" : "exchange",
					"action" : "call-return",
					"correlation" : _correlation,
					"ok" : True,
					"outputs" : _inputs}
		elif _operation == "cast-mirror" :
			_outbound_message = {
					"__type__" : "exchange",
					"action" : "cast",
					"component" : "0" * 40,
					"operation" : "cast-mirror",
					"inputs" : _inputs}
		else :
			raise Exception ()
		_trace_information ("executing `%s`...", _operation)
		_outbound_payload = _inbound_payload
		_outbound_packet = (_outbound_message, _outbound_payload)
		_output (_outbound_packet)
	_output_close ()
	_input_close ()

def _backend_abacus (_identifier) :
	import operator
	_operators = {
			"+" : operator.__add__,
			"-" : operator.__sub__,
			"*" : operator.__mul__,
			"/" : operator.__div__}
	while True :
		_inbound_packet = _input ()
		if _inbound_packet is None : break
		_inbound_message, _inbound_payload = _inbound_packet
		if _inbound_message["__type__"] != "exchange" : raise Exception ()
		if _inbound_message["action"] != "call" : raise Exception ()
		_operation = _inbound_message["operation"]
		_correlation = _inbound_message["correlation"]
		_inputs = _inbound_message["inputs"]
		if _operation not in _operators : raise Exception ()
		if _inbound_payload != "" : raise Exception ()
		_operator = _operators[_operation]
		_operands = map (float, _inputs)
		_trace_information ("executing `%s` with `%s`...", _operation, _operands)
		_outcome = None
		for _operand in _operands :
			if _outcome is None :
				_outcome = _operand
			else :
				_outcome = _operator (_outcome, _operand)
		_outbound_message = {
				"__type__" : "exchange",
				"action" : "call-return",
				"correlation" : _correlation,
				"ok" : True,
				"outputs" : _outcome}
		_outbound_payload = ""
		_outbound_packet = (_outbound_message, _outbound_payload)
		_output (_outbound_packet)
	_output_close ()
	_input_close ()

_backend_scenarios = {
		"parrot" : _backend_parrot,
		"abacus" : _backend_abacus,
}

## ----------------------------------------

def _frontend_python_parrot () :
	_output (({
			"__type__" : "execute",
			"executable" : sys.executable,
			"argument0" : None,
			"arguments" : [sys.argv[0], "backend", "parrot", "identifier"],
			"environment" : None,
			"working-directory" : None,
	}, ""))
	for i in xrange (0, 10) :
		_outbound_message = {
				"__type__" : "exchange",
				"action" : "call",
				"operation" : "echo",
				"correlation" : str (i),
				"inputs" : i}
		_outbound_payload = str (i) * str (i)
		_outbound_packet = (_outbound_message, _outbound_payload)
		_output (_outbound_packet)
		_inbound_packet = _input ()
		if _inbound_packet is None : raise Exception ()
		_inbound_message, _inbound_payload = _inbound_packet
		if _inbound_message["__type__"] != "exchange" : raise Exception ()
		if _inbound_message["action"] != "call-return" : raise Exception ()
		if _inbound_message["correlation"] != str (i) : raise Exception ()
		if _inbound_message["ok"] != True : raise Exception ()
		if _inbound_message["outputs"] != i : raise Exception ()
		if _inbound_payload != _outbound_payload : raise Exception ()
	_output_close ()
	_inbound_packet = _input ()
	if _inbound_packet is None : raise Exception ()
	_inbound_message, _inbound_payload = _inbound_packet
	if _inbound_message["__type__"] != "exit" : raise Exception ()
	if _inbound_message["exit-status"] != 0 : raise Exception ()
	if _inbound_payload != "" : raise Exception ()
	_input_close ()

def _frontend_python_abacus () :
	_output (({
			"__type__" : "execute",
			"executable" : sys.executable,
			"argument0" : None,
			"arguments" : [sys.argv[0], "backend", "abacus", "identifier"],
			"environment" : None,
			"working-directory" : None,
	}, ""))
	__frontend_abacus ()

def _frontend_node_abacus () :
	_output (({
			"__type__" : "execute",
			"executable" : "/usr/bin/node",
			"argument0" : None,
			"arguments" : ["./mosaic_component_abacus.js"],
			"environment" : None,
			"working-directory" : "./applications/mosaic-component/sources",
	}, ""))
	__frontend_abacus ()

def _frontend_java_abacus () :
	_output (({
			"__type__" : "execute",
			"executable" : "/usr/bin/java",
			"argument0" : None,
			"arguments" : [
				"-jar",
				"../mosaic-java-components/components-container/target/components-container-0.2-SNAPSHOT-jar-with-dependencies.jar",
				"eu.mosaic_cloud.components.examples.abacus.AbacusComponentCallbacks",
				"file:../mosaic-java-components/components-examples/target/components-examples-0.2-SNAPSHOT.jar"],
			"environment" : None,
			"working-directory" : None,
	}, ""))
	__frontend_abacus ()

def __frontend_abacus () :
	for i in xrange (0, 10) :
		_outbound_message = {
				"__type__" : "exchange",
				"action" : "call",
				"operation" : "+",
				"correlation" : str (i),
				"inputs" : [i * 2, i * 3]}
		_outbound_payload = ""
		_outbound_packet = (_outbound_message, _outbound_payload)
		_output (_outbound_packet)
		_inbound_packet = _input ()
		if _inbound_packet is None :raise Exception ()
		_inbound_message, _inbound_payload = _inbound_packet
		if _inbound_message["__type__"] != "exchange" : raise Exception ()
		if _inbound_message["action"] != "call-return" : raise Exception ()
		if _inbound_message["correlation"] != str (i) : raise Exception ()
		if _inbound_message["ok"] != True : raise Exception ()
		if _inbound_message["outputs"] != i * 5 : raise Exception ()
		if _inbound_payload != "" : raise Exception ()
	_output_close ()
	_inbound_packet = _input ()
	if _inbound_packet is None : raise Exception ()
	_inbound_message, _inbound_payload = _inbound_packet
	if _inbound_message["__type__"] != "exit" : raise Exception ()
	if _inbound_message["exit-status"] != 0 : raise Exception ()
	if _inbound_payload != "" : raise Exception ()
	_input_close ()

def _frontend_test_execute_1 () :
	_output (({
			"__type__" : "execute",
			"executable" : "/bin/sleep",
			"argument0" : None,
			"arguments" : ["60s"],
			"environment" : None,
			"working-directory" : None,
	}, ""))
	_sleep (0.1)
	_output (({
			"__type__" : "signal",
			"signal" : "terminate",
	}, ""))
	_sleep (0.1)
	_inbound_packet = _input ()
	if _inbound_packet is None : raise Exception ()
	_inbound_message, _inbound_payload = _inbound_packet
	if _inbound_message["__type__"] != "exit" : raise Exception ()
	if _inbound_message["exit-status"] != 0 : raise Exception ()
	if _inbound_payload != "" : raise Exception ()
	_output_close ()
	_input_close ()

def _frontend_test_execute_2 () :
	_output (({
			"__type__" : "execute",
			"executable" : "/bin/sleep",
			"argument0" : None,
			"arguments" : ["60s"],
			"environment" : None,
			"working-directory" : None,
	}, ""))
	_sleep (0.1)
	_output_close ()
	_input_close ()

def _frontend_test_execute_3 () :
	for i in xrange (0, 10) :
		_output (({
				"__type__" : "execute",
				"executable" : "/bin/sleep",
				"argument0" : None,
				"arguments" : ["60s"],
				"environment" : None,
				"working-directory" : None,
		}, ""))
		_sleep (0.1)
		_output (({
				"__type__" : "signal",
				"signal" : "terminate",
		}, ""))
		_sleep (0.1)
		_inbound_packet = _input ()
		if _inbound_packet is None : raise Exception ()
		_inbound_message, _inbound_payload = _inbound_packet
		if _inbound_message["__type__"] != "exit" : raise Exception ()
		if _inbound_message["exit-status"] != 15 : raise Exception ()
		if _inbound_payload != "" : raise Exception ()
	_output_close ()
	_input_close ()

def _frontend_test_exchange_1 () :
	for i in xrange (0, 10) :
		_outbound_packet = ({
				"__type__" : "exchange",
				"data" : i,
		}, "")
		_output (_packet)
	_output_close ()
	_input_close ()

def _frontend_test_exchange_2 () :
	_output (({
			"__type__" : "execute",
			"executable" : "/bin/sleep",
			"argument0" : None,
			"arguments" : ["60s"],
			"environment" : None,
			"working-directory" : None,
	}, ""))
	for i in xrange (0, 10) :
		_outbound_packet = ({
				"__type__" : "exchange",
				"data" : i,
		}, "")
		_output (_outbound_packet)
		_inbound_packet = _input ()
		if _inbound_packet is None : raise Exceptio ()
		if _inbound_packet != _outbound_packet : raise Exception ()
	_output_close ()
	_inbound_packet = _input ()
	if _inbound_packet is None : raise Exception ()
	_inbound_message, _inbound_payload = _inbound_packet
	if _inbound_message["__type__"] != "exit" : raise Exception ()
	if _inbound_message["exit-status"] != 0 : raise Exception ()
	if _inbound_payload != "" : raise Exception ()
	_input_close ()

def _frontend_test_rabbitmq () :
	_output (({
			"__type__" : "execute",
			"executable" : "../mosaic-components-rabbitmq/scripts/run-node",
			"argument0" : None,
			"arguments" : None,
			"environment" : None,
			"working-directory" : None,
	}, ""))
	_sleep (0.1)
	_inbound_packet = _input ()
	if _inbound_packet is None : raise Exception ()
	_inbound_message, _inbound_payload = _inbound_packet
	if _inbound_message["__type__"] != "resources" : raise Exception ()
	if _inbound_payload != "" : raise Exception ()
	if _inbound_message["action"] != "acquire" : raise Exception ()
	_correlation = _inbound_message["correlation"]
	_specifications = _inbound_message["specifications"]
	if _specifications != {"broker_socket" : "socket:ipv4:tcp", "management_socket" : "socket:ipv4:tcp"} : raise Exception ()
	_outbound_message = {
			"__type__" : "resources",
			"action" : "acquire-return",
			"correlation" : _correlation,
			"ok" : True,
			"descriptors" : {
				"broker_socket" : {
					"ip" : "127.0.0.1",
					"port" : 15672,
				},
				"management_socket" : {
					"ip" : "127.0.0.1",
					"port" : 15673,
				}}}
	_outbound_payload = ""
	_outbound_packet = (_outbound_message, _outbound_payload)
	_output (_outbound_packet)
	_inbound_packet = _input ()
	if _inbound_packet == None : raise Exception ()
	_inbound_message, _inbound_data = _inbound_packet
	if _inbound_message["__type__"] != "exchange" : raise Exception ()
	if _inbound_message["action"] != "register" : raise Exception ()
	_group = _inbound_message["group"]
	_correlation = _inbound_message["correlation"]
	if _group == "" : raise Exception ()
	_outbound_message = {
			"__type__" : "exchange",
			"action" : "register-return",
			"correlation" : _correlation,
			"ok" : True}
	_outbound_payload = ""
	_outbound_packet = (_outbound_message, _outbound_payload)
	_output (_outbound_packet)
	_sleep (6)
	_output_close ()
	_inbound_packet = _input ()
	if _inbound_packet is None : raise Exception ()
	_inbound_message, _inbound_payload = _inbound_packet
	if _inbound_message["__type__"] != "exit" : raise Exception ()
	if _inbound_message["exit-status"] != 0 : raise Exception ()
	if _inbound_payload != "" : raise Exception ()
	_input_close ()

_frontend_scenarios = {
		"test-execute-1" : _frontend_test_execute_1,
		"test-execute-2" : _frontend_test_execute_2,
		"test-execute-3" : _frontend_test_execute_3,
		"test-exchange-1" : _frontend_test_exchange_1,
		"test-exchange-2" : _frontend_test_exchange_2,
		"python-parrot" : _frontend_python_parrot,
		"python-abacus" : _frontend_python_abacus,
		"node-abacus" : _frontend_node_abacus,
		"java-abacus" : _frontend_java_abacus,
		"test-rabbitmq" : _frontend_test_rabbitmq,
}

## ----------------------------------------

def _backend (_scenario, _identifier) :
	
	global _input_stream
	global _output_stream
	
	_trace_information ("executing backend scenario `%s`...", _scenario)
	_scenario = _backend_scenarios[_scenario]
	
	_input_stream = sys.stdin
	_output_stream = sys.stdout
	
	_scenario (_identifier)

def _frontend (_scenario) :
	
	global _input_stream
	global _output_stream
	
	_trace_information ("executing frontend scenario `%s`...", _scenario)
	_scenario = _frontend_scenarios[_scenario]
	
	_trace_debugging ("starting harness...")
	if True :
		_process = subprocess.Popen (
				["[mosaic_harness]"],
				executable = "./.outputs/gcc/applications-elf/mosaic_harness.elf",
				stdin = subprocess.PIPE, stdout = subprocess.PIPE)
	else :
		_process = subprocess.Popen (
				["[mosaic_harness_strace]", "-e", "trace=file,desc,process", "--", "./.outputs/gcc/applications-elf/mosaic_harness.elf"],
				executable = "/usr/bin/strace",
				stdin = subprocess.PIPE, stdout = subprocess.PIPE)
	
	_input_stream = _process.stdout
	_output_stream = _process.stdin
	
	_scenario ()
	
	_trace_debugging ("waiting harness...")
	_process.wait ()

## ----------------------------------------

import json
import os
import struct
import subprocess
import sys
import time

## ----------------------------------------

def _input () :
	_size = _input_stream.read (4)
	if _size == "" :
		return None
	(_size,) = struct.unpack (">l", _size)
	_data = _input_stream.read (_size)
	_split = _data.find ("\0")
	if _split == -1 :
		raise Exception ()
	_message = _data[:_split]
	_payload = _data[_split+1:]
	_message = _message.decode ("utf-8")
	_message = json.loads (_message)
	_packet = (_message, _payload)
	_trace_debugging ("received `%s`;", _packet)
	return _packet

def _input_close () :
	_trace_debugging ("closing receive...")
	_input_stream.close ()

def _output (_packet) :
	_trace_debugging ("sending `%s`...", _packet)
	_message, _payload = _packet
	_message = json.dumps (_message)
	_message = _message.encode ("utf-8")
	_data = _message + "\0" + _payload
	_size = len (_data)
	_size = struct.pack (">l", _size)
	_output_stream.write (_size + _data)
	_output_stream.flush ()

def _output_close () :
	_trace_debugging ("closing send...")
	_output_stream.close ()

## ----------------------------------------

def _trace_error (_format, *_parts) :
	if _trace_level <= 4 :
		print >> sys.stderr, "[%5d][ee] %s" % (os.getpid (), _format % _parts)

def _trace_warning (_format, *_parts) :
	if _trace_level <= 3 :
		print >> sys.stderr, "[%5d][ww] %s" % (os.getpid (), _format % _parts)

def _trace_information (_format, *_parts) :
	if _trace_level <= 2 :
		print >> sys.stderr, "[%5d][ii] %s" % (os.getpid (), _format % _parts)

def _trace_debugging (_format, *_parts) :
	if _trace_level <= 1 :
		print >> sys.stderr, "[%5d][dd] %s" % (os.getpid (), _format % _parts)

## ----------------------------------------

def _sleep (_timeout) :
	_trace_debugging ("sleeping...")
	time.sleep (_timeout)

## ----------------------------------------

def _main () :
	
	global _harness_input
	global _harness_output
	
	if len (sys.argv) < 2 :
		raise Exception ()
	_behaviour = sys.argv[1]
	if _behaviour == "backend" :
		if len (sys.argv) != 4 :
			raise Exception ()
		_scenario = sys.argv[2]
		_identifier = sys.argv[3]
		_behaviour = lambda : _backend (_scenario, _identifier)
	elif _behaviour == "frontend" :
		if len (sys.argv) != 3 :
			raise Exception ()
		_scenario = sys.argv[2]
		_behaviour = lambda : _frontend (_scenario)
	else :
		raise Exception ()
	
	_behaviour ()
	
	sys.exit (0)

_main ()

## ----------------------------------------


## ----------------------------------------

if __name__ != "__main__" :
	raise Exception ()

## ----------------------------------------

_trace_level = 2 # 1 -> debugging; 2 -> information; 3 -> warning; 4 -> error

## ----------------------------------------

def _backend_parrot (_identifier) :
	while True :
		_packet = _input ()
		if _packet is None :
			break
		_message, _payload = _packet
		if _message["__type__"] != "exchange" :
			raise Exception ()
		elif _message["action"] == "call" :
			_message["action"] = "return"
			_output (_packet)
		elif _message["action"] == "cast" :
			_message["component"] = _identifier
			_output (_packet)
		else :
			raise Exception ()
	_output_close ()
	_input_close ()

def _backend_abacus (_identifier) :
	while True :
		_packet = _input ()
		if _packet is None :
			break
		_message, _payload = _packet
		if _message["__type__"] != "exchange" :
			raise Exception ()
		elif _message["action"] == "call" :
			if _payload != "" :
				raise Exception ()
			_correlation = _message["correlation"]
			_request = _message["meta-data"]
			_operator = _request["operator"]
			_operands = _request["operands"]
			if _operator == "+" :
				(_first_operand, _second_operand) = _operands
				_outcome = float (_first_operand) + float (_second_operand)
			else :
				raise Exception ()
			_packet = ({
				"__type__" : "exchange",
				"action" : "return",
				"correlation" : _correlation,
				"meta-data" : {
					"ok" : True,
					"outcome" : _outcome,
				},
			}, "")
			_output (_packet)
		else :
			raise Exception ()
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
	_sleep (0.1)
	for i in xrange (0, 10) :
		_output_message = {
				"__type__" : "exchange",
				"action" : "call",
				"correlation" : str (i),
				"meta-data" : i,
		}
		_output_payload = ""
		_output_packet = (_output_message, _output_payload)
		_output (_output_packet)
		_input_packet = _input ()
		if _input_packet is None :
			raise Exception ()
		_input_message, _input_payload = _input_packet
		if _input_message["__type__"] != "exchange" :
			raise Exception ()
		if _input_message["action"] != "return" :
			raise Exception ()
		if _input_message["correlation"] != str (i) :
			raise Exception ()
		if _input_message["meta-data"] != i :
			raise Exception ()
	_output_close ()
	_input_packet = _input ()
	if _input_packet is None :
		raise Exception ()
	_input_message, _input_payload = _input_packet
	if _input_message["__type__"] != "exit" :
		raise Exception ()
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
	_sleep (0.1)
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
	_sleep (0.1)
	__frontend_abacus ()

def _frontend_node_abacus () :
	_output (({
			"__type__" : "execute",
			"executable" : "/usr/bin/node",
			"argument0" : None,
			"arguments" : ["./mosaic_component_abacus.js"],
			"environment" : None,
			"working-directory" : "./applications/mosaic-cluster/sources",
	}, ""))
	_sleep (0.1)
	__frontend_abacus ()

def __frontend_abacus () :
	for i in xrange (0, 10) :
		_output_message = {
				"__type__" : "exchange",
				"action" : "call",
				"correlation" : str (i),
				"meta-data" : {
					"operator" : "+",
					"operands" : [i * 2, i * 3],
				},
		}
		_output_payload = ""
		_output_packet = (_output_message, _output_payload)
		_output (_output_packet)
		_input_packet = _input ()
		if _input_packet is None :
			raise Exception ()
		_input_message, _input_payload = _input_packet
		if _input_message["__type__"] != "exchange" :
			raise Exception ()
		if _input_message["action"] != "return" :
			raise Exception ()
		if _input_message["correlation"] != str (i) :
			raise Exception ()
		if _input_message["meta-data"]["outcome"] != i * 5 :
			raise Exception ()
	_output_close ()
	_input_packet = _input ()
	if _input_packet is None :
		raise Exception ()
	_input_message, _input_payload = _input_packet
	if _input_message["__type__"] != "exit" :
		raise Exception ()
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
	_input_packet = _input ()
	if _input_packet is None :
		raise Exception ()
	_input_message, _input_payload = _input_packet
	if _input_message["__type__"] != "exit" :
		raise Exception ()
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
		_input_packet = _input ()
		if _input_packet is None :
			raise Exception ()
		_input_message, _input_payload = _input_packet
		if _input_message["__type__"] != "exit" :
			raise Exception ()
	_output_close ()
	_input_close ()

def _frontend_test_exchange () :
	for i in xrange (0, 10) :
		_output (({
				"__type__" : "exchange",
				"index" : i,
		}, ""))
		_sleep (0.1)
	_output_close ()
	_input_close ()

def _frontend_test_nodejs () :
	_output (({
			"__type__" : "execute",
			"executable" : "/usr/bin/node",
			"argument0" : None,
			"arguments" : ["./mosaic_component_abacus.js"],
			"environment" : None,
			"working-directory" : "./applications/mosaic-cluster/sources",
	}, ""))
	_sleep (0.1)
	for i in xrange (0, 10) :
		_output_message = {
				"__type__" : "exchange",
				"index" : i,
		}
		_output_payload = ""
		_output_packet = (_output_message, _output_payload)
		_output (_output_packet)
		_input_packet = _input ()
		if _input_packet != _output_packet :
			raise Exception ()
		_sleep (0.1)
	_output_close ()
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
	_input_packet = _input ()
	if _input_packet is None :
		raise Exception ()
	_input_meta_data, _input_data = _input_packet
	if _input_meta_data["__type__"] != "resources" :
		raise Exception ()
	if _input_data != "" :
		raise Exception ()
	if _input_meta_data["action"] != "acquire" :
		raise Exception ()
	_acquire_correlation = _input_meta_data["correlation"]
	_acquire_resources = _input_meta_data["resources"]
	if _acquire_resources != {"broker_socket" : "socket:ipv4:tcp", "management_socket" : "socket:ipv4:tcp"} :
		raise Exception ()
	_output (({
			"__type__" : "resources",
			"action" : "return",
			"correlation" : _acquire_correlation,
			"ok" : True,
			"resources" : {
				"broker_socket" : {
					"ip" : "127.0.0.1",
					"port" : 15672,
				},
				"management_socket" : {
					"ip" : "127.0.0.1",
					"port" : 15673,
				},
			},
	}, ""))
	_input_packet = _input ()
	if _input_packet == None :
		raise Exception ()
	_input_meta_data, _input_data = _input_packet
	if _input_meta_data["__type__"] != "exit" :
		raise Exception ()
	_output_close ()
	_input_close ()

_frontend_scenarios = {
		"python-parrot" : _frontend_python_parrot,
		"python-abacus" : _frontend_python_abacus,
		"java-abacus" : _frontend_java_abacus,
		"node-abacus" : _frontend_node_abacus,
		"test-execute-1" : _frontend_test_execute_1,
		"test-execute-2" : _frontend_test_execute_2,
		"test-execute-3" : _frontend_test_execute_3,
		"test-exchange" : _frontend_test_exchange,
		"test-nodejs" : _frontend_test_nodejs,
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
				["[mosaic_component_harness]"],
				executable = "./.outputs/gcc/applications-elf/mosaic_component_harness.elf",
				stdin = subprocess.PIPE, stdout = subprocess.PIPE)
	else :
		_process = subprocess.Popen (
				["[mosaic_component_harness_strace]", "-e", "trace=file,desc,process", "--", "./.outputs/gcc/applications-elf/mosaic_component_harness.elf"],
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
	_trace_debugging ("received: `%s`;", _packet)
	return _packet

def _input_close () :
	_trace_debugging ("closing receive...")
	_input_stream.close ()

def _output (_packet) :
	_trace_debugging ("sending: `%s`...", _packet)
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

def _sleep (_timeout) :
	_trace_debugging ("sleeping...")
	time.sleep (_timeout)

def _trace_information (_format, *_parts) :
	if _trace_level <= 2 :
		print >> sys.stderr, "[%5d][ii] %s" % (os.getpid (), _format % _parts)

def _trace_debugging (_format, *_parts) :
	if _trace_level <= 1 :
		print >> sys.stderr, "[%5d][dd] %s" % (os.getpid (), _format % _parts)

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

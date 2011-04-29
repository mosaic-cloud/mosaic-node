
## ----------------------------------------

if __name__ != "__main__" :
	raise Exception ()

## ----------------------------------------

def _test_1 () :
	for i in xrange (0, 10) :
		_output ({
				"__type__" : "exchange",
				"index" : i,
		})
		_sleep (0.1)
	_output_close ()
	_input_close ()

def _test_2 () :
	_output ({
			"__type__" : "execute",
			"executable" : "/bin/sleep",
			"arguments" : ["[mosaic_component_harness_subprocess]", "60"],
			"environment" : None,
			"working-directory" : None,
	})
	_sleep (0.1)
	for i in xrange (0, 10) :
		_output ({
				"__type__" : "exchange",
				"index" : i,
		})
		_sleep (0.1)
	_output ({
			"__type__" : "signal",
			"signal" : "terminate",
	})
	_sleep (0.1)
	print >> sys.stderr, _input ()
	_output_close ()
	_input_close ()

def _test_3 () :
	_output ({
			"__type__" : "execute",
			"executable" : "/bin/sleep",
			"arguments" : ["[mosaic_component_harness_subprocess]", "60"],
			"environment" : None,
			"working-directory" : None,
	})
	_output_close ()
	_input_close ()

def _nodejs () :
	_output ({
			"__type__" : "execute",
			"executable" : "/usr/bin/node",
			"arguments" : ["[mosaic_component_harness_subprocess]", "./applications/mosaic-cluster/sources/mosaic_component_harness.js"],
			"environment" : None,
			"working-directory" : None,
	})
	for i in xrange (0, 10) :
		_output ({
				"__type__" : "exchange",
				"index" : i,
		})
		_sleep (0.1)
	print >> sys.stderr, _input ()
	_output_close ()
	_input_close ()

_scenarios = {
	"test-1" : _test_1,
	"test-2" : _test_2,
	"test-3" : _test_3,
	"nodejs" : _nodejs,
}

## ----------------------------------------

import json
import struct
import subprocess
import sys
import time

## ----------------------------------------

def _input () :
	_header = _harness_input.read (4)
	(_size,) = struct.unpack (">l", _header)
	print >> sys.stderr, _size
	_data = _harness_input.read (_size)
	_message = _data
	_message = _message.decode ("utf-8")
	_object = json.loads (_message)
	return _message

def _input_close () :
	_harness_input.close ()

def _output (_object) :
	_message = json.dumps (_object)
	_message = _message.encode ("utf-8")
	_size = len (_message)
	_header = struct.pack (">l", _size)
	_data = _message
	_harness_output.write (_header + _data)
	_harness_output.flush ()

def _output_close () :
	_harness_output.close ()

def _sleep (_timeout) :
	time.sleep (_timeout)

## ----------------------------------------

def _main () :
	
	global _harness_input
	global _harness_output
	
	if len (sys.argv) == 2 :
		_executable = sys.argv[1]
		_scenario = None
	elif len (sys.argv) == 3 :
		_executable = sys.argv[1]
		_scenario = sys.argv[2]
	else :
		raise Exception ()
	
	_scenario = _scenarios[_scenario]
	
	_process = subprocess.Popen (
			["[mosaic_component_harness]"], executable = _executable,
			stdin = subprocess.PIPE, stdout = subprocess.PIPE)
	
	_harness_input = _process.stdout
	_harness_output = _process.stdin
	
	_scenario ()
	
	_process.wait ()
	
	sys.exit (0)

_main ()

## ----------------------------------------

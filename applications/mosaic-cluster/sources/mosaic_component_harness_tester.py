#!/usr/bin/python2.7

if __name__ != '__main__' :
	raise Exception ()

import json
import struct
import sys
import time

def _test_1 () :
	for i in xrange (0, 10) :
		_output ({
				"__action__" : "exchange",
				"index" : i,
		})
		_sleep (0.1)
	_output (None)

def _test_2 () :
	for i in xrange (0, 1) :
		_output ({
				"__action__" : "execute",
				"executable" : "/bin/sleep",
				"arguments" : ["cmd", "60"],
				"environment" : None,
				"working-directory" : None,
		})
		_sleep (0.1)
		for i in xrange (0, 10) :
			_output ({
					"__action__" : "exchange",
					"index" : i,
			})
			_sleep (0.1)
		_output ({
				"__action__" : "signal",
				"signal" : "terminate",
		})
		_sleep (0.1)
	_output (None)

def _test_3 () :
	_output ({
			"__action__" : "execute",
			"executable" : "/bin/sleep",
			"arguments" : ["cmd", "60"],
			"environment" : None,
			"working-directory" : None,
	})
	_output (None)

_scenarios = {
	"test-1" : _test_1,
	"test-2" : _test_2,
	"test-3" : _test_3,
}

def _output (_object) :
	if _object is None :
		sys.stdout.close ()
	else :
		_message = json.dumps (_object)
		_message = _message.encode ("utf-8")
		_payload = struct.pack (">l", len (_message)) + _message
		sys.stdout.write (_payload)
		sys.stdout.flush ()

def _sleep (_timeout) :
	time.sleep (_timeout)

if len (sys.argv) == 1 :
	_scenario = "test-1"
elif len (sys.argv) == 2 :
	_scenario = sys.argv[1]
else :
	raise Exception ()

_scenario = _scenarios[_scenario]
_scenario ()

sys.exit (0)

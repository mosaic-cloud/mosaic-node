
-record (configuration, {controller, controller_token, executable, argument0, arguments}).
-record (execute_specification, {executable, argument0, arguments, environment, working_directory}).
-record (signal_specification, {signal}).
-record (terminate_specification, {}).

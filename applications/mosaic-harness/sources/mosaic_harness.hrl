
-record (frontend_configuration, {controller, controller_token, executable, argument0, arguments}).
-record (frontend_execute_specification, {executable, argument0, arguments, environment, working_directory}).
-record (frontend_signal_specification, {signal}).
-record (frontend_terminate_specification, {}).

-record (backend_configuration, {controller, controller_token, input_descriptor, output_descriptor}).

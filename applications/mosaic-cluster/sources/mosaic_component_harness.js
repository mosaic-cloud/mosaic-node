
process.stdin.resume ();

process.on ("exit", function () {
	console.error ("-> exit");
});

process.on ("SIGTERM", function () {
	console.error ("-> signalled (SIGTERM)");
});

process.stdin.on ("data", function (_data) {
	console.error ("-> input" + _data);
	process.stdout.write (new Buffer ([0, 0, 0, 37]));
	process.stdout.write (new Buffer ('{"__type__":"exchange","key":"value"}'))
});

process.stdin.on ("end", function () {
	console.error ("-> end");
});

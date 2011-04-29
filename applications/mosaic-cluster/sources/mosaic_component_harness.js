
process.stdin.resume ();

process.on ("exit", function () {
	console.error ("-> exit");
});

process.on ("SIGTERM", function () {
	console.error ("-> signalled (SIGTERM)");
});

process.stdin.on ("data", function (_data) {
	console.error ("-> exchange");
	process.stdout.write (_data);
});

process.stdin.on ("end", function () {
	console.error ("-> eos");
});

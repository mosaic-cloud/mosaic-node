
#include <stdlib.h>
#include <unistd.h>

int main (int argc, char ** argv)
{
	int timeout, outcome;
	char buffer[1];
	if (argc == 1)
		timeout = -1;
	else if (argc == 2)
		timeout = -1;
	else if (argc == 3)
		timeout = atoi (argv[1]);
	else
		exit (101);
	if (timeout == -1) {
		outcome = read (0, buffer, sizeof (buffer));
		if (outcome == 0)
			exit (0);
		else if (outcome > 0)
			exit (101);
		else
			exit (102);
	} else {
		sleep (timeout);
		exit (0);
	}
}

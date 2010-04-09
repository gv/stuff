/*
	This program tries to run in given moments.

*/

#include <stdio.h>
#include <assert.h>
#include <getopt.h>
#include <time.h>
#include <sys/time.h>

#define TRACE(LEVEL_, LIST_) if(prog.verbosity >= LEVEL_) {	\
		printf LIST_;																						\
		printf("\n");																						\
	}

struct TimeTest {
	unsigned verbosity;
	unsigned interval;
	unsigned howManyTimes;
} theTest = {
	0,
	1000000,
	100
};


void runTest() {
	struct timeval start, now;
	struct timespec interval, remaining;
	int err, i;
	
	// get start time
	err = gettimeofday(&start, 0);
	if(err) {
		fprintf(stderr, "Couldn't get time of day\n");
		return;
	}

	interval.tv_sec = 0;
	interval.tv_nsec = theTest.interval;

	for(i = theTest.howManyTimes; i > 0; i--) {
		nanosleep(&interval, &remaining);
		err = gettimeofday(&now, 0);
		if(err) {
			fprintf(stderr, "Couldn't get time of day\n");
			return;
		}
		
		printf("%ld %ld\n", now.tv_sec, now.tv_usec);
	}
}

		

	
	


int main(int argc, char **argv) {
	char c;

  while ((c = getopt (argc, argv, "v")) != -1) {
    switch(c) {
		case 'v':
			theTest.verbosity++;
			break;
		case '?':
			if (isprint (optopt))
				fprintf (stderr, "Unknown option `-%c'.\n", optopt);
			else
				fprintf (stderr,
								 "Unknown option character `\\x%x'.\n",
								 optopt);
			return 1;

		default: assert("Not" ==  "happening");
		}
	}

	if(optind < argc) {
		theTest.interval = strtol(argv[optind++], NULL);
	}

	runTest();
}
	
	

	

	
	


	
		
			
	

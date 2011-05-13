#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <sys/stat.h>

void debug(const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
}

char *loadWhole(const char *path, char **end) {
	char *r = 0;
	struct stat st;
	
	if(0 <= stat(path, &st)) {
		r = malloc(st.st_size + 1);
		
		if(r) {
			FILE *fp = fopen(path, "rb");
			if(fp) {
				int size = fread(r, 1, st.st_size, fp);
				if(end)
					*end = r + size;
					
				if(size < st.st_size) {
					fprintf(stderr, "Can't read more than %d of %u in %s", 
						size, (unsigned)st.st_size, path);
				}

				fclose(fp);
			} else {
				fprintf(stderr, "Can't fopen %s\n", path);
			}
				
		} else {
			fprintf(stderr, "No memory to load %s\n", path);
		}
	} else {
		fprintf(stderr, "Can't stat %s\n", path);
	}

	return r;
}


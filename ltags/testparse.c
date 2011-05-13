#include <stdlib.h>
#include <stdio.h>

#define startGenericSpan startGenericSpan_base
#define addTagToCurrentSpan addTagToCurrentSpan_base
#include "tokenizer.c"
#undef addTagToCurrentSpan
#undef startGenericSpan

struct Span *startGenericSpan(struct File *pf, const char *start) {
	struct Span *s = startGenericSpan_base(pf, start);
	printf("<span>");
	return s;
}

struct Span *addTagToCurrentSpan(struct File *pf, 
	const char *start, const char *end) {
	struct Span *s = addTagToCurrentSpan_base(pf, start, end);
	printf("<add t='");
	fwrite(start, 1, end - start, stdout);
	printf("'/>");
	return s;
}

void saveSpan(const struct Span *span) {
	printf("</span>");
}


int main(int argc, char **argv) {
	struct File *pf = malloc(sizeof (struct File));

	if(!argc) {
		fprintf(stderr, "arg please\n");
		exit(1);
	}

	pf->path = argv[1];
	if(!chooseLanguage(pf)) {
		fprintf(stderr, "No language detected for for %s\n", pf->path);
		exit(1);
	}		
	pf->currentSpan = NULL;

	//debug("Loading: %s", path);
	pf->contents = loadWhole(pf->path, &pf->contentsEnd);
	*pf->contentsEnd = '\n'; //padding

	startGenericSpan(pf, pf->contents);
	addTagToCurrentSpan(pf, F_FEATURE, F_FEATURE + 1);
	
	parse(pf);
	while(pf->currentSpan)
		finishLastSpan(pf, pf->contentsEnd);
}


#include <stdlib.h>
#include <string.h>
#include "java_res.h"
#include "tokenizer.h"

#ifndef _WIN32
# define stricmp strcasecmp
#endif

struct JsSpanState {
	int braceCnt;
	int flags;
};
 
#define JPART(span) ((struct JsSpanState*)span->particular)

static struct Span *startSpan(struct File *pf, const char *start) {
	struct Span *s = startGenericSpan(pf, start);
	s->particular = calloc(sizeof(struct JsSpanState), 1);
	return s;
}

static void processWord(struct File *pf) {
	if(!strncmp(pf->token, "function", pf->tokenEnd - pf->token)) {
		
		addTagToCurrentSpan(pf, B_FEATURE, B_FEATURE + 1);
		return;
	}

	if(spanHasFeature(pf->currentSpan, F_FEATURE))
		startSpan(pf, pf->token);

	if(spanHasFeature(pf->currentSpan, B_FEATURE))
		startSpan(pf, pf->token);
}

static void processNonword(struct File *pf, const char *p) {
	switch(*p) {
	case '=':
		if('=' != p[1]) {
			addTagToCurrentSpan(pf, W_FEATURE, W_FEATURE + 1);
		}
		break;
		
	case '}':
	case ';': // down to a nearest function body
		while(!spanHasFeature(pf->currentSpan, F_FEATURE)) {
			if(spanHasFeature(pf->currentSpan, B_FEATURE))
				break;
			finishLastSpan(pf, p);
		}

		if('}' == *p) {
			JPART(pf->currentSpan)->braceCnt--;
			if(!JPART(pf->currentSpan)->braceCnt)
				finishLastSpan(pf, p);
			
}


static int startParsing(struct File *f) {
}

static void finish(struct File *pf) {
	free(pf->langParserState);
}


static int isJsSrcName(const char *path) {
	const char *suffix;
	suffix = strrchr(path, 0) - 3;
	return (suffix > path && 
		(!stricmp(suffix, ".ks") || !stricmp(suffix, ".js")));
}


const struct Language jsLanguage = {
	isJsSrcName, 
	startParsing, 
	processWord, 
	processNonword,
	finish
};

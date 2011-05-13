#include <stdlib.h>
#include <string.h>
#include "tokenizer.h"

#ifndef _WIN32
# define stricmp strcasecmp
#endif

struct JsSpanState {
	int braceCnt;
	int flags;
	int type;
	enum {
		STATEMENTS, 
		AFTER_FUNCTION,
		AFTER_FUNCTION_NAME,
		END_AFTER_BRACES_CLOSE
	} mode;
};

struct JsParserState {
	int dontknowwhat;
};
 
#define TOPSTATE ((struct JsSpanState*)pf->currentSpan->particular)

static int startParsing(struct File *pf) {
	pf->langParserState = calloc(sizeof (struct JsParserState), 1);
	pf->currentSpan->particular = calloc(sizeof(struct JsSpanState), 1);
	TOPSTATE->mode = END_AFTER_BRACES_CLOSE;
	TOPSTATE->braceCnt = pf->contentsEnd - pf->contents;
	return 1;
}

#define STATE ((struct JavaParserState*)pf->langParserState)

static struct Span *startSpan(struct File *pf, const char *start) {
	struct Span *s = startGenericSpan(pf, start);
	s->particular = calloc(sizeof(struct JsSpanState), 1);
	return s;
}

#define JPART(span) ((struct JsSpanState*)span->particular)

static void processWord(struct File *pf) {
	switch(TOPSTATE->mode) {
	case AFTER_FUNCTION:
		if(!spanHasFeature(pf->currentSpan, W_FEATURE)) { // was not assigned
			addTagToCurrentSpan(pf, W_FEATURE, W_FEATURE+ 1);
			addTagToCurrentSpan(pf, D_FEATURE, D_FEATURE + 1);
			addTagToCurrentSpan(pf, pf->token, pf->tokenEnd);
		}
		TOPSTATE->mode = END_AFTER_BRACES_CLOSE;
		TOPSTATE->braceCnt = 0;
		startSpan(pf, pf->tokenEnd);
		TOPSTATE->mode = STATEMENTS;
		return;

	case END_AFTER_BRACES_CLOSE:
		startSpan(pf, pf->token);

	case STATEMENTS:
		if(!strncmp(pf->token, "function", pf->tokenEnd - pf->token)) {
			if(spanHasFeature(pf->currentSpan, W_FEATURE))
				startSpan(pf, pf->token);
			TOPSTATE->mode = AFTER_FUNCTION;
			return;
		}
		addTagToCurrentSpan(pf, pf->token, pf->tokenEnd);
		return;
	}

}

static void processNonword(struct File *pf, const char *p) {
	switch(*p) {
	case '{':
		if(AFTER_FUNCTION == TOPSTATE->mode) {
			TOPSTATE->mode = END_AFTER_BRACES_CLOSE;
			TOPSTATE->braceCnt = 1;
			return;
		}
		while(END_AFTER_BRACES_CLOSE != TOPSTATE->mode) 
			finishLastSpan(pf, p);
		TOPSTATE->braceCnt++;
		break;

	case '=':
	case ':':
		if('=' == p[1]) 
			break;

		while(END_AFTER_BRACES_CLOSE != TOPSTATE->mode) 
			finishLastSpan(pf, p);
		startSpan(pf, pf->token);
		TOPSTATE->mode = STATEMENTS;
		addTagToCurrentSpan(pf, pf->token, pf->tokenEnd);
		addTagToCurrentSpan(pf, W_FEATURE, W_FEATURE + 1);
		break;
		
	case '}':
	case ';': // down to statements
		while(END_AFTER_BRACES_CLOSE != TOPSTATE->mode) 
			finishLastSpan(pf, p);

		if('}' != *p)
			break;
		TOPSTATE->braceCnt--;
		if(!TOPSTATE->braceCnt)
			finishLastSpan(pf, p);
	}
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

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
		TOPSTATE->mode = STATEMENTS;

	case STATEMENTS:
		if(!strncmp(pf->token, "var", pf->tokenEnd - pf->token)) 
			return;
		//if(!strncmp(pf->token, "for", pf->tokenEnd - pf->token)) 
		//	return;
		//if(!strncmp(pf->token, "if", pf->tokenEnd - pf->token)) 
		//	return;
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
	struct Span *s;
	switch(*p) {
	case '(':
		if(AFTER_FUNCTION == TOPSTATE->mode) {
			TOPSTATE->mode = END_AFTER_BRACES_CLOSE;
			TOPSTATE->braceCnt = 1;
		}
		return;

	case '{':
		s = pf->currentSpan;
		while(END_AFTER_BRACES_CLOSE != JPART(s)->mode) 
			s = s->parent;
		JPART(s)->braceCnt++;
		break;
		
	case '=':
	case ':':
		if('=' != p[1]) {
			if(p > pf->contents && p[-1] != '=') {
				delTagFromCurrentSpan(pf, pf->token);
				while(END_AFTER_BRACES_CLOSE != TOPSTATE->mode) 
					finishLastSpan(pf, pf->token);
				startSpan(pf, pf->token);
				TOPSTATE->mode = STATEMENTS;
				addTagToCurrentSpan(pf, pf->token, pf->tokenEnd);
				addTagToCurrentSpan(pf, W_FEATURE, W_FEATURE + 1);
			}
		}
	break;
		
	case '}':
	case ';': { 
		s = pf->currentSpan;
		while(END_AFTER_BRACES_CLOSE != JPART(s)->mode) { 
			if(spanHasFeature(s, W_FEATURE))
				break;
			s = s->parent;
		}
		
		if(END_AFTER_BRACES_CLOSE != JPART(s)->mode)
			while(END_AFTER_BRACES_CLOSE != TOPSTATE->mode) 
				finishLastSpan(pf, p);
		
		if('}' != *p)
			break;
		
		while(END_AFTER_BRACES_CLOSE != JPART(s)->mode) { 
			s = s->parent;
		}

		JPART(s)->braceCnt--;
		if(!JPART(s)->braceCnt) {
			while(pf->currentSpan != s)
				finishLastSpan(pf, p);
			finishLastSpan(pf, p);
		}
	}
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

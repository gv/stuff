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
		TAKE_FUNCTION_NAME,
		CONTAIN_COMPOUND,
		CONTAIN_NOTHING,
		CONTAIN_VALUE
	} mode;
};

struct JsParserState {
	int dontknowwhat;
};
 
#define TOPSTATE ((struct JsSpanState*)pf->currentSpan->particular)

static void startParsing(struct File *pf) {
	pf->langParserState = calloc(sizeof (struct JsParserState), 1);
	pf->currentSpan->particular = calloc(sizeof(struct JsSpanState), 1);
	TOPSTATE->mode = CONTAIN_COMPOUND;
	TOPSTATE->braceCnt = pf->contentsEnd - pf->contents;
}

#define STATE ((struct JavaParserState*)pf->langParserState)

static struct Span *startSpan(struct File *pf, const char *start) {
	struct Span *s = startGenericSpan(pf, start);
	s->particular = calloc(sizeof(struct JsSpanState), 1);
	return s;
}

#define JPART(span) ((struct JsSpanState*)span->particular)

static void processWord(struct File *pf) {
	if(!strncmp(pf->token, "var", pf->tokenEnd - pf->token)) 
		return;

	if(!strncmp(pf->token, "function", pf->tokenEnd - pf->token)) {
		while(CONTAIN_NOTHING == TOPSTATE->mode)
			finishLastSpan(pf, pf->token);
		startSpan(pf, pf->token); 
		// if name doesn't follow, this span will just have no tags
		TOPSTATE->mode = TAKE_FUNCTION_NAME;
		addTagToCurrentSpan(pf, W_FEATURE, W_FEATURE+ 1);
		addTagToCurrentSpan(pf, D_FEATURE, D_FEATURE + 1);
		return;
	}

	if(TAKE_FUNCTION_NAME == TOPSTATE->mode) {
		addTagToCurrentSpan(pf, pf->token, pf->tokenEnd);
		TOPSTATE->mode = CONTAIN_COMPOUND;
		TOPSTATE->braceCnt = 0;
		return;
	}

	if(CONTAIN_NOTHING != TOPSTATE->mode) {
		startSpan(pf, pf->token);
		TOPSTATE->mode = CONTAIN_NOTHING;
	}
	addTagToCurrentSpan(pf, pf->token, pf->tokenEnd);
}

static struct Span *findCompound(struct Span *s) {
	while(CONTAIN_COMPOUND != JPART(s)->mode) 
		s = s->parent;
	return s;
}

static void processNonword(struct File *pf, const char *p) {
	struct Span *s;
	switch(*p) {
	case '(':
		if(TAKE_FUNCTION_NAME == TOPSTATE->mode) {
			TOPSTATE->mode = CONTAIN_COMPOUND;
			TOPSTATE->braceCnt = 0;
		}
		return;

	case '{':
		s = pf->currentSpan;
		JPART(findCompound(s))->braceCnt++;
		break;
		
	case '}':		
		s = findCompound(pf->currentSpan);
		JPART(s)->braceCnt--;
		if(!JPART(s)->braceCnt) {
			while(pf->currentSpan != s)
				finishLastSpan(pf, p);
			finishLastSpan(pf, p);
		}
		break;

	case '=':
	case ':': {
		if('=' == p[1]) 
			break;
		if(p == pf->contents) 
			break;
		if(p[-1] == '=' || p[-1] == '<' || p[-1] == '>')
			break;

		while(CONTAIN_NOTHING == TOPSTATE->mode || CONTAIN_VALUE == TOPSTATE->mode) {
			delTagFromCurrentSpan(pf, pf->token);
			finishLastSpan(pf, pf->token);
		}
		
		startSpan(pf, pf->token);
		TOPSTATE->mode = CONTAIN_VALUE;
		addTagToCurrentSpan(pf, pf->token, pf->tokenEnd);
		addTagToCurrentSpan(pf, W_FEATURE, W_FEATURE + 1);
		}
	break;

	case ';': // if we're inside assignment, return to compound
		s = pf->currentSpan;
		while(CONTAIN_COMPOUND != JPART(s)->mode) { 
			if(CONTAIN_VALUE == JPART(s)->mode) {
				while(CONTAIN_COMPOUND != TOPSTATE->mode)
					finishLastSpan(pf, p);
				break;
			}
			s = s->parent;
		}
		break;
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

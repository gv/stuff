#include <stdlib.h>
#include <string.h>
#include "java_res.h"
#include "tokenizer.h"

#ifndef _WIN32
# define stricmp strcasecmp
#endif

struct JavaParserState {
	char *probableName, *probableNameEnd;
	int flags;
};

struct JavaSpanState {
	int braceCnt;
	int flags;
};
 
static void startParsingJavaSrc(struct File *pFile) {
	pFile->langParserState = calloc(sizeof (struct JavaParserState), 1);
	pFile->currentSpan->particular = calloc(sizeof(struct JavaSpanState), 1);
}

static void finish(struct File *pf) {
	free(pf->langParserState);
}

#define JPART(span) ((struct JavaSpanState*)span->particular)

#define LIST_EXPECTED 1
#define ANNOTATION    2

static struct Span *startSpan(struct File *pf, const char *start) {
	struct Span *s = startGenericSpan(pf, start);
	s->particular = calloc(sizeof(struct JavaSpanState), 1);
	return s;
}

#define STATE ((struct JavaParserState*)pf->langParserState)

static void processJavaWord(struct File *pf) {
	int rw;

	if('@' == *pf->token) {
		STATE->flags |= ANNOTATION;
		return;
	}
	STATE->flags &= ~ANNOTATION;
	
	rw = getJavaReservedWordIndex(pf->token, pf->tokenEnd - pf->token);
	//debug("%d", rw);

	if(spanHasFeature(pf->currentSpan, F_FEATURE)) // file
		startSpan(pf, pf->token);

	if(spanHasFeature(pf->currentSpan, B_FEATURE)) // class or function body
		startSpan(pf, pf->token);

	switch(rw) {
	case ENUM_:
		JPART(pf->currentSpan)->flags |= LIST_EXPECTED;
		break;
	case CLASS_:
	case INTERFACE_:
		//debug("class");
		addFeatureToCurrentSpan(pf, C_FEATURE);
		addFeatureToCurrentSpan(pf, D_FEATURE);
		return;
	}
	
	addTagToCurrentSpan(pf, pf->token, pf->tokenEnd);
}


static void processJavaNonword(struct File *pf, const char *p) {
	struct Span *newSpan, *body;

	if(STATE->flags & ANNOTATION) {
		if(')' == *p)
			STATE->flags &= ~ANNOTATION;
		return;
	}
		
	switch(*p) {
	case ';':
		if(!spanHasFeature(pf->currentSpan, F_FEATURE))
			if(!spanHasFeature(pf->currentSpan, B_FEATURE))
				finishLastSpan(pf, p);
	break;

	case '{':
		if(LIST_EXPECTED & JPART(pf->currentSpan)->flags)
			break;

		if(spanHasFeature(pf->currentSpan, C_FEATURE)) {
			addFeatureToCurrentSpan(pf, B_FEATURE);
		} else if(pf->currentSpan->parent) {
			if(spanHasFeature(pf->currentSpan->parent, C_FEATURE)) {
				// method
				addFeatureToCurrentSpan(pf, B_FEATURE);
			}
		}
		body = findSpanWithFeature(pf->currentSpan, B_FEATURE);
		if(body)
			JPART(body)->braceCnt++;
		else
			debug("%s:unexp {", pf->path);
		break;
		
	case '}': 
		if(LIST_EXPECTED & JPART(pf->currentSpan)->flags) {
			finishLastSpan(pf, p);
			JPART(pf->currentSpan)->flags &= ~LIST_EXPECTED;
			break;
		}
		//body = findSpanWithFeature(pf->currentSpan, B_FEATURE);
		if(spanHasFeature(pf->currentSpan, B_FEATURE)) {
			JPART(pf->currentSpan)->braceCnt--;
			if(!JPART(pf->currentSpan)->braceCnt)
				finishLastSpan(pf, p);
		}
		else
			debug("%s:%d:unexp }", pf->path, p - pf->contents);
		break;
	}
}




static int endsWithDotJava(const char *path) {
	const char *suffix;
	suffix = strrchr(path, 0) - 5;
	return (suffix > path && !stricmp(suffix, ".java"));
}

const struct Language javaLanguage = {
	endsWithDotJava, 
	startParsingJavaSrc, 
	processJavaWord, 
	processJavaNonword,
	finish
};

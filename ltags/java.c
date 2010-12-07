#include <stdlib.h>
#include <string.h>
#include "java_res.h"
#include "tokenizer.h"

#ifndef _WIN32
# define stricmp strcasecmp
#endif

struct JavaParserState {
	char *probableName, *probableNameEnd;
};

struct JavaSpanState {
	int braceCnt;
};
 
static void startParsingJavaSrc(struct File *pFile) {
	pFile->langParserState = calloc(sizeof (struct JavaParserState), 1);
}



static void processJavaWord(struct File *pf) {
	int rw = getJavaReservedWordIndex(pf->token, pf->tokenEnd - pf->token);
	
	if(spanHasTag(pf->currentSpan, F_FEATURE))
		startSpan(pf, pf->token);
	addTagToCurrentSpan(pf, pf->token, pf->tokenEnd);
}

static void processJavaNonword(struct File *pf, const char *p) {
	struct Span *newSpan;
	switch(*p) {
	case ';':
	case ')':
		if(!spanHasTag(pf->currentSpan, F_FEATURE))
			finishLastSpan(pf, p);
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
	processJavaNonword
};

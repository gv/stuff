#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>
#include "tokenizer.h"

// hyperoptimized

const int charFlags[/*256*/128] = {
	4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, // 00
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 10
	4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 20
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, // 30
	0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, // 40
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 3, 3, // 50
	0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, // 60
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, // 70
};

unsigned classifyChar(int c) {
	if(c >= countof(charFlags))
		return CHAR_TOKENSTART | CHAR_TOKENMIDDLE;
	else 
		return charFlags[c];
}
 
#define SPACE         0
#define TOKEN         1
#define NUMBER        2
#define COMMENT_LINE  '\n'
#define COMMENT_LINES '*'

void parse(struct File *pf) {
	int r;
	char *p;
	int mode, flags;

	pf->language->startParsing(pf);
	
	pf->token = p = pf->contents;
	mode = SPACE;
	flags = 0;
	for(;;p++) {
#ifdef QUESTION_TESTPARSE
		fputc(*p, stdout);
#endif 
		switch(mode) {
		case '\'':
		case '"':
			// p[-1] is valid here, bc we must have entered this mode by
			// reading some characters
			if(mode == *p) {
				if('\\' != p[-1]) 
					mode = SPACE;
			}	else if(pf->contentsEnd == p)
				goto end;
		break;
					
			
		case COMMENT_LINE:
			if('\n' == *p) {
				if(pf->contentsEnd == p)
					goto end;
				mode = SPACE;
			}
			break;

		case COMMENT_LINES:
			if('*' == *p) {
				// p[1] is valid here, bc last valid byte is padded '\n'
				if('/' == p[1])
					mode = SPACE;
			} else if (pf->contentsEnd == p) 
				goto end;
			break;

		case NUMBER:
			if('.' == *p)
				break;
			if(*p >= countof(charFlags))
				flags = CHAR_TOKENSTART | CHAR_TOKENMIDDLE;
			else 
				flags = charFlags[*p];
			
			if(flags & CHAR_TOKENMIDDLE) 
				break;
			
			goto space;
			
		case TOKEN:
			if(*p >= countof(charFlags))
				flags = CHAR_TOKENSTART | CHAR_TOKENMIDDLE;
			else 
				flags = charFlags[*p];

			if(flags & CHAR_TOKENMIDDLE) 
				break;
			else {
				// token just completed!
				pf->tokenEnd = p;
				pf->language->processWord(pf);
				mode = SPACE;
				// no break
			}
			
		case SPACE:
		space:

			if('/' == *p) {
				// p[1] is a valid memory reference here because the last byte is 
				// appended '\n'
				if('*' == p[1]) {
					mode = COMMENT_LINES;
					break;
				} else if('/' == p[1]) {
					mode = COMMENT_LINE;
					break;
				}
			}

			if('"' == *p || '\'' == *p) {
				mode = *p;
				break;
			}
			
			flags = classifyChar(*p);
			
			if(CHAR_TOKENSTART & flags) {
				pf->token = p;
				mode = TOKEN;
			} else if(CHAR_TOKENMIDDLE & flags) {
				mode = NUMBER;
			} else if(CHAR_SPACE & flags) {
				if(pf->contentsEnd == p)
					goto end;
				break;
			} else 
				pf->language->processNonword(pf, p);
		} // switch(mode)
	} // for(;;p++)
	
		
	
 end:
	pf->language->finishParsing(pf);
}

struct Span *addTagToCurrentSpan(struct File *pf, 
	const char *start, const char *end) {
	pf->currentSpan->tagsEnd->start = start;
	pf->currentSpan->tagsEnd->end = end;
	pf->currentSpan->tagsEnd++;
	return pf->currentSpan;
}

struct Span *startGenericSpan(struct File *pf, const char *start) {
	struct Span *s = malloc(sizeof (struct Span));
	s->path = pf->path;
	s->mtime = pf->mtime;
	s->start = start - pf->contents;
	s->tagsEnd = s->tags;
	s->end = 0;
	s->particular = NULL;

	s->parent = pf->currentSpan;
	pf->currentSpan = s;
	return s;
}

struct Span *finishLastSpan(struct File *pf, const char *end) {
	struct Span *s = pf->currentSpan;
	struct Word *w;
	s->end = end - pf->contents;

	for(w = pf->currentSpan->tags; w < pf->currentSpan->tagsEnd; w++) {
		if(w->start >= pf->contents)
			if(w->end <= pf->contentsEnd) {
				saveSpan(s);
				break;
			}
	}
	pf->currentSpan = s->parent;
	free(s->particular);
	free(s);
	return pf->currentSpan;
}

int spanHasFeature(const struct Span *ps, const char *start) {
	const struct Word *w = ps->tagsEnd;
	while(--w >= ps->tags) {
		if(start == w->start)
			return 1;
	}
	 
	return 0;
}

struct Span *findSpanWithFeature(struct Span *ps, const char *feature) {
	if(!ps)
		return NULL;
	if(spanHasFeature(ps, feature))
		return ps;
	return findSpanWithFeature(ps->parent, feature);
}

struct Span *delTagFromCurrentSpan(struct File *pf, const char *start) {
	struct Word *w = pf->currentSpan->tagsEnd;
	while(--w >= pf->currentSpan->tags) {
		if(w->start = start) {
			*w = *pf->currentSpan->tagsEnd;
			pf->currentSpan->tagsEnd--;
			break;
		}
	}
	return pf->currentSpan;
}
			
	 
	 
const char F_FEATURE[] = "f"; // file
const char D_FEATURE[] = "d"; // definition
const char C_FEATURE[] = "c"; // class
const char B_FEATURE[] = "b"; // body
const char W_FEATURE[] = "w"; // write

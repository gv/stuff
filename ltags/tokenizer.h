#include "storage.h"

struct File;
struct Language {
	int (*couldDoPath)(const char *path);
	void (*startParsing)(struct File*);
	void (*processWord)(struct File*);
	void (*processNonword)(struct File*, const char*);
	void (*finishParsing)(struct File*);
};

// parser state		
struct File {
	char *path;
	int mtime;

	char *contents;
	char *contentsEnd;
	char *token;
	char *tokenEnd;
	struct Span *rootSpan;
	struct Span *currentSpan;

	const struct Language *language;
	void *langParserState;
};

#define countof(something_M) (sizeof(something_M)/sizeof(something_M[0]))

#define CHAR_TOKENMIDDLE 1
#define CHAR_TOKENSTART  2
#define CHAR_SPACE       4

extern const char F_FEATURE[];
extern const char C_FEATURE[];
extern const char D_FEATURE[];
extern const char B_FEATURE[];
extern const char W_FEATURE[];

void 
parse(struct File*);

unsigned 
classifyChar(int c);

struct Span *
addTagToCurrentSpan(struct File *pf, 
	const char *start, const char *end);

struct Span *
startGenericSpan(struct File *pf, const char *start);

struct Span *
finishLastSpan(struct File *pf, const char *end);

int 
spanHasFeature(const struct Span *ps, const char *start);

struct Span *
findSpanWithFeature(struct Span *ps, const char *feature);

struct Language *chooseLanguage(struct File *pf);

char *loadWhole(const char *path, char **end);



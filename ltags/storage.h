#define MAX_TAG_CNT 256
#define MAX_FEATURE_CNT 256

#define ASSERTSQL(expr_M) do{																						\
		r = expr_M; if(r != SQLITE_OK) {																		\
			fprintf(stderr, "SQLite assertion fail: '%s', code %d\n", #expr_M, r); \
			exit(1);																													\
		}}while(0)
 
struct Word {
	const char *start, *end;
};

struct Span {
	struct Word tags[MAX_TAG_CNT], *tagsEnd;
	const char *features[MAX_FEATURE_CNT], **endOfFeatures;
	int start, end;
	int mtime;
	int weight;
	const char *path;
	
	struct Span *parent;
	void *particular;

	char *tagsText, *featuresText;
};

void 
initStorageThread();

void 
saveSpan(const struct Span*);


#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/stat.h>
#include <assert.h>

#ifdef _WIN32
# include "ext/dirent.h"
# include "ext/getopt/getopt.h"
#else
# include <getopt.h>
# include <dirent.h>
# include <strings.h>
# define stricmp strcasecmp
#endif

#include "ext/sqlite/sqlite3.h"
#include "queue.h"
#include "tokenizer.h"


#ifndef MAX_PATH
# ifdef PATH_MAX
#  define MAX_PATH PATH_MAX
# endif
#endif

#define QUESTIONABLE "1"

char *strChrOrEnd(const char *str, char c) {
	char *r = strchr(str, c);
	if(r)
		return r;
	return strchr(str, 0);
}

void normalizePath(char *path) {
	char *p;
	if(':' == path[1])
		path[0] = tolower(path[0]);
	for(p = path; *p; p++) 
		if('\\' == *p) *p = '/';
}

const char *getPathFrom(char *d, const char *path, const char *base) {
	// Both arguments are absolute paths 
	// base is a path to a directory
	const char *slash = NULL;
	
	*d = 0;
#ifdef _WIN32
	while(*path && tolower(*path) == tolower(*base)) 
#else
		while(*path && *path == *base) 
#endif
			{
				if('/' == *base) {
					slash = base;
				}
				path++, base++;
			}

	if(!slash) // different Windows drive letters
		return path;
	
	if(*base) { // go back
		path -= (base - slash);
		base = slash;
	}

	while(*base) {
		if('/' == *base)
			strcat(d, "../");
		base++;
	}
	
	strcat(d, path + 1);
	return d;
}

char wdPath[MAX_PATH];

#ifdef _WIN32
char *realpath(char *path, char *resolved) {
	char *end;
	const char *itemStart = path, *itemEnd;
	
	if('/' == path[0])
		return path;
	if(':' == path[1] && '/' == path[2])
		return path;

	strncpy(resolved, wdPath, MAX_PATH);
	for(itemEnd = path; *itemEnd; itemStart = itemEnd + 1) {
		itemEnd = strChrOrEnd(itemStart, '/');
		if(itemEnd == itemStart) 
			continue;
		if('.' == *itemStart) {
			if(1 == itemEnd - itemStart)
				continue;
			if('.' == itemStart[1] && itemEnd - itemStart == 2) {
				end = strrchr(resolved, '/');
				if(end)
					*end = 0;
				continue;
			}
		}
			
		end = strrchr(resolved, 0);
		if(end >= resolved + MAX_PATH)
			return NULL;
		*end++ = '/';
		while(end < resolved + MAX_PATH - 1 && itemStart < itemEnd)
			*end++ = *itemStart++;
		
		*end = 0;
	}
	return resolved;
}
#endif

#define PROGNAME "ltags"

const char *dbName = "." PROGNAME ".sqlite";
char dbPath[MAX_PATH];
char dbDirPath[MAX_PATH];

int findDb() {
	char *end, *minEnd = 0;
	struct stat st;

	strcpy(dbDirPath, wdPath);
	end = strchr(dbDirPath, 0);
	strcat(dbDirPath, "/");
	do {
		strcpy(end + 1, dbName);
		
		if(stat(dbDirPath, &st) >= 0 ) {
			minEnd = end;
		}

		*end = 0;
		end = strrchr(dbDirPath, '/');
	} while(end > dbDirPath + 2);

	if(!minEnd)
		return 0;
	
	strcpy(dbDirPath, wdPath);
	*minEnd = 0;
	
	strcpy(dbPath, dbDirPath);
	strcat(dbPath, "/");
	strcat(dbPath, dbName);
	return 1;
}
		
	
	
static int callback(void *NotUsed, int argc, char **argv, char **azColName){
  int i;
  for(i=0; i<argc; i++){
    printf("%s = %s\n", azColName[i], argv[i] ? argv[i] : "NULL");
  }
  printf("\n");
  return 0;
}


sqlite3 *db;

void run(char *stmt) {
	int rc;
  char *zErrMsg = 0;
  rc = sqlite3_exec(db, stmt, callback, 0, &zErrMsg);
  if( rc!=SQLITE_OK ){
    fprintf(stderr, "SQL error: %s\n", zErrMsg);
    sqlite3_free(zErrMsg);
		exit(1);
  }
}



sqlite3_int64 getPathId(const char *path) {
	// creates if not found
	sqlite3_int64 res = 0;
	int r;
	sqlite3_stmt *stm;

	ASSERTSQL(sqlite3_prepare_v2(db, "SELECT oid FROM paths WHERE path = ?", 
			-1, &stm, 0));
	ASSERTSQL(sqlite3_bind_text(stm, 1, path, -1, SQLITE_STATIC));
	
	r = sqlite3_step(stm);
	if(r == SQLITE_ROW) {
		res = sqlite3_column_int64(stm, 0);
	} else if(r != SQLITE_DONE) {
		debug("getPathId: %d", r);
		exit(1);
	}

	ASSERTSQL(sqlite3_finalize(stm));
	if(!res) {
		// TODO LOCK
		ASSERTSQL(sqlite3_prepare_v2(db, 
				"INSERT INTO paths(path, status) VALUES(?, 0)", 
				-1, &stm, 0));
		ASSERTSQL(sqlite3_bind_text(stm, 1, path, -1, SQLITE_STATIC));
		r = sqlite3_step(stm);
		if(r != SQLITE_DONE) {
			debug("Adding path '%s' not done (%d)", path, r);
			exit(1);
		}
		ASSERTSQL(sqlite3_finalize(stm));
		res = sqlite3_last_insert_rowid(db);
	}
	return res;
}


pthread_t parserThreads[4];
void *files[5];
queue_t fileQueue = QUEUE_INITIALIZER(files);

void updateFile(const char *path) {
	char bf[2 * MAX_PATH], *name, *nameEnd;
	struct File *pf;
	struct stat st;
	int r;
	int storedTime = 0;
	sqlite3_stmt *stm;
	
	r = stat(path, &st);
	if(r < 0) {
		debug("Can't stat %s", path);
		return;
	}

	/*name = strrchr(path, UP);
		nameEnd = strchr(name, '.');
		ASSERTSQL(sqlite3_prepare_v2(db, "SELECT mtime FROM spans WHERE name = ?", -1,
		&stm, 0));

		ASSERTSQL(sqlite3_bind_text(stm, 1, name, nameEnd - name, SQLITE_STATIC));

		r = sqlite3_step(stm);
		if(SQLITE_ROW == r) {
		storedTime = sqlite3_column_int(stm, 0);
		} else if(r != SQLITE_DONE) {
		fprintf(stderr, "step: %d\n", r);
		return;
		}
	*/

	if(storedTime == st.st_mtime) {
		debug("Skipping: %s", path);
		return;
	}

	
	pf = malloc(sizeof(*pf));

	/*
		If we can't parse this, it's still probably a part of the project,
		so we'd like its path to show up in results
	*/
	pf->path = strdup(path);
	pf->pathId = getPathId(path);
	if(!chooseLanguage(pf)) {
		debug("No language detected for for %s", path);
	}		
	pf->mtime = st.st_mtime;
	pf->currentSpan = NULL;

	if(pf->language) {
		debug("Loading: %s", path);
		pf->contents = loadWhole(path, &pf->contentsEnd);
		*pf->contentsEnd = '\n'; //padding
	}

	queue_enqueue(&fileQueue, pf);
}

void updateDir(char *path) {
	char *end = strrchr(path, 0), *suffix;
	DIR *d;
	struct dirent *entry;

	if(*path) {
		d = opendir(path);
		*end++ = '/';
	} else {
		d = opendir(".");
	}

	if(!d) {
		debug("Can't opendir: %s", path);
		return;
	}
	
	while(entry = readdir(d)) {
		if('.' == entry->d_name[0])
			continue;
		
		if(DT_DIR & entry->d_type) {
			strncpy(end, entry->d_name, MAX_PATH - (end - path));
			updateDir(path);
			continue;
		}

		strncpy(end, entry->d_name, MAX_PATH - (end - path));
		updateFile(path);
	}	
}



void *parserThread(void *unused) {
	int r;
	struct File *pf = 0;
	const char *ws, *we;

	initStorageThread();

	while(1) {
		pf = queue_dequeue(&fileQueue);
		if(!pf)
			break;
		
		startGenericSpan(pf, pf->contents);
		addFeatureToCurrentSpan(pf, F_FEATURE);

		assert(strchr(pf->path, '/'));
		ws = we = strchr(pf->path, 0);
		while(--ws) {
			if(!(CHAR_TOKENMIDDLE & classifyChar(*ws))) {
				
				if(we - ws > 1) {
					debug(ws + 1);
					addTagToCurrentSpan(pf, ws+1, we);
				}
				if('/' == *ws)
					break;
				we = ws;
			}
		}
			
		if(pf->language) {
			debug("Parsing: %s", pf->path);
			parse(pf);
		}

		while(pf->currentSpan)
			finishLastSpan(pf, pf->contentsEnd);

		if(pf->language)
			free(pf->contents);
		free(pf->path);
		free(pf);
	}
	return 0;
}

/*
	Tested on android/packages/apps/Gallery/
	
	vg@nostromo:~/src/android/packages/apps/Gallery$ find -iname *.java|xargs du -ch	
	...
	520K    total


*/

		
void update(const char **srcPaths) {
	int r;
	char curPath[MAX_PATH];
	sqlite3_stmt *stm;
	struct stat st;
	int justCreated = 0;
	
	int i;

	if(stat(dbPath, &st) < 0)
		justCreated = 1;

	r = sqlite3_open(dbPath, &db);
	if(r) {
		fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
		sqlite3_close(db);
		exit(1);
	}

	//#ifdef USE_FTS3
		if(justCreated) {
			ASSERTSQL(sqlite3_prepare_v2(db, 
					"CREATE VIRTUAL TABLE spans USING fts3("
					"pathId   INTEGER, "
					"features VARCHAR(256), "
					"weight   INTEGER, "
					"mtime    INTEGER, "
					"start    INTEGER, "
					"end      INTEGER, "
					"status   INTEGER, "
					"tags     TEXT)",
					-1, &stm, 0));

			r = sqlite3_step(stm);
			if(r != SQLITE_DONE) {
				debug("Table creation step: %d", r);
			}
			ASSERTSQL(sqlite3_finalize(stm));

			ASSERTSQL(sqlite3_prepare_v2(db, 
					"CREATE TABLE paths("
					"path     VARCHAR(256), "
					"status   INTEGER)",
					-1, &stm, 0));

			r = sqlite3_step(stm);
			if(r != SQLITE_DONE) {
				debug("Table creation step: %d", r);
			}
			ASSERTSQL(sqlite3_finalize(stm));

		}
		//#endif

			/*run("CREATE TABLE IF NOT EXISTS spans("
				"path   VARCHAR(256), "
				"mtime  INTEGER, "
				"start  INTEGER, "
				"end    INTEGER, "
				"status INTEGER)");
	
				/*run("CREATE TABLE IF NOT EXISTS tags("
				"name   VARCHAR(100), "
				"sid    INTEGER)");*/
		run("PRAGMA journal_mode=WAL");
	
		//run("CREATE INDEX spidx on spans(path, start)");

		for(i = 0; i < countof(parserThreads); i++) {
			pthread_create(&parserThreads[i], NULL, parserThread, NULL);
		}
	
		if(*srcPaths) {
			sqlite3_stmt *stm;
			int r;
			char path[MAX_PATH];
			run("BEGIN");

			ASSERTSQL(sqlite3_prepare_v2(db, 
					"UPDATE spans SET status=" QUESTIONABLE " WHERE pathId=?", 
					-1, &stm, 0));
			for(; *srcPaths; srcPaths++) {
				char *givenPath = strdup(*srcPaths);
				ASSERTSQL(sqlite3_reset(stm));
				ASSERTSQL(sqlite3_bind_int(stm, 1, getPathId(*srcPaths)));
				if(SQLITE_DONE != sqlite3_step(stm))
					debug("Invalidating not done");
				normalizePath(givenPath);
				updateFile(realpath(givenPath, path));
				free(givenPath);
			}
		} else {
			debug("Invalidating old entries...");
			run("UPDATE spans SET status=1"); 
			run("BEGIN");

			strcpy(curPath, dbDirPath);
			debug(curPath);
			updateDir(curPath);
		}

		// parser thread stops when it receives NULL
		for(i = 0; i < countof(parserThreads); i++) {
			queue_enqueue(&fileQueue, NULL);
		}
		for(i = 0; i < countof(parserThreads); i++) {
			pthread_join(parserThreads[i], NULL);
		}

		debug("Commit...");
		run("COMMIT");
		debug("Deleting old entries...");
		run("DELETE FROM spans WHERE status=1");
	
		sqlite3_close(db);
}


int readSpan(struct Span *s, sqlite3_stmt *stm) {
	int r;

	r = sqlite3_step(stm);
	if(r != SQLITE_ROW)	
		return 0;
	s->pathId = sqlite3_column_int64(stm, 0);
	s->start = sqlite3_column_int(stm, 1);
	s->end = sqlite3_column_int(stm, 2);
	s->tagsText = (char*)sqlite3_column_text(stm, 3); // not const
	s->featuresText = (char*) sqlite3_column_text(stm, 4);
	return 1;
}

struct Word *splitTags(struct Span *s) {
	char *tags, *next;
	tags = s->tagsText;
	s->tagsEnd = &s->tags[0];
	while(*tags) {
		next = strchr(tags, ' ');
		if(!next)
			next = strchr(tags, 0);
		s->tagsEnd->start = tags;
		s->tagsEnd->end = next;
		s->tagsEnd++;
		if(!*next)
			break;
		tags = next + 1;
	}
	return &s->tags[0];
}

#define TERM_SEQ 1
#define TERM_POS 2
#define TERM_INCOMPLETE 4

/*
	Search condition.
*/
struct Term {
	int flags;
	char *arg, *argPos, *argEnd;
	sqlite3_stmt *stm;
	
	union {
		// Usual search word (or prefix, if TERM_INCOMPLETE)
		struct {
			char *word;
			char *feature;
		};

		// Position in the source. 
		// Must restrict results to bodies enclosing that position.
		struct {
			unsigned pos;
			char *path;
		};
	};
};

int termAllowsSpan(struct Term *t, struct Span *s) {
	if(TERM_POS & t->flags) {
		if(t->pos >= s->start)
			if(t->pos <= s->end)
				//if(!strcmp(t->path, s->path))
				// Now it's broken!
					return 1;
	} else {
		struct Word *tag;

		if(t->feature) {
			if(!strstr(s->featuresText, t->feature))
				return 0;
		}

		for(tag = splitTags(s); tag < s->tagsEnd; tag++) {
			if(TERM_INCOMPLETE & t->flags) {
				if(!strncmp(t->word, tag->start, strlen(t->word)))
					return 1;
			} else {
				if(strlen(t->word) == tag->end - tag->start)
					if(!strncmp(t->word, tag->start, tag->end - tag->start))
						return 1;
			}
		}
	}
	return 0;
}

void startSearch(struct Term *t, sqlite3_stmt **stm) {
	int r;
	if(TERM_POS & t->flags) {
		ASSERTSQL(sqlite3_prepare_v2(db, 
				"SELECT pathId, start, end, tags " 
				"FROM spans " 
				"WHERE pathId = ? "
				"AND start <= ? "
				"AND end >= ? "
				"ORDER BY start DESC",
				-1, stm, 0));
		
		ASSERTSQL(sqlite3_bind_text(*stm, 1, t->path, -1, SQLITE_STATIC));
		ASSERTSQL(sqlite3_bind_int(*stm, 2, t->pos));
		ASSERTSQL(sqlite3_bind_int(*stm, 3, t->pos));
	} else {
		char *ftsQuery;
		ftsQuery = malloc(strlen(t->word) + 2);
		strcpy(ftsQuery, t->word);
		if(TERM_INCOMPLETE & t->flags) {
			strcat(ftsQuery, "*"); 
		}
		
		//debug("searching indexed: %s", ftsQuery);
		
		if(!strlen(t->word)) {
			ASSERTSQL(sqlite3_prepare_v2(db, 
					"SELECT pathId, start, end, tags, features " 
					"FROM spans ", 
					-1, stm, 0));
			return;
		}
		

		ASSERTSQL(sqlite3_prepare_v2(db, 
				"SELECT pathId, start, end, tags, features " 
				"FROM spans " 
				"WHERE tags MATCH ? ", 
				-1, stm, 0));
		
		ASSERTSQL(sqlite3_bind_text(*stm, 1, ftsQuery, -1, SQLITE_STATIC));
	}
}

void printSpan(struct Span *span, 
	const struct Term *terms, const struct Term *lastTerm) {
	// Count lines to make output grep-like
	const char *absPath;
	const char *path;
	const char *contents;
	char *contentsEnd;
	const char *line, *target, *nextLine;
	int lineNumber, r;
	char pathFromWd[MAX_PATH];
	const char *targetEnd, *bestLine = NULL;
	sqlite3_stmt *stm;

	ASSERTSQL(sqlite3_prepare_v2(db, "SELECT path FROM paths WHERE oid=?", 
			-1, &stm, 0));
	ASSERTSQL(sqlite3_bind_int64(stm, 1, span->pathId));
	r = sqlite3_step(stm);
	if(r == SQLITE_DONE) {
		debug("NO PATH %d", span->pathId);
		exit(1);
	}
	if(r != SQLITE_ROW) {
		debug("readPath: %d", r);
		return;
	}
	absPath = sqlite3_column_text(stm, 0);
	path = getPathFrom(pathFromWd, absPath, wdPath);
				
	// print the shortest path
	if(strlen(absPath) <= strlen(path))
		path = absPath;

	contents = loadWhole(path, &contentsEnd);
	if(!contents) {
		fprintf(stderr, "Can't load %s\n", path);
		return;
	}
				
	*contentsEnd = 0;
	lineNumber = 1;
	target = contents + span->start;
	targetEnd = contents + span->end;
	line = contents;
	bestLine = contents;
	do {
		nextLine = memchr(line, '\n', contentsEnd - line);
		if(!nextLine) {
			nextLine = contentsEnd;
			break;
		} else 
			nextLine++;
					
		if(nextLine > targetEnd)
			break;
					
		if(nextLine > target) {
			const struct Term *t = terms;
			char *p;
			for(; t < lastTerm; t++) {
				if(!(t->flags & TERM_POS))
					if(p = strstr(line, t->word))
						if(p < nextLine)
							break;
			}
			if(t < lastTerm)
				break;
		}
		lineNumber++;
		line = nextLine;
	} while(1);
			
	printf("%s:%d:", path, lineNumber);
	if(target >= line && target <= nextLine) {
		fwrite(line, 1, target - line, stdout);
		printf("<<");
	} else 
		target = line;

	if(targetEnd >= line && targetEnd <= nextLine) {
		fwrite(target, 1, targetEnd - target, stdout);
		printf(">>");
	} else 
		targetEnd = target;

	fwrite(targetEnd, 1, nextLine - targetEnd, stdout);
	// hm, where does newline come from
}

#define MAX_COMPLETION_COUNT 100 // TODO implement
#define MAX_RESULTING_SPAN_COUNT 50

#define SEARCH 1
#define COMPLETE 2
#define UPDATE 3
#define LSDEFS 4

char *dupPart(const char *start, const char *end) {
	char *r = malloc(end - start + 1);
	memcpy(r, start, end - start);
	r[end - start] = 0;
	return r;
}

int main(int argc, char **argv){
	int r;
	int mode = SEARCH;
	struct Term terms[50], *lastTerm = terms, *indexTerm = NULL, 
		*completionTargetTerm;

	static struct option longOpts[] = {
		{"complete", required_argument, 0, 'C'},
		{"update", no_argument, 0, 'u'},
		{"lsdefs", required_argument, 0, 'l'},
		{"wheredb", no_argument, 0, 'W'},
		{0, 0, 0, 0}
	};
	int c, longOptIndex = 0;
	char *srcPathArg = NULL;
	int completionTargetIndex = 0;
	int verbose = 0;

	if(!getcwd(wdPath, sizeof dbPath - sizeof dbName - 2)) {
		fprintf(stderr, "Can't getcwd\n");
		exit(1);
	}
	normalizePath(wdPath);

	while(c = getopt_long(argc, argv, "Cvul:", longOpts, &longOptIndex), c != -1) {
		switch(c) {
		case 'C':
			mode = COMPLETE;
			completionTargetIndex = atoi(optarg);
			break;

		case 'u':
			mode = UPDATE;
			break;

		case 'l':
			mode = LSDEFS;
			lastTerm = &terms[0];
			lastTerm->flags = TERM_POS;
			lastTerm->path = optarg;
			break;

		case 'W':
			if(findDb())
				printf("%s\n", dbPath);
			exit(0);
			break;

		case 'v':
			verbose = 1;
			break;			
		}
	}


	if(UPDATE == mode) {
		if(!findDb()) {
			strcpy(dbDirPath, wdPath);
			strcpy(dbPath, wdPath);
			strcat(dbPath, "/");
			strcat(dbPath, dbName);
		}

		update(argv + optind);
	} else  {
		struct Span *span, *leaves = NULL;
		char *ftsQuery;

		// --complete really should be as time-sensitive as we can get,
		// because I get really annoyed when I press TAB and bash goes
		// god knows where
		if(!findDb()) {
			if(SEARCH == mode) {
				fprintf(stderr, "No database found!\n");
				return 1;
			} else {
				// Autocompletion is notoriously noninteractive process, so I think
				// we have to take every chance to send out a message we can get
				puts("--create-index-because-it-s-not-found");
				return 0;
			}
		}
		
		r = sqlite3_open(dbPath, &db);
		if(r) {
			fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
			sqlite3_close(db);
			exit(1);
		}

		if(LSDEFS == mode) {
			sqlite3_stmt *stm;
			struct Span span;
			int started = 0;

			if(optind > argc) {
				fprintf(stderr, "list definitions: no position in file given\n");
				exit(1);
			}
			
			lastTerm->pos = atoi(argv[optind]);
			startSearch(lastTerm, &stm);
			while(readSpan(&span, stm)) {
				struct Word *w = splitTags(&span);
				for(; w < span.tagsEnd; w++) {
					if(w->end - w->start == 1)
						if(w->start[0] == 'd') {
							if(started)
								printf("/");
							printf("%s", span.tagsText);
							started = 1;
							break;
						}
				}
			}
			puts("");
			return 0;
		} 


		/*
			Read search query from command line
		*/

		if(COMPLETE == mode) {
			completionTargetIndex += optind;
			optind++; // program name comes first
		}

		for(; optind < argc; optind++) {
			static const char punctuation[] = ":;,`~!()-=\\| ";
			//static const char nonBreakable[] = " .";
			char *e = argv[optind], *s; 
			char *feature = NULL;
			
			if(s = strrchr(e, ':')) {
				// path/name:1234 is a character number which must be inside resulting span 
				char *numEnd;
				lastTerm->pos = strtol(s + 1, &numEnd, 10);
				if(0 == *numEnd) {
					lastTerm->path = malloc(s - e + 1);
					memcpy(lastTerm->path, e, s - e);
					lastTerm->path[s - e] = 0;
					debug(lastTerm->path);
					lastTerm->flags = TERM_POS;
					lastTerm++;
					continue;
				}

				// also, d:getUser will get you spans tagged "getUser" having feature "d",
				// which should mean "definition"
				if(s - e) {
					feature = dupPart(e, s);
					e = s + 1;
				}
			} 
			
			do {
				s = e + strspn(e, punctuation);
				e = strpbrk(s, punctuation);
				if(!e) 
					e = strchr(s, 0);
				if(e - s) {
					lastTerm->flags = 0;
					if('*' == e[-1]) {
						e--;
						lastTerm->flags |= TERM_INCOMPLETE;
					}
					lastTerm->word = malloc(e - s + 1);
					lastTerm->arg = argv[optind];
					lastTerm->argPos = s;
					lastTerm->argEnd = e;
					memcpy(lastTerm->word, s, e - s);
					lastTerm->word[e - s] = 0;
					if(feature) {
						lastTerm->feature = feature;
						feature = NULL;
					}

					lastTerm++;
					if('*' == *e) 
						e++;

				}
			} while(e - s);

			if(completionTargetIndex == optind) {
				completionTargetTerm = NULL;
				if(lastTerm > terms) {
					completionTargetTerm = lastTerm - 1;
					// XXX check term mode
					if(*completionTargetTerm->argEnd) // space after word
						completionTargetTerm = NULL;
				}
				
				if(!completionTargetTerm) {
					completionTargetTerm = lastTerm;
					lastTerm->flags = 0;
					lastTerm->arg = argv[optind];
					lastTerm->argEnd = lastTerm->argPos = strrchr(lastTerm->arg, 0);
					lastTerm->word = strdup("");
					lastTerm++;
				}
				completionTargetTerm->flags |= TERM_INCOMPLETE;
			}
		}
	

		if(lastTerm == terms) {
			fputs("No query", stderr);
			exit(1);
		}

		
		/*
			Look into index
		*/

		for(indexTerm = &terms[0]; indexTerm < lastTerm; indexTerm++ ) {
			struct Span *span, *betterLeaves = 0;
			int count = 0;
			startSearch(indexTerm, &indexTerm->stm);

			while(readSpan(span = malloc(sizeof(*span)), indexTerm->stm)) {
				span->parent = NULL;
				span->tagsText = strdup(span->tagsText);
				span->featuresText = strdup(span->featuresText);

				if(!termAllowsSpan(indexTerm, span))
					continue;

				if(&terms[0] == indexTerm) {
					span->particular = betterLeaves;
					betterLeaves = span;
					count++;
				} else {
					struct Span *leaf = leaves;
					while(leaf) {
						if(span->pathId == leaf->pathId) {
							if(span->start < leaf->start && span->end > leaf->end) {
								// span is outside
								span->parent = leaf;
								span->particular = betterLeaves;
								betterLeaves = span;
								count++;
								break;
							}	else if(span->start >= leaf->start && span->end <= leaf->end) {
								// span is inside or at the same place
								span->parent = leaf;
								span->particular = betterLeaves;
								betterLeaves = span;
								count++;
								break;
							}
						}
						leaf = leaf->particular;
					}
				}
			}

			//debug("%s: %d", indexTerm->word, count);
			//free()
			leaves = betterLeaves;
		}
				
		span = leaves;
		while(span) {
			if(SEARCH == mode) {
				printSpan(span, terms, lastTerm);

				if(verbose) {
					struct Span *other = span;
					printf("which is %s: %s\n", span->featuresText, span->tagsText);
					if(other->parent)
						puts("also:");
					while(other = other->parent) {
						printSpan(other, terms, lastTerm);
						printf("which is %s: %s\n", other->featuresText, other->tagsText);
					}
					puts("");
				}
			} else { // we need to complete prefix
				struct Word *tag;
				struct Span *s = span;
				while(s) {
					for(tag = splitTags(span); tag < span->tagsEnd; tag++) {
						if(!strncmp(completionTargetTerm->word, tag->start, 
								strlen(completionTargetTerm->word))) {
							fwrite(completionTargetTerm->arg, 1, 
								completionTargetTerm->argPos - completionTargetTerm->arg, stdout);
							fwrite(tag->start, 1, tag->end - tag->start, stdout);
							puts(completionTargetTerm->argEnd);
						}
					}
					s = s->parent;
				}
			}
			span = span->particular;
		}

	}

  return 0;
}

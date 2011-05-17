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
# define MAX_PATH 1000
#endif

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

#define PROGNAME "ltags"

const char *dbName = "." PROGNAME ".sqlite";
char wdPath[MAX_PATH];
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
	pf->path = strdup(path);
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
		addTagToCurrentSpan(pf, F_FEATURE, F_FEATURE + 1);

		assert(strchr(pf->path, '/'));
		ws = we = strrchr(pf->path, 0);
		while(--ws) {
			if(!(CHAR_TOKENMIDDLE & classifyChar(*ws))) {
				
				if(we - ws > 1)
					addTagToCurrentSpan(pf, ws+1, we);
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
					"path   VARCHAR(256), "
					"mtime  INTEGER, "
					"start  INTEGER, "
					"end    INTEGER, "
					"status INTEGER, "
					"tags   TEXT)",
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
			char path[PATH_MAX];
			run("BEGIN");

			ASSERTSQL(sqlite3_prepare_v2(db, 
					"UPDATE spans SET status=1 WHERE path=?", -1, &stm, 0));
			for(; *srcPaths; srcPaths++) {
				ASSERTSQL(sqlite3_reset(stm));
				ASSERTSQL(sqlite3_bind_text(stm, 1, *srcPaths, -1, SQLITE_STATIC));
				if(SQLITE_DONE != sqlite3_step(stm))
					debug("Invalidating not done");
				normalizePath(*srcPaths);
				updateFile(realpath(*srcPaths, path));
			}
		} else {
			debug("Invalidating old entries...");
			run("UPDATE spans SET status=1"); // 1 means "questionable"
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
	s->path = sqlite3_column_text(stm, 0);
	s->start = sqlite3_column_int(stm, 1);
	s->end = sqlite3_column_int(stm, 2);
	s->tagsText = (char*)sqlite3_column_text(stm, 3); // not const 
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

struct Term {
	int flags;
	union {
		char *word;
		struct {
			unsigned pos;
			char *path;
		};
	};
	sqlite3_stmt *stm;
};


int checkTerm(struct Term *t, struct Span *s) {
	if(TERM_POS & t->flags) {
		if(t->pos >= s->start)
			if(t->pos <= s->end)
				if(!strcmp(t->path, s->path))
					return 1;
	} else {
		struct Word *tag = &s->tags[0];
		for(; tag < s->tagsEnd; tag++) {
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
				"SELECT path, start, end, tags " 
				"FROM spans " 
				"WHERE path = ? "
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

		ASSERTSQL(sqlite3_prepare_v2(db, 
				"SELECT path, start, end, tags " 
				"FROM spans " 
				"WHERE tags MATCH ? ", 
				-1, stm, 0));
		
		ASSERTSQL(sqlite3_bind_text(*stm, 1, ftsQuery, -1, SQLITE_STATIC));
	}
}	

#define SEARCH 1
#define COMPLETE 2
#define UPDATE 3
#define LSDEFS 4

int main(int argc, char **argv){
	int r;
	sqlite3_stmt *stm;
	int mode = SEARCH;
	struct Term terms[50], *lastTerm = terms, *indexTerm = 0, *completionTargetTerm;
	char *srcPathArg = NULL;

	static struct option longOpts[] = {
		{"complete", required_argument, 0, 'C'},
		{"update", no_argument, 0, 'u'},
		{"lsdefs", required_argument, 0, 'l'},
		{"wheredb", no_argument, 0, 'W'},
		{0, 0, 0, 0}
	};

	int c, optInd = 0;

	int completionTargetIndex = 0;

	char *p;
	
	if(!getcwd(wdPath, sizeof dbPath - sizeof dbName - 2)) {
		fprintf(stderr, "Can't getcwd");
		exit(1);
	}

	normalizePath(wdPath);

	while(c = getopt_long(argc, argv, "ul:", longOpts, &optInd), c != -1) {
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

			
		}
	}


	if(UPDATE == mode) {
		if(!findDb()) {
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

		if(COMPLETE == mode) {
			completionTargetIndex += optind;
			optind++; // program name comes first
		}


		for(; optind < argc; optind++) {
			static const char punctuation[] = ":;,`~!()-=\\| ";
			//static const char nonBreakable[] = " .";
			char *e = argv[optind], *s; 
			
			// path/name:1234 is a character number which must be inside resulting span 
			if(s = strrchr(e, ':')) {
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
					memcpy(lastTerm->word, s, e - s);
					lastTerm->word[e - s] = 0;
					lastTerm++;
					if('*' == *e) 
						e++;
				}
			} while(e - s);

			if(completionTargetIndex == optind) {
				completionTargetTerm = lastTerm - 1;
				completionTargetTerm->flags |= TERM_INCOMPLETE;
			}
		}

		if(lastTerm == terms) {
			fputs("No query", stderr);
			exit(1);
		}

		for(indexTerm = &terms[0]; indexTerm < lastTerm; indexTerm++ ) {
			struct Span *span, *betterLeaves = 0;
			int count = 0;
			startSearch(indexTerm, &indexTerm->stm);

			while(readSpan(span = malloc(sizeof(*span)), indexTerm->stm)) {
				span->tagsText = strdup(span->tagsText);
				span->path = strdup(span->path);
				span->parent = NULL;

				if(&terms[0] == indexTerm) {
					span->particular = betterLeaves;
					betterLeaves = span;
					count++;
				} else {
					struct Span *leaf = leaves;
					while(leaf) {
						int matched = 0;
						if(!strcmp(span->path, leaf->path)) {
							if(span->start < leaf->start && span->end > leaf->end) {
								// span is outside
								//debug("'%s' outside '%s'", span->tagsText, leaf->tagsText);
								span->parent = leaf;
								span->particular = betterLeaves;
								betterLeaves = span;
								matched = 1;
								count++;
								break;
							}	else if(span->start >= leaf->start && span->end <= leaf->end) {
								// span is inside or at the same place
								//debug("'%s' inside or eq '%s'", span->tagsText, leaf->tagsText);
								span->parent = leaf;
								span->particular = betterLeaves;
								betterLeaves = span;
								matched = 1;
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
			char pathFromWd[MAX_PATH];
			const char *path;
			const char *contents;
			char *contentsEnd;
			const char *line, *target, *nextLine;
			int lineNumber;

			/*struct Term *term = terms;
			int allTermsSatisfied = 1;
			for(; term < lastTerm; term++) {
				if(!checkTerm(term, &span)) {
					allTermsSatisfied = 0;
					break;
				}
			}
			if(!allTermsSatisfied)
			continue;*/

			// Count lines to make output grep-like
			
			if(SEARCH == mode) {
				char *targetEnd, *bestLine = NULL;
				path = getPathFrom(pathFromWd, span->path, wdPath);
				
				// print the shortest path
				if(strlen(span->path) <= strlen(path))
					path = span->path;

				contents = loadWhole(path, &contentsEnd);
				if(!contents) {
					fprintf(stderr, "Can't load %s\n", path);
					continue;
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
						struct Term *t = terms;
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
			} else { // we need to complete prefix
				struct Word *tag;
				struct Span *s = span;
				while(s) {
					for(tag = splitTags(span); tag < span->tagsEnd; tag++) {
						if(!strncmp(completionTargetTerm->word, tag->start, 
								strlen(completionTargetTerm->word))) {
							fwrite(tag->start, 1, tag->end - tag->start, stdout);
							fputs(" ", stdout);
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

#include <stdio.h>
#include <sys/stat.h>

#include "ext/sqlite/sqlite3.h"
#include "ext/dirent.h"
#include "ext/getopt/getopt.h"

void debug(const char *s) {
	fprintf(stderr, "%s\n", s);
}

sqlite3 *db;

static int callback(void *NotUsed, int argc, char **argv, char **azColName){
  int i;
  for(i=0; i<argc; i++){
    printf("%s = %s\n", azColName[i], argv[i] ? argv[i] : "NULL");
  }
  printf("\n");
  return 0;
}

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

struct Span {
	char *name, *nameEnd;
	int start, end;
	int mtime;
	char *path;
	char *options;
	struct Span *parent;
	void *particular;
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
	void *langParserState;
};

// Let's be consistent with a metaphor of a tree
#define UP '/' 

// hyperoptimized

#define CHAR_TOKENMIDDLE 1
#define CHAR_TOKENSTART  2
#define CHAR_SPACE       4

const int charFlags[/*256*/128] = {
	4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, // 00
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 10
	4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 20
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, // 30
	0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, // 40
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 3, 0, // 50
	0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, // 60
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, // 70
};

#define countof(something_M) (sizeof(something_M)/sizeof(something_M[0]))

int loadSpan(struct Span *pSpan) {
	return 1;
}

void saveSpan(const struct Span *pSpan) {
	int r;
	sqlite3_stmt *stm;
	static const char *update = "UPDATE spans SET status=0 WHERE " 
		"name=? AND path=? AND start=? AND end=?",
		*insert = "INSERT INTO spans (name, path, start, end, mtime, status) "
		"VALUES (?, ?, ?, ?, ?, 0)";
	const char *op = update;
	
	while(1) {
		r = sqlite3_prepare_v2(db, op, -1, &stm, 0);
		if(r != SQLITE_OK) { 
			fprintf(stderr, "prep (%s): %d\n", op, r);
			exit(1);
		}

		r = sqlite3_bind_text(stm, 1, pSpan->name, pSpan->nameEnd - pSpan->name, 
			SQLITE_STATIC);
		if(r != SQLITE_OK) { 
			fprintf(stderr, "bind name: %d\n", r);
			exit(1);
		}

		r = sqlite3_bind_text(stm, 2, pSpan->path, -1, 
			SQLITE_STATIC);
		if(r != SQLITE_OK) { 
			fprintf(stderr, "bind path: %d\n", r);
			exit(1);
		}

		r = sqlite3_bind_int(stm, 3, pSpan->start);
		if(r != SQLITE_OK) { 
			fprintf(stderr, "bind start: %d\n", r);
			exit(1);
		}

		r = sqlite3_bind_int(stm, 4, pSpan->end);
		if(r != SQLITE_OK) { 
			fprintf(stderr, "bind end: %d\n", r);
			exit(1);
		}

		if(insert == op) {
			r = sqlite3_bind_int(stm, 4, pSpan->mtime);
			if(r != SQLITE_OK) { 
				fprintf(stderr, "bind mtime: %d\n", r);
				exit(1);
			}
		}
			

		// TODO LOCK

		r = sqlite3_step(stm);
		if(r != SQLITE_DONE) {
			fprintf(stderr, "step: %d\n", r);
			exit(1);
		}

		r = sqlite3_changes(db);
		
		// TODO UNLOCK

		if(r)
			return;

		op = insert;
	}
}
	

struct JavaParserState {
	char *probableName, *probableNameEnd;
};

#define THIS ((struct JavaParserState*)(pFile->langParserState))

static void startParsingJavaSrc(struct File *pFile) {
	THIS = calloc(sizeof *THIS, 1);
}

#define NEW 1

static int getJavaReservedWordIndex(char *word, char *end) {
	if(!strncmp(word, "new", end - word))
		return NEW;
	return 0;
}

struct Span *startSpan(struct File *pf, const char *name, const char *nameEnd,
	const char *start) {
	struct Span *s = malloc(sizeof (struct Span));
	s->name = name;
	s->nameEnd = nameEnd;
	s->path = pf->path;
	s->mtime = pf->mtime;
	s->start = start - pf->contents;
	s->end = 0;

	s->parent = pf->currentSpan;
	pf->currentSpan = s;
	return s;
}

struct Span *finishLastSpan(struct File *pf, const char *end) {
	struct Span *s = pf->currentSpan;
	s->end = end - pf->contents;
	saveSpan(s);
	pf->currentSpan = s->parent;
	free(s);
	return pf->currentSpan;
}

static void parseJavaWord(struct File *pf) {
	char bf[121], *p = bf, *start = pf->token;
	while(start < pf->tokenEnd)
		*p++ = *start++;
	*p = 0;
	puts(bf);

	startSpan(pf, pf->token, pf->tokenEnd, pf->token);
	finishLastSpan(pf, pf->tokenEnd);
}

static void parseJavaPunctuation(struct File *pFile, const char *p) {
	struct Span *newSpan;
	if('{' == *p) {
	}
}



#define SPACE         0
#define TOKEN         1
#define NUMBER        2
#define COMMENT_LINE  '\n'
#define COMMENT_LINES '*'

void parseJava(const char *path) {
	FILE *fp;
	struct File file;
	struct stat st;
	int rc;
	char bf[2 * MAX_PATH], *name, *nameEnd;
	sqlite3_stmt *stm;
	int storedTime = 0;

	char *p;
	int mode, flags;
	
	rc = stat(path, &st);
	if(rc < 0) {
		debug("Can't stat");
		return;
	}

	name = strrchr(path, UP);
	nameEnd = strchr(name, '.');
	rc = sqlite3_prepare_v2(db, "SELECT mtime FROM spans WHERE name = ?", -1,
		&stm, 0);
	if(rc != SQLITE_OK) {
		fprintf(stderr, "prepare: %d\n", rc);
		return;
	}

	rc = sqlite3_bind_text(stm, 1, name, nameEnd - name, SQLITE_TRANSIENT);
	if(rc != SQLITE_OK) {
		fprintf(stderr, "bind: %d\n", rc);
		return;
	}

	rc = sqlite3_step(stm);
	if(SQLITE_ROW == rc) {
		storedTime = sqlite3_column_int(stm, 0);
	} else if(rc != SQLITE_DONE) {
		fprintf(stderr, "step: %d\n", rc);
		return;
	}
	
	if(storedTime == st.st_mtime) {
		debug("Skipping:");
		debug(path);
		return;
	}

	fp = fopen(path, "r");
	if(!fp) {
		fprintf(stderr, "Can't fopen %s\n", path);
		goto end;
	}

	file.path = path;
	file.mtime = st.st_mtime;

	file.contents = malloc(st.st_size + 1);
	if(!file.contents) {
		fprintf(stderr, "No memory to load %s\n", path);
		goto end;
	}

	debug("Loading:");
	debug(path);

	fread(file.contents, 1, st.st_size, fp);
	file.contentsEnd = file.contents + st.st_size;
	*file.contentsEnd = '\n'; //padding

	file.token = p = file.contents;
	mode = SPACE;
	flags = 0;
	for(;;p++) {
		switch(mode) {
		case '\'':
		case '"':
			// p[-1] is valid here, bc we must have entered this mode by
			// reading some characters
			if(mode == *p) {
				if('\\' != p[-1]) 
					mode = SPACE;
			}	else if(file.contentsEnd == p)
				goto end;
			break;
					
			
		case COMMENT_LINE:
			if('\n' == *p) {
				if(file.contentsEnd == p)
					goto end;
				mode = SPACE;
			}
			break;

		case COMMENT_LINES:
			if('*' == *p) {
				// p[1] is valid here, bc last valid byte is padded '\n'
				if('/' == p[1])
					mode = SPACE;
			} else if (file.contentsEnd == p) 
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
				file.tokenEnd = p;
				parseJavaWord(&file);
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
			
			if(*p >= countof(charFlags))
				flags = CHAR_TOKENSTART | CHAR_TOKENMIDDLE;
			else 
				flags = charFlags[*p];
			
			if(CHAR_TOKENSTART & flags) {
				file.token = p;
				mode = TOKEN;
			} else if(CHAR_TOKENMIDDLE & flags) {
				mode = NUMBER;
			} else if(CHAR_SPACE & flags) {
				if(file.contentsEnd == p)
					goto end;
				break;
			} else 
				parseJavaPunctuation(&file, p);
			
		} // switch(mode)
	} // for(;;p++)
		
		
	
 end:
	if(fp)
		fclose(fp);
}


void updateDir(char *path) {
	char *end= strrchr(path, 0), *suffix;
	DIR *d = opendir(path);
	struct dirent *entry;

	if(!d) {
		debug("Can't opendir:");
		debug(path);
		return;
	}
	
	*end++ = UP;
	while(entry = readdir(d)) {
		if('.' == entry->d_name[0])
			continue;
		
		if(S_ISDIR(entry->d_type)) {
			strncpy(end, entry->d_name, MAX_PATH - (end - path));
			updateDir(path);
			continue;
		}

		strncpy(end, entry->d_name, MAX_PATH - (end - path));
		suffix = strrchr(end, 0) - 5;
		if(suffix > path && !stricmp(suffix, ".java")) {
			parseJava(path);
		}
	}	
}

void update() {
  int rc;
	char curPath[MAX_PATH];
	
  rc = sqlite3_open(".ltags.sqlite", &db);
  if( rc ){
    fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
    sqlite3_close(db);
    exit(1);
  }

	//run("CREATE VIRTUAL TABLE regions USING rtree(id, start, end)");
	run("CREATE TABLE IF NOT EXISTS spans("
		"name   VARCHAR(100), "
		"path   VARCHAR(256), "
		"mtime  INTEGER, "
		"start  INTEGER, "
		"end    INTEGER, "
		"status INTEGER)");
	
	debug("Invalidating old entries...");
	run("UPDATE spans SET status=1"); // 1 means "questionable"
	run("BEGIN");
	strcpy(curPath, ".");
	updateDir(curPath);
	debug("Commit...");
	run("COMMIT");
	debug("Deleting old entries...");
	run("DELETE FROM spans WHERE status=1");


  sqlite3_close(db);
}

const char *dbPath = ".ltags.sqlite";

int main(int argc, char **argv){
	int r;
	sqlite3_stmt *stm;

	static struct option longOpts[] = {
		{0, 0, 0, 0}
	};

	int c, optInd = 0;
	while(c = getopt_long(argc, argv, "", longOpts, &optInd), c != -1) {

	}

	if(optind == argc) {
		update();
	} else {
		struct stat st;
		if(0 > stat(dbPath, &st))
			update();
		
		r = sqlite3_open(dbPath, &db);
		if( r ){
			fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
			sqlite3_close(db);
			exit(1);
		}

		r = sqlite3_prepare_v2(db, 
			"SELECT name, path, start, end " 
			"FROM spans " 
			"WHERE name = ? "
			, 
			-1, &stm, 0);
		if(r != SQLITE_OK) {
			fprintf(stderr, "No prepare: %d\n", r);
			exit(1);
		}

		r = sqlite3_bind_text(stm, 1, argv[optind], -1, SQLITE_STATIC);
		if(r != SQLITE_OK) {
			fprintf(stderr, "No bind: %d\n", r);
			exit(1);
		}

		while(r = sqlite3_step(stm), r == SQLITE_ROW) {
			const char *path, *name;
			int start, end;
			name = sqlite3_column_text(stm, 0);
			path = sqlite3_column_text(stm, 1);
			start = sqlite3_column_int(stm, 2);
			end = sqlite3_column_int(stm, 3);
			printf("%s %s %d %d\n", name, path, start, end);
		}

		if(r != SQLITE_DONE) {
			fprintf(stderr, "No step: %d", r);
			exit(1);
		}
	}

  return 0;
}

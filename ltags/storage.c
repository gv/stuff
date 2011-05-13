#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "storage.h"
#include "ext/sqlite/sqlite3.h"

extern sqlite3 *db;

int loadSpan(struct Span *pSpan) {
	return 1;
}
 
#ifdef _WIN32
# define __thread __declspec(thread)
#endif

__thread sqlite3_stmt *spanUpdateStm, *spanInsertStm;

void initStorageThread() {
	int r;
	ASSERTSQL(sqlite3_prepare_v2(db, 
			"UPDATE spans SET status=0 WHERE " 
			"path=? AND start=? AND end=? AND tags MATCH ?",
			-1, &spanUpdateStm, 0));
	ASSERTSQL(sqlite3_prepare_v2(db,
			"INSERT INTO spans (path, start, end, tags, mtime, status) "
			"VALUES (?, ?, ?, ?, ?, 0)",
			-1, &spanInsertStm, 0));
}

void saveSpan(const struct Span *pSpan) {
	int r;
	sqlite3_stmt *stm;
	char text[MAX_TAG_CNT * 32], *tail =  text, *nextTail = text;
	const struct Word *w;
	
	w = pSpan->tags;
	while(w < pSpan->tagsEnd) {
		nextTail = tail + (w->end - w->start) + 1;
		if(nextTail > text + sizeof text) {
			debug("SPAN TEXT OVERFLOW");
			break;
		}

		memcpy(tail, w->start, w->end - w->start);
		tail = nextTail;
		tail[-1] = ' ';
		w++;
	}
	tail[-1] = 0;
	//debug("Saving span: %s", text);
	
	stm = spanUpdateStm;
	while(1) {
		ASSERTSQL(sqlite3_reset(stm));
		ASSERTSQL(sqlite3_bind_text(stm, 1, pSpan->path, -1, SQLITE_STATIC));
		ASSERTSQL(sqlite3_bind_int(stm, 2, pSpan->start));
		ASSERTSQL(sqlite3_bind_int(stm, 3, pSpan->end));
		ASSERTSQL(sqlite3_bind_text(stm, 4, text, -1, SQLITE_STATIC));

		if(spanInsertStm == stm) {
			ASSERTSQL(sqlite3_bind_int(stm, 5, pSpan->mtime));
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
		
		stm = spanInsertStm;
	}
}
	

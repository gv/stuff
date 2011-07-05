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
			"pathId=? AND start=? AND end=? AND weight=? AND features=? AND tags MATCH ?",
			-1, &spanUpdateStm, 0));
	ASSERTSQL(sqlite3_prepare_v2(db,
			"INSERT INTO spans "
			"(pathId, start, end, weight, features, tags, mtime, status) "
			"VALUES (?, ?, ?, ?, ?, ?, ?, 0)",
			-1, &spanInsertStm, 0));
}

void saveSpan(const struct Span *pSpan) {
	int r;
	sqlite3_stmt *stm;
	char text[MAX_TAG_CNT * 32], *tail =  text, *nextTail = text;
	const char **pft;
	char *featuresText = NULL;
	const struct Word *w;
	int featuresTextLen = 0;

	for(pft = pSpan->features; pft < pSpan->endOfFeatures; pft++)
		featuresTextLen += strlen(*pft) + 1;
	if(featuresTextLen) {
		featuresText = malloc(featuresTextLen); // or die
		featuresText[0] = 0;
		for(pft = pSpan->features; pft < pSpan->endOfFeatures; pft++) {
			strcat(featuresText, *pft);
			strcat(featuresText, " ");
		}
		featuresText[featuresTextLen - 1] = 0;
	}
	
	w = pSpan->tags;
	tail = text;
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
		sqlite3_reset(stm);
		ASSERTSQL(sqlite3_bind_int64(stm, 1, pSpan->pathId));
		ASSERTSQL(sqlite3_bind_int(stm, 2, pSpan->start));
		ASSERTSQL(sqlite3_bind_int(stm, 3, pSpan->end));
		ASSERTSQL(sqlite3_bind_int(stm, 4, pSpan->weight));
		ASSERTSQL(sqlite3_bind_text(stm, 5, 
				featuresText ? featuresText : "", featuresTextLen,
				SQLITE_STATIC));
		ASSERTSQL(sqlite3_bind_text(stm, 6, text, -1, SQLITE_STATIC));

		if(spanInsertStm == stm) {
			ASSERTSQL(sqlite3_bind_int(stm, 7, pSpan->mtime));
		}

		sqlite3_mutex_enter(sqlite3_db_mutex(db));

		r = sqlite3_step(stm);
		if(r != SQLITE_DONE) {
			// TODO get rid of fts3
			debug("save span: %d (%s)", r,  sqlite3_errmsg(db));
		}

		r = sqlite3_changes(db);
		
		sqlite3_mutex_leave(sqlite3_db_mutex(db));

		if(r)
			break;
		
		stm = spanInsertStm;
	}

	free(featuresText);
}
	

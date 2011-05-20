#include <stdio.h>
#include "ltags/ext/sqlite/sqlite3.h"

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

#define ASSERTSQL(expr_M) do{																						\
		r = expr_M; if(r != SQLITE_OK) {																		\
			fprintf(stderr, "SQLite assertion fail: '%s', code %d\n", #expr_M, r); \
			exit(1);																													\
		}}while(0)
 

const char dbPath[] = "demo.sqlite";

int main(int argc, char **argv) {
	int r;
	sqlite3_stmt *stm = 0;

	r = sqlite3_open(dbPath, &db);
	if(r) {
		fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
		sqlite3_close(db);
		exit(1);
	}

	if(argc > 1) {
		printf("making...\n");
		ASSERTSQL(sqlite3_prepare_v2(db, 
				"CREATE TABLE spans ("
				"path   VARCHAR(256), "
				"name   VARCHAR(256), "
				"status INTEGER)",
				-1, &stm, 0));
	
		r = sqlite3_step(stm);
		if(r != SQLITE_DONE) {
			printf("Table creation step: %d\n", r);
		}
		ASSERTSQL(sqlite3_finalize(stm));

		run("INSERT INTO spans VALUES('blow', 'grip', 3748)");
		run("INSERT INTO spans VALUES('floating', 'sister', 5)");
		run("INSERT INTO spans VALUES('macaroni', 'clever', 84)");
		run("INSERT INTO spans VALUES('blow', 'apartment', 40)");
		
		run("CREATE INDEX plum ON spans(path)");
		run("CREATE INDEX bird ON spans(name)");
	} else {
		ASSERTSQL(sqlite3_prepare_v2(db, 
				"SELECT status FROM spans WHERE path = 'blow' AND name='grip'",
				-1, &stm, 0));
		
		while(r = sqlite3_step(stm), r == SQLITE_ROW) {
			printf("%d\n", sqlite3_column_int(stm, 0));
		}	
		ASSERTSQL(sqlite3_finalize(stm));
	}
	sqlite3_close(db);
}
	

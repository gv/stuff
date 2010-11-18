#include <stdio.h>
#include "ext/sqlite/sqlite3.h"
#include "ext/dirent.h"


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
  char *zErrMsg = 0;
  rc = sqlite3_exec(db, stmt, callback, 0, &zErrMsg);
  if( rc!=SQLITE_OK ){
    fprintf(stderr, "SQL error: %s\n", zErrMsg);
    sqlite3_free(zErrMsg);
		exit(1);
  }
}

#define UP '/' // Let's be consistent with a metaphor of a tree
	
void update(char *rootPath) {
	char p[MAX_PATH];
	
}
	
int main(int argc, char **argv){
  int rc;


  rc = sqlite3_open(".ltags.sqlite", &db);
  if( rc ){
    fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
    sqlite3_close(db);
    exit(1);
  }

	//run("CREATE VIRTUAL TABLE regions USING rtree(id, start, end)");
	run("CREATE TABLE IF NOT EXISTS regions("
		"word, path, mtime, data)");
	
	
	
  sqlite3_close(db);
  return 0;
}

#include <stdio.h>
#include <sys/stat.h>

#include "ext/sqlite/sqlite3.h"
#include "ext/dirent.h"

void debug(const char *s) {
	fputs(s, stderr);
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

// Let's be consistent with a metaphor of a tree
#define UP '/' 

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
			puts(path);
		}
	}	
}
	
int main(int argc, char **argv){
  int rc;
	char curPath[MAX_PATH];

  rc = sqlite3_open(".ltags.sqlite", &db);
  if( rc ){
    fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
    sqlite3_close(db);
    exit(1);
  }

	//run("CREATE VIRTUAL TABLE regions USING rtree(id, start, end)");
	run("CREATE TABLE IF NOT EXISTS regions("
		"word, path, mtime, start, end, status)");
	
	strcpy(curPath, ".");
	updateDir(curPath);
  sqlite3_close(db);
  return 0;
}

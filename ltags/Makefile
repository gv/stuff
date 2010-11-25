B := build/

$(shell mkdir $(B))

CFLAGS := /Zi

ltags.exe: $(B)ltags.o $(B)sqlite3.o $(B)getopt.o $(B)getopt1.o
	link /DEBUG $(B)ltags.o $(B)sqlite3.o $(B)getopt.o $(B)getopt1.o

$(B)ltags.o: ltags.c
	cl /c $(CFLAGS) ltags.c /Fo$(B)ltags.o 

$(B)sqlite3.o: ext/sqlite/sqlite3.c
	cl /c $(CFLAGS) /DSQLITE_ENABLE_RTREE=1 ext/sqlite/sqlite3.c /Fo$(B)sqlite3.o 

$(B)getopt.o: ext/getopt/getopt.c 	
	cl /c $(CFLAGS) ext/getopt/getopt.c /Fo$(B)getopt.o 

$(B)getopt1.o: ext/getopt/getopt1.c 	
	cl /c $(CFLAGS) ext/getopt/getopt1.c /Fo$(B)getopt1.o 


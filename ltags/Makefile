BP := build/

$(shell mkdir $(BP))

ltags.exe: $(BP)ltags.o $(BP)sqlite3.o
	link $(BP)ltags.o $(BP)sqlite3.o

$(BP)ltags.o: ltags.c
	cl /c ltags.c /Fo$(BP)ltags.o 

$(BP)sqlite3.o: ext/sqlite/sqlite3.c
	cl /c /DSQLITE_ENABLE_RTREE=1 ext/sqlite/sqlite3.c /Fo$(BP)sqlite3.o 



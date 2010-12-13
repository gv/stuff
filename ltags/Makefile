# For GNU make

B = build/

$(shell mkdir $(B))

ifeq ($(OS), Windows_NT)
	CC = cl /c
	CFLAGS += /Zi
	CFLAGS += /Iext/pthread
	O = /Fo
	OB = /Fo$(B)
	X = .exe
	F = /
else 
	CC = gcc -c
	CFLAGS += -g
	O = -o 
	OB = -o $(B)
	F = -
endif

#   Rules go here

all: ltags$X

$Bltags.o: ltags.c queue.h tokenizer.h storage.h
	$(CC) $(CFLAGS) $FO5 ltags.c $O$@

$Bjava.o: java.c java_res.h tokenizer.c storage.h
	$(CC) $(CFLAGS) $FO5 java.c $O$@

$Btokenizer.o: tokenizer.c tokenizer.h storage.h
	$(CC) $(CFLAGS) $FO5 tokenizer.c $O$@

$Bstorage.o: storage.c storage.h
	$(CC) $(CFLAGS) $FO5 storage.c $O$@

$Bsqlite3.o: ext/sqlite/sqlite3.c
	$(CC) $(CFLAGS) $FO4 \
	$FDSQLITE_OMIT_LOAD_EXTENSION=1 \
	$FDSQLITE_OMIT_DEPRECATED=1 \
	$FDSQLITE_OMIT_UTF16=1 \
	$FDSQLITE_OMIT_EXPLAIN=1 \
	$FDSQLITE_OMIT_MEMORYDB=1 \
	$FDSQLITE_OMIT_AUTOVACUUM=1 \
	$FDSQLITE_OMIT_INCRBLOB=1 \
	$FDSQLITE_THREADSAFE=1 \
	$FDSQLITE_ENABLE_FTS3 \
	ext/sqlite/sqlite3.c $O$@


ifeq ($(OS), Windows_NT)
ltags$X: $Bltags.o $Bjava.o $Bstorage.o $Btokenizer.o \
	$Bsqlite3.o $Bgetopt.o $Bgetopt1.o \
	prebuilt/pthreadVC2.lib
	cmd /c copy prebuilt\pthreadVC2.dll .
	link /DEBUG $^

$Bgetopt.o: ext/getopt/getopt.c 	
	cl /c $(CFLAGS) ext/getopt/getopt.c /Fo$(B)getopt.o 

$Bgetopt1.o: ext/getopt/getopt1.c 	
	cl /c $(CFLAGS) ext/getopt/getopt1.c /Fo$(B)getopt1.o 
else 
ltags$X: $Bltags.o $Bjava.o $Bsqlite3.o $Bstorage.o $Btokenizer.o
	gcc -lm -lpthread $^ -o $@
endif


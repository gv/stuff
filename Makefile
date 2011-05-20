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

all: sqltdemo$X

$Bsqltdemo.o: sqltdemo.c
	$(CC) $(CFLAGS) sqltdemo.c $O$@

$Bsqlite3.o: ltags/ext/sqlite/sqlite3.c
	$(CC) $(CFLAGS) \
	$FDSQLITE_OMIT_LOAD_EXTENSION=1 \
	$FDSQLITE_OMIT_DEPRECATED=1 \
	$FDSQLITE_OMIT_UTF16=1 \
	$FDSQLITE_OMIT_EXPLAIN=1 \
	$FDSQLITE_OMIT_MEMORYDB=1 \
	$FDSQLITE_OMIT_AUTOVACUUM=1 \
	$FDSQLITE_OMIT_INCRBLOB=1 \
	$FDSQLITE_OMIT_SHARED_CACHE=1 \
	$FDSQLITE_OMIT_PROGRESS_CALLBACK=1 \
	$FDSQLITE_OMIT_TRACE=1 \
	$FDSQLITE_OMIT_UTF16=1 \
	$FDSQLITE_THREADSAFE=1 \
	ltags/ext/sqlite/sqlite3.c $O$@

sqltdemo$X: $Bsqltdemo.o $Bsqlite3.o 
	link /DEBUG $^


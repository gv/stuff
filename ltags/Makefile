# For GNU make

B = build/

$(shell mkdir $(B))

ifeq ($(OS), Windows_NT)
	CC = cl /c
	CFLAGS += /Zi
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

$Bltags.o: ltags.c
	$(CC) $(CFLAGS) ltags.c $O$@

$Bjava.o: java.c java_res.h
	$(CC) $(CFLAGS) java.c $O$@

$Bsqlite3.o: ext/sqlite/sqlite3.c
	$(CC) $(CFLAGS) $FDSQLITE_OMIT_LOAD_EXTENSION=1 \
	$FDSQLITE_ENABLE_FTS3 \
	ext/sqlite/sqlite3.c $O$@


ifeq ($(OS), Windows_NT)
ltags$X: $Bltags.o $Bjava.o $Bsqlite3.o $Bgetopt.o $Bgetopt1.o
	link /DEBUG $^

$Bgetopt.o: ext/getopt/getopt.c 	
	cl /c $(CFLAGS) ext/getopt/getopt.c /Fo$(B)getopt.o 

$Bgetopt1.o: ext/getopt/getopt1.c 	
	cl /c $(CFLAGS) ext/getopt/getopt1.c /Fo$(B)getopt1.o 
else 
ltags$X: $Bltags.o $Bjava.o $Bsqlite3.o
	gcc -lm -lpthread $^ -o $@
endif


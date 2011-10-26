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

kbmix$X: kbmix.c
	-taskkill /f /im $@ 
	cl $^ winmm.lib user32.lib gdi32.lib

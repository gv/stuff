c = libs
lib_cling_sources = $c/cling-core-1.0.2-sources.jar
lib_cling = $c/cling-core-1.0.2.jar
lib_teleal = $c/teleal-common-1.0.12.jar

ifeq ($(OS), Windows_NT)
classpath += $(lib_cling);$(lib_cling_sources);$(lib_teleal)
other.cmd: vg/scother/demo.class
	echo java -cp ".;$(classpath)"	 vg.scother.demo > $@

else
classpath += $(lib_cling):$(lib_cling_sources):$(lib_teleal)
other.sh: viewer.jar
	echo "#!/bin/sh" > $@
	echo java -jar $^ >> $@
	chmod +x $@
endif

classfiles = gen/vg/scother/demo.class gen/vg/Sc/NetworkNode.class

viewer.jar: $(classfiles)
	unzip -nq $(lib_cling) -d gen -x META-INF*
	unzip -nq $(lib_teleal) -d gen -x META-INF*
	jar cvfe $@ vg.scother.demo -C gen vg -C gen org 

$(classfiles): demo.java src/vg/Sc/NetworkNode.java
	javac -d gen -cp "$(classpath)" src/vg/Sc/NetworkNode.java demo.java

clean:
	rm -r vg


# demo

B=bin/

ifeq ($(OS), Windows_NT)
	CC = cl /c
	CCLINK = cl
	CFLAGS += /Zi
	CFLAGS += /Iext/pthread
#	CFLAGS += /Ox
	O = /Fo
	OB = /Fo$(B)
	X = .exe
	F = /
else 
	CC = gcc -c
	CCLINK = gcc
	CFLAGS += -g
	CFLAGS += -O5
	O = -o 
	OB = -o $(B)
	F = -
endif

process.exe: $Bprocess.o opencv_core231d.lib opencv_highgui231d.lib
	link /DEBUG $^ 

CV_INCLUDES += /Iopencv/opencv/modules/core/include
CV_INCLUDES += /Iopencv/opencv/modules/highgui/include

$Bprocess.o: process.cpp
	$(CC) /Zi /EHsc $(CV_INCLUDES) $^ $O$@


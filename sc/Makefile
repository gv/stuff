c = libs
classpath += $c/cling-core-1.0.2-sources.jar;$c/cling-core-1.0.2.jar;$c/teleal-common-1.0.12.jar

ifeq ($(OS), Windows_NT)
other.cmd: vg/scother/demo.class
	echo java -cp ".;$(classpath)"	 vg.scother.demo > $@

else
demo.sh: vg/scother/demo.class
	echo "#!/bin/sh" > demo.sh
	echo java -cp ".;$(classpath)" vg.scother.demo >> demo.sh
	chmod +x demo.sh
endif

vg/scother/demo.class: demo.java src/vg/Sc/NetworkNode.java
	javac -d . -cp "$(classpath)" src/vg/Sc/NetworkNode.java demo.java

clean:
	rm -r vg

c = ../../classes
classpath += $c/cling-core-1.0.2-sources.jar:$c/cling-core-1.0.2.jar:$c/teleal-common-1.0.12.jar

demo.sh: vg/scother/demo.class
	echo "#!/bin/sh" > demo.sh
	echo java -cp .:$(classpath) vg.scother.demo >> demo.sh
	chmod +x demo.sh

vg/scother/demo.class: demo.java src/vg/Sc/NetworkNode.java
	javac -d . -cp $(classpath) src/vg/Sc/NetworkNode.java demo.java

clean:
	rm -r vg

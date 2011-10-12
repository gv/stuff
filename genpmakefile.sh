#!/bin/sh

for i in {1..500}; do
	echo all: $i
	echo 
	echo $i:
	echo -e "\t$1 $2 $3 2>&1 |sed s/\$$/-$i/g"
	echo
done




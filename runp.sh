#!/bin/sh

for i in {1..500}; do
	command="$command ($1 $2 $3 2>&1|sed s/$/-$i/g) & "
done 

echo $command
eval "$command wait"

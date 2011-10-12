#!/bin/sh

for i in {1..640}; do
	command="$command ($@ 2>&1|sed s/$/-$i/g) & "
done 

echo $command
eval "$command wait"

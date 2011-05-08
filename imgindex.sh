#!/bin/sh

echo "<html><body>"
ls -rt|while read n; do
info=`ls -l $n`
    echo "<img src='$n' title='$info'/><br /><br />"
done
echo "</body></html>"

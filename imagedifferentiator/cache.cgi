#!/bin/sh

# Recursion can be kinda painful here I think.
if [ "$(echo "$QUERY_STRING" | grep "$SERVER_NAME")" ]; then
   filename="/message.jpg"
fi

if [ -z "$filename" ]; then
   imgsPath=images
   mkdir -p $imgsPath

   filename=$imgsPath/$(echo "$QUERY_STRING"|sed s/[/:?+]/_/g)
   if [ ! -r "$filename" ]; then
       wget "$QUERY_STRING" -O "$filename" >/dev/null
   	   touch $filename
   fi
fi

#echo 'Content-type: text/html'
#echo
#echo $filename

echo "Location: $filename"
echo

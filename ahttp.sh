$(dirname $0)/ffmpeg -i $1 -re -y -c:v libx264 \
	-vprofile baseline \
	-x264opts level=41 -threads 4 \
	-map 0:v -map 0:a:0 \
	-strict -2 -c:a aac -b:a 160000 -ac 2   -hls_time 10 \
	-hls_list_size 6 -hls_wrap 18 -start_number 1 stream.m3u8

#	-b:v BITRATE \
#	-preset medium \
#-s RESOLUTION \

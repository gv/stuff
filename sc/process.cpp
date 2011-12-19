#include <opencv2/core/core.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <stdio.h>
#include <math.h>

int main(int argc, char** argv) {
	if(argc <= 1) {
		fprintf(stderr, "arg!\n");
		return 1;
	}
		
	char *path = argv[1];
	IplImage *in = cvLoadImage(path);
	if(!in) {
		fprintf(stderr, "Can't load %s\n", path);
		return 1;
	}

	printf("depth: %d, channels: %d, order: %s\n", in->depth, in->nChannels, 
		in->channelSeq);

	IplImage* out=cvCreateImage(cvSize(in->width, in->height), 
		IPL_DEPTH_8U, 3);
	if(!out) {
		fprintf(stderr, "!create\n");
		return 1;
	}

	unsigned char *d = (unsigned char*)in->imageData;
	for(int x = 1; x < in->width - 1; x++) {
		for(int y = 0; y < in->height - 1; y++) {
			int rr, bb, gg;
			int r = d[x*in->nChannels + y*in->widthStep];
			int g = d[x*in->nChannels + y*in->widthStep + 1];
			int b = d[x*in->nChannels + y*in->widthStep + 2];

			rr = d[(x+1)*in->nChannels + y*in->widthStep];
			gg = d[(x+1)*in->nChannels + y*in->widthStep + 1];
			bb = d[(x+1)*in->nChannels + y*in->widthStep + 2];

			int right = abs(rr - r);// + abs(gg - g) + abs(bb - b);

			rr = d[(x-1)*in->nChannels + y*in->widthStep];
			gg = d[(x-1)*in->nChannels + y*in->widthStep + 1];
			bb = d[(x-1)*in->nChannels + y*in->widthStep + 2];

			int left = abs(rr - r);// + abs(gg - g) + abs(bb - b);

			rr = d[x*in->nChannels + (y+1)*in->widthStep];
			gg = d[x*in->nChannels + (y+1)*in->widthStep + 1];
			bb = d[x*in->nChannels + (y+1)*in->widthStep + 2];

			int bottom = abs(rr - r);// + abs(gg - g) + abs(bb - b);
			
			int vb, vg;
			if(abs(left - right) > bottom * 2) {
				vb = 255;//abs(right - left) * 2;
				vg = 0;
			} else if(bottom > right * 2){
				vg = 255;
				vb = 0;
			} else {
				vb = vg = 0;
			}
				
			out->imageData[x*out->nChannels + y*out->widthStep] = vb;
			out->imageData[x*out->nChannels + y*out->widthStep + 1] = vg;
			out->imageData[x*out->nChannels + y*out->widthStep + 2] = 0;
		}
	}

	if(!cvSaveImage("gram.bmp", out))
		fprintf(stderr, "not saved\n");
}

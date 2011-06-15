#include <stdlib.h>
#include <jni.h>

#include <ctype.h>
#include <opencv2/core/core.hpp>
#include <opencv2/features2d/features2d.hpp>

using namespace cv;

extern "C"
jintArray Java_vg_Sc_SessionList_findFeatures(JNIEnv* j, jobject jthis, 
	jbyteArray imgBytesArr, jint width) {
	jintArray r;
	jint *out;
	int i, ptCnt, height;
	jbyte *imgBytes;

	height = j->GetArrayLength(imgBytesArr) / width * 2 / 3;
	imgBytes = j->GetByteArrayElements(imgBytesArr, NULL);
	Mat greyMat(width, height, CV_8UC1, imgBytes);
	SurfFeatureDetector detector(100./*hessian_threshold*/, 1/*octaves*/, 
		2/*octave_layers*/);
	typedef vector<KeyPoint> Points;
	Points keyPoints;
	detector.detect(greyMat, keyPoints);
	
	j->ReleaseByteArrayElements(imgBytesArr, imgBytes, 0);

	ptCnt = keyPoints.size();
	r = j->NewIntArray(ptCnt * 3);
	out = j->GetIntArrayElements(r, NULL);
	jint* o = out;
	for(Points::iterator p = keyPoints.begin(); p < keyPoints.end(); p++) {
		*o++ = p->pt.x;
		*o++ = p->pt.y;
	}
	for(Points::iterator p = keyPoints.begin(); p < keyPoints.end(); p++) {
		*o++ = p->size;
	}
	j->ReleaseIntArrayElements(r, out, 0);
	return r;
}

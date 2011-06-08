#include <stdlib.h>
#include <jni.h>


extern "C"
jintArray Java_vg_Sc_MainScreen_findFeatures(JNIEnv* j, jobject jthis, 
	jbyteArray imgBytes, jint width) {
	jintArray r;
	jint *out;
	int i, ptCnt, height;

	height = j->GetArrayLength(imgBytes) / width;

	ptCnt = rand() % 30;
	r = j->NewIntArray(ptCnt * 2);
	out = j->GetIntArrayElements(r, NULL);
	for(i = 0; i < ptCnt; i++) {
		out[i*2] = rand() % width;
		out[i*2 + 1] = rand() % height;
	}
	j->ReleaseIntArrayElements(r, out, 0);
	return r;
}

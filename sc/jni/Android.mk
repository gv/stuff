LOCAL_PATH:= $(call my-dir)
include $(CLEAR_VARS)

include $(LOCAL_PATH)/Cvcore.mk
include $(CLEAR_VARS)

LOCAL_MODULE    := libducttapedcams
LOCAL_SRC_FILES := findpoints.cpp

LOCAL_CPPFLAGS  += -fexceptions

LOCAL_CPPFLAGS  += -I$(LOCAL_PATH)/../opencv/opencv/modules/features2d/include
LOCAL_CPPFLAGS  += -I$(LOCAL_PATH)/../opencv/opencv/modules/flann/include
LOCAL_CPPFLAGS  += -I$(LOCAL_PATH)/../opencv/opencv/modules/imgproc/include
CV_FEAT = ../opencv/opencv/modules/features2d/src
LOCAL_SRC_FILES += $(CV_FEAT)/surf.cpp
LOCAL_SRC_FILES += $(CV_FEAT)/sift.cpp
LOCAL_SRC_FILES += $(CV_FEAT)/fast.cpp
LOCAL_SRC_FILES += $(CV_FEAT)/mser.cpp
LOCAL_SRC_FILES += $(CV_FEAT)/stardetector.cpp
LOCAL_SRC_FILES += $(CV_FEAT)/detectors.cpp
LOCAL_SRC_FILES += $(CV_FEAT)/keypoint.cpp
LOCAL_SRC_FILES += $(CV_FEAT)/orb.cpp
LOCAL_SRC_FILES += $(CV_FEAT)/dynamic.cpp

CV_IMPC = ../opencv/opencv/modules/imgproc/src
LOCAL_SRC_FILES += $(CV_IMPC)/imgwarp.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/sumpixels.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/smooth.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/filter.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/tables.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/utils.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/templMatch.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/contours.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/matchcontours.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/pyramids.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/featureselect.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/shapedescr.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/linefit.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/color.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/geometry.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/corner.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/morph.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/thresh.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/convhull.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/deriv.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/approx.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/rotcalipers.cpp
LOCAL_SRC_FILES += $(CV_IMPC)/moments.cpp



#LOCAL_C_INCLUDES := $(LOCAL_PATH)/../opencv/opencv/modules/core/include
LOCAL_CPPFLAGS  += -I$(LOCAL_PATH)/../opencv/opencv/modules/core/include
LOCAL_STATIC_LIBRARIES += cvcore

# for core/persistence.cpp
LOCAL_LDLIBS    := -lz

include $(BUILD_SHARED_LIBRARY)



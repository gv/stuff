LOCAL_PATH:= $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE    := libducttapedcams
LOCAL_SRC_FILES := findpoints.cpp

include $(BUILD_SHARED_LIBRARY)

LOCAL_PATH:= $(call my-dir)

include $(CLEAR_VARS)

#LOCAL_C_INCLUDES := $(LOCAL_PATH)/../opencv/opencv/modules/core/include
LOCAL_CPPFLAGS  += -I$(LOCAL_PATH)/../opencv/opencv/modules/core/include
LOCAL_MODULE    := libducttapedcams
LOCAL_SRC_FILES := findpoints.cpp

include $(BUILD_SHARED_LIBRARY)

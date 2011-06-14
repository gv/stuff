LOCAL_PATH:= $(call my-dir)

include $(CLEAR_VARS)

#LOCAL_C_INCLUDES := $(LOCAL_PATH)/../opencv/opencv/modules/core/include
LOCAL_CPPFLAGS  += -I$(LOCAL_PATH)/../opencv/opencv/modules/core/include
LOCAL_CPPFLAGS  += -I$(LOCAL_PATH)/../opencv/opencv/modules/features2d/include
LOCAL_CPPFLAGS  += -I$(LOCAL_PATH)/../opencv/opencv/modules/flann/include
LOCAL_MODULE    := libducttapedcams
LOCAL_SRC_FILES := findpoints.cpp

LOCAL_CPPFLAGS  += -fexceptions
OCV_CORE = ../opencv/opencv/modules/core/src
LOCAL_SRC_FILES += $(OCV_CORE)/alloc.cpp
LOCAL_SRC_FILES += $(OCV_CORE)/matrix.cpp
LOCAL_SRC_FILES += $(OCV_CORE)/system.cpp
LOCAL_SRC_FILES += $(OCV_CORE)/copy.cpp
LOCAL_SRC_FILES += $(OCV_CORE)/convert.cpp
LOCAL_SRC_FILES += $(OCV_CORE)/array.cpp
LOCAL_SRC_FILES += $(OCV_CORE)/datastructs.cpp
LOCAL_SRC_FILES += $(OCV_CORE)/tables.cpp
LOCAL_SRC_FILES += $(OCV_CORE)/mathfuncs.cpp
LOCAL_SRC_FILES += $(OCV_CORE)/matop.cpp
LOCAL_SRC_FILES += $(OCV_CORE)/stat.cpp
LOCAL_SRC_FILES += $(OCV_CORE)/arithm.cpp
LOCAL_SRC_FILES += $(OCV_CORE)/rand.cpp

# why we need that?
#LOCAL_SRC_FILES += $(OCV_CORE)/persistence.cpp

OCV_FEAT = ../opencv/opencv/modules/features/src


include $(BUILD_SHARED_LIBRARY)

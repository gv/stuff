include $(CLEAR_VARS)

LOCAL_MODULE := cvcore
LOCAL_CPPFLAGS  += -fexceptions
LOCAL_CPPFLAGS  += -I$(LOCAL_PATH)/../opencv/opencv/modules/core/include

CV_CORE = ../opencv/opencv/modules/core/src
LOCAL_SRC_FILES += $(CV_CORE)/alloc.cpp
LOCAL_SRC_FILES += $(CV_CORE)/matrix.cpp
LOCAL_SRC_FILES += $(CV_CORE)/system.cpp
LOCAL_SRC_FILES += $(CV_CORE)/copy.cpp
LOCAL_SRC_FILES += $(CV_CORE)/convert.cpp
LOCAL_SRC_FILES += $(CV_CORE)/array.cpp
LOCAL_SRC_FILES += $(CV_CORE)/datastructs.cpp
LOCAL_SRC_FILES += $(CV_CORE)/tables.cpp
LOCAL_SRC_FILES += $(CV_CORE)/mathfuncs.cpp
LOCAL_SRC_FILES += $(CV_CORE)/matop.cpp
LOCAL_SRC_FILES += $(CV_CORE)/stat.cpp
LOCAL_SRC_FILES += $(CV_CORE)/arithm.cpp
LOCAL_SRC_FILES += $(CV_CORE)/rand.cpp
LOCAL_SRC_FILES += $(CV_CORE)/lapack.cpp
LOCAL_SRC_FILES += $(CV_CORE)/matmul.cpp
# for detector
LOCAL_SRC_FILES += $(CV_CORE)/drawing.cpp
LOCAL_SRC_FILES += $(CV_CORE)/dxt.cpp

# why do we need that?
LOCAL_SRC_FILES += $(CV_CORE)/persistence.cpp

include $(BUILD_STATIC_LIBRARY)

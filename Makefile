FFMPEG_LIBS := libavcodec libavutil libavfilter libavformat

CFLAGS += -Wall -g
CFLAGS := $(shell pkg-config --cflags $(FFMPEG_LIBS)) $(CFLAGS)
LDFLAGS := $(shell pkg-config --libs $(FFMPEG_LIBS)) $(LDLIBS)

filtering_video: filtering_video.c
decode_video: decode_video.c


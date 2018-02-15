
LDFLAGS=-lavformat -lavcodec -lavutil

ffmpeg: ffmpeg.scm enum-pixfmts.so foreign-enum.scm
	csc ${LDFLAGS} ffmpeg.scm
enum-pixfmts.so: enum-pixfmts.scm
	csc -s ${LDFLAGS} enum-pixfmts.scm
encode: encode.c
grabtest: grabtest.c
ffmpeg-test: ffmpeg-test.c

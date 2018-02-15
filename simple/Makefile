
LDFLAGS=-lavformat -lavcodec -lavutil

ffmpeg: ffmpeg.scm pixfmts.so foreign-enum.scm
	csc ${LDFLAGS} ffmpeg.scm
pixfmts.so: pixfmts.scm
	csc -s ${LDFLAGS} pixfmts.scm
encode: encode.c
grabtest: grabtest.c
ffmpeg-test: ffmpeg-test.c

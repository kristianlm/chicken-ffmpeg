
LDFLAGS=-lavformat -lavcodec -lavutil -lavfilter

main: main.scm ffmpeg.so
	csc main.scm

ffmpeg.so: ffmpeg.scm enum-pixfmts.so foreign-enum.scm
	csc -s ${LDFLAGS} ffmpeg.scm

enum-pixfmts.so: enum-pixfmts.scm
	csc -s ${LDFLAGS} enum-pixfmts.scm

encode: encode.c
grabtest: grabtest.c
ffmpeg-test: ffmpeg-test.c

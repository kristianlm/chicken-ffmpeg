(load "enum-pixfmts.so")
(load "ffmpeg.so")

(define fmtx (avformat-open-input "/tmp/testsrc.mpg"))
(define cx (codecx (fmtx-stream fmtx 0)))
(define pkt (fmtx-read fmtx))

(print "fmtx: " fmtx)
(print "fmtx-streams fmtx:" (fmtx-streams fmtx))
(print "cx: " cx)
(print "pkt: " pkt)

(print "send: " (avcodec-send-packet cx pkt))
(print "send: " (avcodec-send-packet cx pkt))
(print "send: " (avcodec-send-packet cx pkt))
(define frame (make-frame))
(print "frame: " frame)
(print "receive: " (avcodec-receive-frame cx frame))
(print "frame: " frame)

(with-output-to-file "/tmp/x"
  (lambda ()
    (print "P5\n" (frame-width frame) " " (frame-height frame) "\n255")
    (write-u8vector (frame-data frame 0))))
 

(repl)



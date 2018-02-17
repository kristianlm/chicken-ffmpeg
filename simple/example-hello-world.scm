(use miscmacros ports)
(load "enum-pixfmts.so")
(load "ffmpeg.so")

(begin
  (define fg    (make-flg))
  (define in    (make-flx fg (find-filter "testsrc")    "in"))
  (define out   (make-flx fg (find-filter "buffersink") "out"))
  (define scale (make-flx fg (find-filter "scale")      "scale"))

  (avfilter-init-str in "size=1024x840")
  (opt-set-pix-fmts out '(gray8))
  (avfilter-init-str scale "100x35")

  (avfilter-link in 0 scale 0)
  (avfilter-link scale 0 out 0)
  (avfilter-graph-config fg))

(define (display-frame frame)
  (let ((data (frame-data frame 0)))
    (do ((y 0 (+ y 1)))
        ((> y (frame-height frame)))
      (do ((x 0 (+ x 1)))
          ((> x (frame-width frame)))
        (print* (string-ref " .-+#" (inexact->exact
                                     (floor (/ (u8vector-ref data x) 52))))))
      (print)))
  (thread-sleep! 0.1))

(port-for-each
 (lambda (frame)
   (print "\033c")
   (print "frame: " frame "  " (frame-pts frame))
   (display-frame frame))
 (let ((frame (make-frame)))
   (lambda ()
     (frame-unref frame)
     (av-buffersink-get-frame out frame))))

(define fmx (avformat-open-input "" "x11grab"))
(define stm (fmtx-stream fmx 0))
(define cox (codecx stm))

(define pkt (make-packet))
(fmtx-read fmx pkt)

(avcodec-send-packet cox pkt)
(avcodec-receive-frame cox frame)

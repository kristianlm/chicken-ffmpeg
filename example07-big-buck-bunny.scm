(use ffmpeg base64 ports)

(display "\x1b[2J") ;; cls

(define (display-frame frame)
  (display "\x1b[u") ;; reset cursor to 0,0
  (print "(memory-statistics): " (memory-statistics))
  (print "frame: " frame)
  (let ((data (frame-data frame 0)))
    (do ((y 0 (+ y 1)))
        ((>= y (frame-height frame)))
      (do ((x 0 (+ x 1)))
          ((>= x (frame-width frame)))
        (display (string-ref
                  " .-+#"
                  (inexact->exact
                   (floor (/ (u8vector-ref
                              data
                              (+ (* (frame-linesize frame 0) y) x)) 52))))))
      (print))))

(define F (avformat-open-input "http://download.blender.org/peach/bigbuckbunny_movies/big_buck_bunny_480p_surround-fix.avi"))

(define stream (fmtx-stream F 0))
(define cx (make-codecx stream))

(begin
  (define fg (make-flg))
  (define in (make-flx fg "buffer" "in"
                       (conc "video_size=" (codecx-width cx) "x" (codecx-height cx)
                             ":pix_fmt=" (codecx-pix-fmt cx)
                             ":time_base="
                             (conc (vector-ref (stream-time-base stream) 0) "/"
                                   (vector-ref (stream-time-base stream) 1))
                             ":pixel_aspect=1/1")))
  (define scale (make-flx fg "scale" "scale" "size=110x35"))
  (define out (make-flx fg "buffersink" "out"))
  (opt-set-pix-fmts out '(gray8))
  (avfilter-link in 0 scale 0)
  (avfilter-link scale 0 out 0)
  (avfilter-graph-config fg))

(define pkt (make-packet))
(define frame (make-frame))

(port-for-each
 (lambda (pkt)
   (when (= 0 (packet-stream-index pkt))
     (avcodec-send-packet cx pkt)
     (avcodec-receive-frame cx frame)
     (av-buffersrc-add-frame in frame)
     (frame-unref frame)
     (av-buffersink-get-frame out frame)
     (display-frame frame)))
 (lambda ()
   (packet-unref pkt)
   (frame-unref frame)   
   (fmtx-read F pkt)))

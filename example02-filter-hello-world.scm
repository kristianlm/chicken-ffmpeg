(use ports ffmpeg)

(define (cls) (display "\x1b[2J"))
(define (reset-cursor) (display "\x1b[u\r"))

; TODO: make it pretty, like this
;; (define-flg
;;   (in "testsrc" "size=100x30")
;;   (out "buffersink" pix-fmts: '(gray8)))
(begin
  (define fg    (make-flg))
  (define in    (make-flx fg (find-filter "testsrc")    "in" "size=100x30"))
  (define out   (make-flx fg (find-filter "buffersink") "out"))
  (opt-set-pix-fmts out '(gray8))

  (avfilter-link in 0 out 0)
  (avfilter-graph-config fg))

(define (display-frame frame)
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

(cls)
(port-for-each
 (lambda (frame)
   (reset-cursor)
   (print "frame: " frame "  " (frame-pts frame))
   (display-frame frame))
 (let ((frame (make-frame)))
   (lambda ()
     (frame-unref frame)
     (av-buffersink-get-frame out frame))))


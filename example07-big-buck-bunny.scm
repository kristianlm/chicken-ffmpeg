;; big buck bunny chicken-ffmpeg example
;; - no sound
;;
;; video url
;; avformat (F)       ← reads packets from media stream, we assume stream #0 is video
;;   ↓
;; decoder (cx)       ← turns h264 packets to video frames (480x)
;;   ↓
;; buffersrc (in)     ← input to filtergraph (video format/size needs to match incoming frames)
;;   ↓
;; scale              ← converts original video size to suitable for terminal
;;   ↓
;; buffersink (out)   ← converts to gray8 for us
;;   ↓
;; display-frame
(use ffmpeg ports)

(define (cls) (display "\x1b[2J"))
(define (reset-cursor) (display "\x1b[u\r"))

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

(define F (avformat-open-input
           "big-buck-bunny.mp4"
           ;;"http://download.blender.org/peach/bigbuckbunny_movies/big_buck_bunny_480p_surround-fix.avi"
           ))

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
  (define scale (make-flx fg "scale" "scale" "size=110x31"))
  (define out (make-flx fg "buffersink" "out"))
  (opt-set-pix-fmts out '(gray8))
  (avfilter-link in 0 scale 0)
  (avfilter-link scale 0 out 0)
  (avfilter-graph-config fg))

(define frame (make-frame))
(cls)
(port-for-each
 (lambda (pkt)
   (when (= 0 (packet-stream-index pkt))
     (avcodec-send-packet cx pkt)
     (let loop ()
       (when (avcodec-receive-frame cx frame)
         (reset-cursor)
         (print "(memory-statistics): " (memory-statistics))
         (print "stream: " stream)
         (print "pkt: " pkt)
         (print "unfiltered frame: " frame)
         (av-buffersrc-add-frame in frame)
         (frame-unref frame) ;; discard unfirtered frame
         (av-buffersink-get-frame out frame) ;; reusing frame for filtered frames (probably a bad idea)
         (print "filtered   frame: " frame)
         (display-frame frame)
         (loop)))))
 (let ((pkt (make-packet)))
   (lambda ()
     (packet-unref pkt)
     (fmtx-read F pkt))))

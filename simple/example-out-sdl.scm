(use miscmacros ports ffmpeg srfi-18)

(define w 512)
(define h 512)

(begin
  (define fg (make-flg))
  (define in (make-flx fg (find-filter "testsrc2")   "in"))
  (define out (make-flx fg (find-filter "buffersink") "out"))
  (define scale (make-flx fg (find-filter "scale") "scale" (conc w "x" h)))

  (opt-set-pix-fmts out '(rgb0))

  (avfilter-link in 0 scale 0)
  (avfilter-link scale 0 out 0)
  (avfilter-graph-config fg))

(begin
  (define fmx (avformat-open-output "sdl" #f))
  (define stm (avformat-new-stream! fmx))

  (define cp (stream-codecpar stm))
  (set! (codecpar-type cp) 'video)
  (set! (codecpar-id cp) 'rawvideo)
  (set! (codecpar-width cp) w)
  (set! (codecpar-height cp) h)
  (set! (codecpar-format cp) 'rgb0)
  (set! (codecpar-tag cp) (pix-fmt->tag (codecpar-format cp))) ;; need fourcc

  (set! (stream-time-base stm) (s32vector 2 1))
  (set! (stream-avg-frame-rate stm) (s32vector 1 2)))

(avformat-write-header fmx) ;; this should open the SDL output window
(define frame (make-frame))
(define pkt (make-packet ))

(repeat (* 25 1)
  (av-buffersink-get-frame out frame)
  (set! (packet-data pkt) (frame-data frame 0))
  (frame-unref frame)
  (fmtx-write fmx pkt))

(avformat-write-trailer fmx)

;;(repl)


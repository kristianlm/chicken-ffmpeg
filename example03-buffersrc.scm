(use ffmpeg)

(begin
  (define fg (make-flg))
  (define in (make-flx fg "buffer" "in" "video_size=16x16:pix_fmt=gray8:time_base=1/1:pixel_aspect=1/1"))
  (define scale (make-flx fg "scale" "scale" "8x8"))
  (define out (make-flx fg "buffersink" "out"))
  (avfilter-link in 0 scale 0)
  (avfilter-link scale 0 out 0)
  (avfilter-graph-config fg))

(begin
  (define frame (make-frame width: 16 height: 16 format: 'gray8))
  (frame-get-buffer frame 32)
  (frame-make-writable frame)
  ;; TODO: implement frame-data-set!
  (av-buffersrc-add-frame in frame))

(begin
  (define png
    (make-codecx (find-encoder "png")
                 width: 8
                 height: 8
                 pix-fmt: 'gray8
                 time-base: (vector 1 1)))
  (avcodec-open png #f)
  (define frame (av-buffersink-get-frame out))
  (print "got frame " frame)
  (avcodec-send-frame png frame)
  (define pkt (avcodec-receive-packet png))

  (with-output-to-file "/tmp/out.png"
    (lambda () (display (blob->string (u8vector->blob/shared (packet-data pkt)))))))

(use ffmpeg)

(begin
  (define fg (make-flg))
  (define in (make-flx fg "testsrc" "in" "size=256x256"))
  (define out (make-flx fg "buffersink" "out" ""))
  (opt-set-pix-fmts out '(rgb24))
  (avfilter-link in 0 out 0)
  (avfilter-graph-config fg))

(begin
  (define png (make-codecx (find-encoder "png")
                           width: 256
                           height: 256
                           pix-fmt: 'rgb24
                           time-base: (vector 1 1)))
  (avcodec-open png #f)
  (avcodec-send-frame png (av-buffersink-get-frame out))

  (with-output-to-file "out.png"
    (lambda () (write-u8vector (packet-data (avcodec-receive-packet png))))))


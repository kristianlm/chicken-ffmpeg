(use ffmpeg miscmacros)

(begin
  (define fmx (avformat-open-output #f "hi.mkv"))
  (define stm (avformat-new-stream! fmx))

  (define cp (stream-codecpar stm))
  (set! (codecpar-type cp) 'video)
  (set! (codecpar-id cp) 'rawvideo)
  (set! (codecpar-width cp) 7)
  (set! (codecpar-height cp) 7)
  (set! (codecpar-format cp) 'gray8)
  (set! (codecpar-tag cp) (pix-fmt->tag (codecpar-format cp))) ;; need fourcc

  (set! (stream-time-base stm) (s32vector 2 1))
  (set! (stream-avg-frame-rate stm) (s32vector 1 2)))

(define i 0)
(define pkt (make-packet))
(avformat-write-header fmx)
(repeat (* 25 1000)
        (set! i (+ i 1))
        (set! (packet-pts pkt) (* 10 i))
        (set! (packet-dts pkt) (* 10 i))
        (set! (packet-data pkt)
              (let ((x 255))
                (u8vector i 0 0 0 0 0 i
                          0 x 0 x 0 0 0
                          0 x 0 x 0 x 0
                          0 x x x 0 0 0
                          0 x 0 x 0 x 0
                          0 x 0 x 0 x 0
                          i 0 0 0 0 0 i)))
        (fmtx-write fmx pkt))
(avformat-write-trailer fmx)

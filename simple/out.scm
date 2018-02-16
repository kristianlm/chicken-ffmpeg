(use miscmacros)
(load "enum-pixfmts.so")
(load "ffmpeg.so")

(define F (avformat-open-input "testsrc" "lavfi"))

(port-for-each
 (lambda (pkt)
   (print pkt))
 (let ((pkt (make-packet)))
   (lambda ()
     (packet-unref pkt)
     (fmtx-read F pkt))))


;; (foreign-declare "
;; #include <libavcodec/avcodec.h>
;; ")

;; (define frame (make-frame))
;; (set! (frame-width frame) 352)
;; (set! (frame-height frame) 288)
;; (set! (frame-format* frame) (AVPixelFormat->int 'yuv420p))
;; (set! (frame-pts frame) 0)
;; ;;(set! (frame-bit-rate frame) 40000)
;; ;;(set! (frame-time-base frame) 1)

;; (define codec (find-encoder 'h264))
;; (define encoderx (codecx codec #f #f))

;; ((foreign-lambda* void (((c-pointer "AVCodecContext") c)) "
;; //    c->bit_rate = 40000;
;;     c->time_base = (AVRational){1, 25};
;;     c->pix_fmt = AV_PIX_FMT_YUV420P;
;; ") (AVCodecContext-ptr encoderx))

;; (set! (codecx-bit-rate encoderx) 10000)
;; (set! (codecx-width encoderx) 352)
;; (set! (codecx-height encoderx) 288)

;; (print "encoderx: " encoderx)
;; (print "frame: " frame)
;; (frame-get-buffer frame 32)
;; (print "frame: " frame)

;; (avcodec-open encoderx codec)

;; (define pkt (make-packet))
;; (define op (open-output-file "/tmp/oo"))

;; (repeat
;;  250

;;  (while (eq? #f (avcodec-receive-packet encoderx pkt))
;;    (set! (frame-pts frame) (+ (frame-pts frame) 1))
;;    (print* (frame-pts frame) ". ")
;;    (avcodec-send-frame encoderx frame))
;;  (print "pkt: " pkt)
;;  (write-u8vector (packet-data pkt) op))

;; (while (eq? #t (avcodec-receive-packet encoderx pkt))
;;   (write-u8vector (packet-data pkt) op))

;; (close-output-port op)


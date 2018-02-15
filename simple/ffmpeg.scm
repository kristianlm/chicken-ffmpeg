(use
 (only data-structures rassoc)
 (only srfi-1 list-tabulate))

(foreign-declare "
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
//#include <libavdevice/avdevice.h>
//#include <libavutil/time.h>
")

;; ==================== init ====================
((foreign-lambda* void ()
                  "
av_register_all();
avformat_network_init();
"))

(define-record AVPacket ptr)
(define-record AVFormatContext ptr)
(define-record AVStream ptr)
(define-record AVFrame ptr)
(define-record AVCodecContext ptr)
(define-record AVCodecParameters ptr)

(load "pixfmts.so")
(include "foreign-enum.scm")

(define AVMediaTypes
  (foreign-enum
   (unknown    "AVMEDIA_TYPE_UNKNOWN"    "")
   (video      "AVMEDIA_TYPE_VIDEO"      "")
   (audio      "AVMEDIA_TYPE_AUDIO"      "")
   (data       "AVMEDIA_TYPE_DATA"       "")
   (subtitle   "AVMEDIA_TYPE_SUBTITLE"   "")
   (attachment "AVMEDIA_TYPE_ATTACHMENT" "")
   (nb         "AVMEDIA_TYPE_NB"         "")))

(define (AVMediaType->int sym) (cond ((assoc sym AVMediaTypes) => cdr)  (else #f)))
(define (int->AVMediaType int) (cond ((rassoc int AVMediaTypes) => car) (else #f)))


(define (AVCodecParameters->int sym) (cond ((assoc sym codecs) => cdr) (else #f)))
(define (int->AVCodecParameters int) (cond ((rassoc int codecs) => car) (else #f)))

(define-foreign-type AVPacket (c-pointer "AVPacket")
  (lambda (x) (AVPacket-ptr x))
  (lambda (ptr) (make-AVPacket ptr)))

(define-foreign-type AVFormatContext (c-pointer "AVFormatContext")
  (lambda (x) (AVFormatContext-ptr x))
  (lambda (ptr) (make-AVFormatContext ptr)))

(define-foreign-type AVStream (c-pointer "AVStream")
  (lambda (x)   (     AVStream-ptr x))
  (lambda (ptr) (make-AVStream ptr)))

(define-foreign-type AVCodecContext (c-pointer "AVCodecContext")
  (lambda (x)   (     AVCodecContext-ptr x))
  (lambda (ptr) (make-AVCodecContext ptr)))

(define-foreign-type AVMediaType int
  (lambda (sym) (AVMediaType->int sym))
  (lambda (int) (int->AVMediaType int)))

(define-foreign-type AVCodecParameters (c-pointer "AVCodecParameters")
  (lambda (x)   (     AVCodecParameters-ptr x))
  (lambda (ptr) (make-AVCodecParameters ptr)))

(define-foreign-type AVCodecID int
  (lambda (sym) (AVCodecParameters->int sym))
  (lambda (int) (int->AVCodecParameters int)))

(define-syntax ƒget
  (syntax-rules ()
    ((_ rtype ((type arg)) body_str)
     ((foreign-lambda* rtype ((type arg)) "return(" body_str ");")
      arg))))

;; ==================== AVPacket ====================

(define (packet-init pkt) (foreign-lambda* void ((AVPacket pkt)) "av_init_packet(pkt);"))
(define (packet)
  (define pkt ((foreign-lambda* AVPacket () "return(av_packet_alloc());")))
  (packet-init pkt)
  (set-finalizer!
   pkt
   (lambda (pkt)
     (print "freeing " pkt)
     ((foreign-lambda* void ((AVPacket pkt)) "av_packet_free(&pkt);") pkt))))

(define (packet-stream-index pkt) (ƒget int ((AVPacket pkt)) "pkt->stream_index"))
(define (packet-size         pkt) (ƒget int ((AVPacket pkt)) "pkt->size"))

;; ==================== AVFrame ====================

;;(define (frame w h fmt) )

;; ==================== AVFormatContext accessors ====================

(define (fmtx-stream-count fmtx) (ƒget int ((AVFormatContext fmtx)) "fmtx->nb_streams"))
(define (fmtx-stream fmtx idx)
  ((foreign-lambda* AVStream ((AVFormatContext fmtx) (int idx)) "return(fmtx->streams[idx]);") fmtx idx))

(define (fmtx-streams fmtx)
  (list-tabulate (fmtx-stream-count fmtx)
                 (lambda (idx) (fmtx-stream fmtx idx))))

(define (fmtx-filename fmtx) (ƒget c-string ((AVFormatContext fmtx)) "fmtx->filename"))

(define (fmtx-read fmtx #!optional (pkt (packet)))

  ;; TODO: check return value
  ((foreign-lambda* int ((AVFormatContext fmtx)
                         (AVPacket pkt))
                    "return(av_read_frame(fmtx, pkt));")
   fmtx pkt)
  pkt)

;; ==================== AVCodecContext ====================

(define (codecpar-type cp) (ƒget AVMediaType ((AVCodecParameters cp)) "cp->codec_type"))
(define (codecpar-id cp) (ƒget AVCodecID ((AVCodecParameters cp)) "cp->codec_id"))
(define (stream-bitrate stm) (ƒget int ((AVStream stm)) "stm->codecpar->bit_rate"))

;; ==================== AVStream accessors ====================

(define (stream-index stm)   (ƒget int ((AVStream stm)) "stm->index"))
(define (stream-codecpar stm) (ƒget AVCodecParameters ((AVStream stm)) "stm->codecpar"))

(define-record-printer AVPacket
  (lambda (pkt p)
    (display "#<AVPacket" p)
    (display " #" p) (display (packet-stream-index pkt) p)
    (display " bytes:" p) (display (packet-size pkt) p)
    (display ">" p)))

(define-record-printer AVStream
  (lambda (stm p)
    (display "#<AVStream " p)
    (display "#" p) (display (stream-index stm) p)   
    (display " kbps:" p) (display (inexact->exact (floor (/ (stream-bitrate stm) 1000))) p)
    (display " parameters:" p) (display (stream-codecpar stm) p)
    (display ">" p)))

(define-record-printer AVCodecParameters
  (lambda (x p)
    (display "#<AVCodecParameters" p)
    (display " type:" p) (display (codecpar-type x) p)
    (display " id:" p) (display (codecpar-id x) p)
    (display ">" p)))

(define (avformat_open_input url)
  
  (define fmtx
    ((foreign-lambda* AVFormatContext () "return(avformat_alloc_context());")))
  
  ((foreign-lambda* int ((AVFormatContext fmtx)
                         (c-string url))
                    "return(avformat_open_input(&fmtx, url, NULL, NULL));")
   fmtx url)
  
  (set-finalizer!
   fmtx
   (lambda (fmtx)
     (print "closing " fmtx)
     ((foreign-lambda* void ((AVFormatContext fmtx)) "avformat_close_input(&fmtx);")
      fmtx))))

(define (av_dump_format! fmtx #!optional (filename ""))
  ((foreign-lambda* void ((AVFormatContext fmtx) (c-string filename))
                    "av_dump_format(fmtx, 0, filename, 0);")
   fmtx filename))

(define (avformat_find_stream_info! fmtx)
  ((foreign-lambda* void ((AVFormatContext fmtx))
                    "avformat_find_stream_info(fmtx, 0);")
   fmtx))


(define fmtx (avformat_open_input "/tmp/testsrc.mp4"))

(print "fmtx: " fmtx)
(print "fmtx-streams fmtx:" (fmtx-streams fmtx))

;; (for-each
;;  (begin
;;    (lambda (x) (print "nevermind"))
;;    (lambda (filename)
;;      (define fmtx (avformat_open_input filename))
;;      (avformat_find_stream_info! fmtx)
;;      (print "fmtx: " fmtx)
;;      (av_dump_format! fmtx filename)
;;      (print "streams" (fmtx-streams fmtx))))
;;  (command-line-arguments))

(repl)

(use (only data-structures rassoc))

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

(define-record AVFormatContext ptr)
(define-record AVStream ptr)
(define-record AVFrame ptr)
(define-record AVCodecContext ptr)

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

;; AVStream getters

(define-syntax ƒget
  (syntax-rules ()
    ((_ type-return ((type arg)) body_str)
     ((foreign-lambda* type-return ((type arg)) "return(" body_str ");")
      arg))))

(define (stream-index stm)   (ƒget int ((AVStream stm)) "stm->index"))
(define (stream-id    stm)   (ƒget int ((AVStream stm)) "stm->codecpar->codec_id"))
(define (stream-type stm)    (ƒget AVMediaType ((AVStream stm)) "stm->codecpar->codec_type"))
(define (stream-bitrate stm) (ƒget int ((AVStream stm)) "stm->codecpar->bit_rate"))


(define-record-printer AVStream
  (lambda (stm p)
    (display "#<AVStream " p)
    (display "#" p) (display (stream-index stm) p)   
    (display " type:" p) (display (stream-type stm) p)
    (display " id:" p) (display (stream-id stm) p)
    
    (display " kbps:" p) (display (inexact->exact (floor (/ (stream-bitrate stm) 1000))) p)
    
    (display ">" p)))

(define (avformat_open_input fmtx url)
  ((foreign-lambda* int ((AVFormatContext fmtx)
                         (c-string url))
                    "return(avformat_open_input(&fmtx, url, NULL, NULL));")
   fmtx url))


(define (AVFormatContext)
  (set-finalizer!
   ((foreign-lambda* AVFormatContext () "return(avformat_alloc_context());"))
   (lambda (x)
     (print "freeing " x)
     ((foreign-lambda* void ((AVFormatContext x)) "avformat_free_context(x);") x))))

(define (av_dump_format! fmtx #!optional (filename ""))
  ((foreign-lambda* void ((AVFormatContext fmtx) (c-string filename))
                    "av_dump_format(fmtx, 0, filename, 0);")
   fmtx filename))

(define (avformat_find_stream_info! fmtx)
  ((foreign-lambda* void ((AVFormatContext fmtx))
                    "avformat_find_stream_info(fmtx, 0);")
   fmtx))


;; getters
(define (fmtx-stream-count fmtx)
  ((foreign-lambda* int ((AVFormatContext fmtx))
                    "return(fmtx->nb_streams);")
   fmtx))

(define (fmtx-stream fmtx idx)
  ((foreign-lambda* AVStream ((AVFormatContext fmtx)
                              (int idx))
                    "return(fmtx->streams[idx]);")
   fmtx idx))

(define fmtx (AVFormatContext))
;;(define (AVFormatContext fmtx))

(print "fmtx: " fmtx)

(avformat_open_input fmtx "/tmp/testsrc.mp4")
;;(av_dump_format! fmtx)

(avformat_find_stream_info! fmtx)
(av_dump_format! fmtx)

(print "format context has " (fmtx-stream-count fmtx) "streams total")

(print "stream 0: " (fmtx-stream fmtx 0))

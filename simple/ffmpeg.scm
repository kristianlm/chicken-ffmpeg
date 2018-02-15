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
(define-record AVCodec ptr)
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
   (attachment "AVMEDIA_TYPE_ATTACHMENT" "")))

(define AVPictureTypes
  (foreign-enum
   (#f   "AV_PICTURE_TYPE_NONE" "Undefined")
   (i    "AV_PICTURE_TYPE_I"    "Intra")
   (p    "AV_PICTURE_TYPE_P"    "Predicted")
   (b    "AV_PICTURE_TYPE_B"    "Bi-dir predicted")
   (s    "AV_PICTURE_TYPE_S"    "S(GMC)-VOP MPEG-4")
   (si   "AV_PICTURE_TYPE_SI"   "Switching Intra")
   (sp   "AV_PICTURE_TYPE_SP"   "Switching Predicted")
   (bi   "AV_PICTURE_TYPE_BI"   "BI type")))

(define (AVMediaType->int sym) (cond ((assoc sym AVMediaTypes) => cdr)  (else #f)))
(define (int->AVMediaType int) (cond ((rassoc int AVMediaTypes) => car) (else #f)))

(define (AVPictureType->int sym) (cond ((assoc sym AVPictureTypes) => cdr)  (else #f)))
(define (int->AVPictureType int) (cond ((rassoc int AVPictureTypes) => car) (else #f)))

(define (AVPixelFormat->int sym) (cond ((assoc sym pixfmts) => cdr)  (else #f)))
(define (int->AVPixelFormat int) (cond ((rassoc int pixfmts) => car) (else #f)))

(define (AVCodecParameters->int sym) (cond ((assoc sym codecs) => cdr) (else #f)))
(define (int->AVCodecParameters int) (cond ((rassoc int codecs) => car) (else #f)))

(define int->video-format int->AVPixelFormat)
(define video-format->int AVPixelFormat->int)

(define-foreign-type AVPacket (c-pointer "AVPacket")
  (lambda (x) (AVPacket-ptr x))
  (lambda (ptr) (and ptr (make-AVPacket ptr))))

(define-foreign-type AVFormatContext (c-pointer "AVFormatContext")
  (lambda (x) (AVFormatContext-ptr x))
  (lambda (ptr) (and ptr (make-AVFormatContext ptr))))

(define-foreign-type AVStream (c-pointer "AVStream")
  (lambda (x)   (     AVStream-ptr x))
  (lambda (ptr) (and ptr (make-AVStream ptr))))

(define-foreign-type AVCodecContext (c-pointer "AVCodecContext")
  (lambda (x)   (     AVCodecContext-ptr x))
  (lambda (ptr) (and ptr (make-AVCodecContext ptr))))

(define-foreign-type AVFrame (c-pointer "AVFrame")
  (lambda (x) (AVFrame-ptr x))
  (lambda (ptr) (and ptr (make-AVFrame ptr))))

(define-foreign-type AVMediaType int
  (lambda (sym) (AVMediaType->int sym))
  (lambda (int) (int->AVMediaType int)))

(define-foreign-type AVPictureType int
  (lambda (sym) (AVPictureType->int sym))
  (lambda (int) (int->AVPictureType int)))

(define-foreign-type AVCodec (c-pointer "AVCodec")
  (lambda (x)   (     AVCodec-ptr x))
  (lambda (ptr) (and ptr (make-AVCodec ptr))))

(define-foreign-type AVCodecParameters (c-pointer "AVCodecParameters")
  (lambda (x)   (     AVCodecParameters-ptr x))
  (lambda (ptr) (and ptr (make-AVCodecParameters ptr))))

(define-foreign-type AVCodecID int
  (lambda (sym) (if (symbol? sym) (AVCodecParameters->int sym) sym))
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

(define (make-frame #!key w h fmt)
  (set-finalizer! ((foreign-lambda AVFrame "av_frame_alloc"))
                  (lambda (x)
                    (print "freeing frame " x)
                    ((foreign-lambda* void ((AVFrame frame)) "av_frame_free(&frame);") x))))

;; ==================== AVFormatContext accessors ====================

(define (fmtx-stream-count fmtx) (ƒget int ((AVFormatContext fmtx)) "fmtx->nb_streams"))
(define (fmtx-stream fmtx idx)
  (when (>= idx (fmtx-stream-count fmtx))
    (error (conc "index out of bounds for size " (fmtx-stream-count fmtx)
                 "(try avformat_find_stream_info!)") idx))
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
(define-syntax define-getters
  (syntax-rules ()
    ((_ ((argtype argname)) ) (begin))
    ((_ ((argtype argname)) (name  rtype  str) rest ...)
     (begin (define name (foreign-lambda* rtype ((argtype argname)) "return(" str ");"))
            (define-getters ((argtype argname)) rest ...)))))

(define-getters ((AVCodecParameters cp))
  (codecpar-type                   AVMediaType                          "cp->codec_type")
  (codecpar-id                     AVCodecID                            "cp->codec_id")
  (codecpar-tag                    int                                  "cp->codec_tag")
  ;;(codecpar-*extradata           byte                                 "cp->*extradata")
  ;;(codecpar-extradata-size       int                                  "cp->extradata_size")
  (codecpar-format*                int                                  "cp->format")
  (codecpar-bit-rate               integer64                            "cp->bit_rate")
  (codecpar-bits-per-coded-sample  int                                  "cp->bits_per_coded_sample")
  (codecpar-bits-per-raw-sample    int                                  "cp->bits_per_raw_sample")
  (codecpar-profile                int                                  "cp->profile")
  (codecpar-level                  int                                  "cp->level")
  (codecpar-width                  int                                  "cp->width")
  (codecpar-height                 int                                  "cp->height")
  ;;(codecpar-sample-aspect-ratio    AVRational                         "cp->sample_aspect_ratio")
  ;;(codecpar-field-order            enum-AVFieldOrder                  "cp->field_order")
  ;;(codecpar-color-range            enum-AVColorRange                  "cp->color_range")
  ;;(codecpar-color-primaries        enum-AVColorPrimaries              "cp->color_primaries")
  ;;(codecpar-color-trc              enum-AVColorTransferCharacteristic "cp->color_trc")
  ;;(codecpar-color-space            enum-AVColorSpace                  "cp->color_space")
  ;;(codecpar-chroma-location        enum-AVChromaLocation              "cp->chroma_location")
  (codecpar-video-delay            int                                  "cp->video_delay")
  (codecpar-channel-layout         unsigned-integer64                   "cp->channel_layout")
  (codecpar-channels               int                                  "cp->channels")
  (codecpar-sample-rate            int                                  "cp->sample_rate")
  (codecpar-block-align            int                                  "cp->block_align")
  (codecpar-frame-size             int                                  "cp->frame_size")
  (codecpar-initial-padding        int                                  "cp->initial_padding")
  (codecpar-trailing-padding       int                                  "cp->trailing_padding")
  (codecpar-seek-preroll           int                                  "cp->seek_preroll"))

(define-getters ((AVFrame x))
  (frame-data                      c-pointer                                                 "x->data")
  (frame-linesize                  c-pointer                                "x->linesize")
  ;;(frame-**extended-data                uint8_t                            "x->**extended_data")
  (frame-width                          int                                "x->width")
  (frame-height                         int                                "x->height")
  (frame-nb-samples                     int                                "x->nb_samples")
  (frame-format*                        int                                "x->format")
  (frame-key-frame                      int                                "x->key_frame")
  (frame-pict-type                      AVPictureType                      "x->pict_type")
  ;;(frame-sample-aspect-ratio            AVRational                         "x->sample_aspect_ratio")
  (frame-pts                            integer64                          "x->pts")
  (frame-pkt-dts                        integer64                          "x->pkt_dts")
  (frame-coded-picture-number           int                                "x->coded_picture_number")
  (frame-display-picture-number         int                                "x->display_picture_number")
  (frame-quality                        int                                "x->quality")
  ;;(frame-*opaque                        void                               "x->*opaque")
  (frame-repeat-pict                    int                                "x->repeat_pict")
  (frame-interlaced-frame               int                                "x->interlaced_frame")
  (frame-top-field-first                int                                "x->top_field_first")
  (frame-palette-has-changed            int                                "x->palette_has_changed")
  (frame-reordered-opaque               integer64                          "x->reordered_opaque")
  (frame-sample-rate                    int                                "x->sample_rate")
  (frame-channel-layout                 unsigned-integer64                 "x->channel_layout")
  ;;(frame-*buf[AV_NUM_DATA_POINTERS]     AVBufferRef                        "x->*buf"[AV_NUM_DATA_POINTERS])
  ;;(frame-**extended-buf                 AVBufferRef                        "x->**extended_buf")
  ;;(frame-nb-extended-buf                int                                "x->nb_extended_buf")
  ;;(frame-**side-data                    AVFrameSideData                    "x->**side_data")
  ;;(frame-nb-side-data                   int                                "x->nb_side_data")
  (frame-flags                          int                                "x->flags")
  ;;(frame-color-range                    enum-AVColorRange                  "x->color_range")
  ;;(frame-color-primaries                enum-AVColorPrimaries              "x->color_primaries")
  ;;(frame-color-trc                      enum-AVColorTransferCharacteristic "x->color_trc")
  ;;(frame-colorspace                     enum-AVColorSpace                  "x->colorspace")
  ;;(frame-chroma-location                enum-AVChromaLocation              "x->chroma_location")
  (frame-best-effort-timestamp          integer64                          "x->best_effort_timestamp")
  (frame-pkt-pos                        integer64                          "x->pkt_pos")
  (frame-pkt-duration                   integer64                          "x->pkt_duration")
  ;;(frame-*metadata                      AVDictionary                       "x->*metadata")
  (frame-decode-error-flags             int                                "x->decode_error_flags")
  (frame-channels                       int                                "x->channels")
  (frame-pkt-size                       int                                "x->pkt_size")
  ;;(frame-*hw-frames-ctx                 AVBufferRef                        "x->*hw_frames_ctx")
  ;;(frame-*opaque-ref                    AVBufferRef                        "x->*opaque_ref")
  (frame-crop-top                       size_t                             "x->crop_top")
  (frame-crop-bottom                    size_t                             "x->crop_bottom")
  (frame-crop-left                      size_t                             "x->crop_left")
  (frame-crop-right                     size_t                             "x->crop_right"))

(define (frame-format frame)
  (define format (frame-format* frame))
  (cond ((< format 0) 'none)
        ((frame-pict-type frame) (int->AVPixelFormat format))
        ;;((not (zero? (frame-sample-rate frame))) 'TODO)
        (else format)))

(define (codecpar-format cp)
  (define format (codecpar-format* cp))
  (case (codecpar-type cp)
    ((video) (int->AVPixelFormat format))
    ((audio) format) ;; TODO symbolize AVSampleFormat
    (else format)))

(define-record-printer AVCodecParameters
  (lambda (x p)
    (define type (codecpar-type x))
    (display "#<AVCodecParameters" p)
    (display " type:" p) (display type p)
    (display " id:" p) (display (codecpar-id x) p)
    (display " format:" p) (display (codecpar-format x) p)
    (display " kbps:" p) (display (inexact->exact (floor (/ (codecpar-bit-rate x) 1000))) p)
    (cond ((eq? 'video type)
           (display " size:" p) (display (codecpar-width x) p)
           (display "x" p)      (display (codecpar-height x) p)))
    (display ">" p)))

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
    (display " " p) (display (stream-codecpar stm) p)
    (display ">" p)))

(define-record-printer AVCodec
  (lambda (x p)
    (display "#<AVCodec " p)
    (display (AVCodec-ptr x) p)
    (display ">" p)))

(define-record-printer AVCodecContext
  (lambda (x p)
    (display "#<AVCodecContext " p)
    (display (AVCodecContext-ptr x) p)
    (display ">" p)))

(define-record-printer AVFormatContext
  (lambda (x p)
    (display "#<AVFormatContext " p)
    (display (fmtx-streams x) p)
    (display ">" p)))

(define-record-printer AVFrame
  (lambda (x p)
    (display "#<AVFrame" p)
    (display " format:" p) (display (frame-format x) p)
    (when (frame-pict-type x) ;; guessing this is a video frame
      (display " size:") (display (frame-width x) p) (display "x" p) (display (frame-height x) p))
    (display ">" p)))

(define (av_dump_format! fmtx #!optional (filename ""))
  ((foreign-lambda* void ((AVFormatContext fmtx) (c-string filename))
                    "av_dump_format(fmtx, 0, filename, 0);")
   fmtx filename))

(define (avformat_find_stream_info! fmtx)
  ((foreign-lambda* void ((AVFormatContext fmtx))
                    "avformat_find_stream_info(fmtx, 0);")
   fmtx))

(define (avformat_open_input url #!key (find-stream-info? #t))

  (define fmtx ((foreign-lambda* AVFormatContext () "return(avformat_alloc_context());")))

  (define ret
    ((foreign-lambda* int ((AVFormatContext fmtx)
                           (c-string url))
                      "return(avformat_open_input(&fmtx, url, NULL, NULL));")
     fmtx url))

  (when (< ret 0)
    ;; fmtx freed by avformat_open_input
    (error "cannot open input file" ret))

  (when find-stream-info?
    (avformat_find_stream_info! fmtx))

  (set-finalizer!
   fmtx
     (lambda (fmtx)
       (print "closing " fmtx)
       ((foreign-lambda* void ((AVFormatContext fmtx)) "avformat_close_input(&fmtx);")
        fmtx))))

(define avcodec_find_decoder
  (foreign-lambda* AVCodec ((AVCodecID cid)) "return(avcodec_find_decoder(cid));"))

(define avcodec_parameters_to_context
  (foreign-lambda int "avcodec_parameters_to_context"
                  AVCodecContext AVCodecParameters))

(define (codecx stream/codec #!optional cp)

  (define codec
    (if (AVStream? stream/codec)
        (begin (set! cp (stream-codecpar stream/codec))
               (avcodec_find_decoder (codecpar-id cp)))
        stream/codec))

  (assert (AVCodec? codec))
  (define cx ((foreign-lambda* AVCodecContext ((AVCodec codec))
                               "return(avcodec_alloc_context3(codec));")
              codec))

  (set-finalizer!
   cx
   (lambda (cx)
     (print "freeing " cx)
     ((foreign-lambda* void ((AVCodecContext cx)) "avcodec_free_context(&cx);") cx)))

  (when cp (avcodec_parameters_to_context cx cp))

  (print "avcodec_open2: " ((foreign-lambda* int ((AVCodecContext cx) (AVCodec codec))
                                             "return(avcodec_open2(cx, codec, NULL));")
                            cx codec))
  cx)

(define (avcodec-send-packet cx pkt)
  ((foreign-lambda int "avcodec_send_packet" AVCodecContext AVPacket) cx pkt))


(define (avcodec-receive-frame cx frame)
  ((foreign-lambda int "avcodec_receive_frame" AVCodecContext AVFrame) cx frame))

(define fmtx (avformat_open_input "/tmp/testsrc.mpg"))
(define cx (codecx (fmtx-stream fmtx 0)))
(define pkt (fmtx-read fmtx))

(print "fmtx: " fmtx)
(print "fmtx-streams fmtx:" (fmtx-streams fmtx))
(print "cx: " cx)
(print "pkt: " pkt)

(print "send: " (avcodec-send-packet cx pkt))
(print "send: " (avcodec-send-packet cx pkt))
(print "send: " (avcodec-send-packet cx pkt))

(define frame (make-frame))
(print "frame: " frame)

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

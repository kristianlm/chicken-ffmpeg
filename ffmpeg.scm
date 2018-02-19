(use
 (only data-structures rassoc conc)
 (only srfi-1 list-tabulate)
 (only lolevel number-of-bytes)
 (only srfi-4 make-u8vector u8vector-length
       blob->u8vector/shared s32vector->blob/shared
       list->s32vector ))

(foreign-declare "
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavutil/imgutils.h>
#include <libavfilter/avfilter.h>
#include <libavdevice/avdevice.h>
#include <libavfilter/buffersink.h>
#include <libavfilter/buffersrc.h>
//#include <libavutil/time.h>
")

;; ==================== init ====================
((foreign-lambda* void ()
                  "
avdevice_register_all();
av_register_all();
avformat_network_init();
avfilter_register_all();
"))

(define-syntax define-getters
  (syntax-rules (AVRational)

    ((_ ((argtype argname)) ) (begin))

    ((_ ((argtype argname)) (name  rtype  str no-setter) rest ...)
     (begin (define name
              (foreign-lambda* rtype ((argtype argname)) "return(" str ");"))
            (define-getters ((argtype argname)) rest ...)) )

    ((_ ((argtype argname)) (name AVRational  str) rest ...)
     (begin
       (define name
         (getter-with-setter
          (lambda (x) (error "TODO: sorry, no AVRational return type yet"))
          (lambda (x v)
            ((foreign-lambda* void ((argtype argname) (AVRational val))
                                     str " = (AVRational){val[0],val[1]};") x v))))
       (define-getters ((argtype argname)) rest ...)))



    ((_ ((argtype argname)) (name  rtype  str) rest ...)
     (begin
       (define name
         (getter-with-setter
          (foreign-lambda* rtype ((argtype argname)) "return(" str ");")
          (lambda (x v)
            ((foreign-lambda* void ((argtype argname) (rtype val))
                                     str " = val;") x v))))
       (define-getters ((argtype argname)) rest ...)))))

;; ==================== FLAGS ====================

(define opt/search-children (foreign-value "AV_OPT_SEARCH_CHILDREN" int))
(define opt/fake-obj        (foreign-value "AV_OPT_SEARCH_FAKE_OBJ" int))

(define fmt/nofile          (foreign-value "AVFMT_NOFILE" int))
(define fmt/neednumber      (foreign-value "AVFMT_NEEDNUMBER" int))
(define fmt/show-ids        (foreign-value "AVFMT_SHOW_IDS" int))
(define fmt/globalheader    (foreign-value "AVFMT_GLOBALHEADER" int))
(define fmt/notimestamps    (foreign-value "AVFMT_NOTIMESTAMPS" int))
(define fmt/generic-index   (foreign-value "AVFMT_GENERIC_INDEX" int))
(define fmt/ts-discont      (foreign-value "AVFMT_TS_DISCONT" int))
(define fmt/variable-fps    (foreign-value "AVFMT_VARIABLE_FPS" int))
(define fmt/nodimensions    (foreign-value "AVFMT_NODIMENSIONS" int))
(define fmt/nostreams       (foreign-value "AVFMT_NOSTREAMS" int))
(define fmt/nobinsearch     (foreign-value "AVFMT_NOBINSEARCH" int))
(define fmt/nogensearch     (foreign-value "AVFMT_NOGENSEARCH" int))
(define fmt/no-byte-seek    (foreign-value "AVFMT_NO_BYTE_SEEK" int))
(define fmt/allow-flush     (foreign-value "AVFMT_ALLOW_FLUSH" int))
(define fmt/ts-nonstrict    (foreign-value "AVFMT_TS_NONSTRICT" int))
(define fmt/ts-negative     (foreign-value "AVFMT_TS_NEGATIVE" int))
(define fmt/seek-to-pts     (foreign-value "AVFMT_SEEK_TO_PTS" int))

(define avio/read           (foreign-value "AVIO_FLAG_READ" int))
(define avio/write          (foreign-value "AVIO_FLAG_WRITE" int))
(define avio/read_write     (foreign-value "AVIO_FLAG_READ_WRITE" int))
(define avio/nonblock       (foreign-value "AVIO_FLAG_NONBLOCK" int))
(define avio/direct         (foreign-value "AVIO_FLAG_DIRECT" int))

(define buffersrc/no-check-format (foreign-value "AV_BUFFERSRC_FLAG_NO_CHECK_FORMAT" int))
(define buffersrc/push            (foreign-value "AV_BUFFERSRC_FLAG_PUSH" int))
(define buffersrc/keep-ref        (foreign-value "AV_BUFFERSRC_FLAG_KEEP_REF" int))

;; ====================

(define-record AVPacket ptr data) ;; data can be string/blob
(define-record AVFormatContext ptr)
(define-record AVStream ptr)
(define-record AVFrame ptr)
(define-record AVCodec ptr)
(define-record AVCodecContext ptr)
(define-record AVCodecParameters ptr)
(define-record AVFilter ptr)
(define-record AVFilterContext ptr)
(define-record AVFilterGraph ptr)
(define-record AVInputFormat ptr)
(define-record AVOutputFormat ptr)


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

(define AVSampleFormats
  (foreign-enum
   (none "AV_SAMPLE_FMT_NONE" "")
   (u8   "AV_SAMPLE_FMT_U8"   "unsigned 8 bits")
   (s16  "AV_SAMPLE_FMT_S16"  "signed 16 bits")
   (s32  "AV_SAMPLE_FMT_S32"  "signed 32 bits")
   (flt  "AV_SAMPLE_FMT_FLT"  "float")
   (dbl  "AV_SAMPLE_FMT_DBL"  "double")
   (u8p  "AV_SAMPLE_FMT_U8P"  "unsigned 8 bits planar")
   (s16p "AV_SAMPLE_FMT_S16P" "signed 16 bits planar")
   (s32p "AV_SAMPLE_FMT_S32P" "signed 32 bits planar")
   (fltp "AV_SAMPLE_FMT_FLTP" "float planar")
   (dblp "AV_SAMPLE_FMT_DBLP" "double planar")
   (s64  "AV_SAMPLE_FMT_S64"  "signed 64 bits")
   (s64p "AV_SAMPLE_FMT_S64P" "signed 64 bits planar")))

(define (AVMediaType->int sym) (cond ((assoc sym AVMediaTypes) => cdr)  (else (error "unknown media type" sym))))
(define (int->AVMediaType int) (cond ((rassoc int AVMediaTypes) => car) (else #f)))

(define (AVPictureType->int sym) (cond ((assoc sym AVPictureTypes) => cdr)  (else (error "unknown picture type" sym))))
(define (int->AVPictureType int) (cond ((rassoc int AVPictureTypes) => car) (else #f)))

(define (AVPixelFormat->int* sym) (cond ((assoc sym pixfmts) => cdr)  (else #f)))
(define (AVPixelFormat->int sym) (cond ((assoc sym pixfmts) => cdr)  (else (error "unknown pixel format" sym))))
(define (int->AVPixelFormat int) (cond ((rassoc int pixfmts) => car) (else #f)))

(define (AVSampleFormat->int sym) (cond ((assoc sym AVSampleFormats) => cdr)  (else (error "unknown sample format" sym))))
(define (int->AVSampleFormat int) (cond ((rassoc int AVSampleFormats) => car) (else #f)))

(define (AVCodecID->int sym) (cond ((assoc sym codecs) => cdr) (else (error "unknown codec" sym))))
(define (int->AVCodecID int) (cond ((rassoc int codecs) => car) (else #f)))

(define-foreign-type AVPacket (c-pointer "AVPacket")
  (lambda (x)   (and x   (AVPacket-ptr x)))
  (lambda (ptr) (and ptr (make-AVPacket ptr #f))))

(define-foreign-type AVFormatContext (c-pointer "AVFormatContext")
  (lambda (x)   (and x   (AVFormatContext-ptr x)))
  (lambda (ptr) (and ptr (make-AVFormatContext ptr))))

(define-foreign-type AVStream (c-pointer "AVStream")
  (lambda (x)   (and x   (AVStream-ptr x)))
  (lambda (ptr) (and ptr (make-AVStream ptr))))

(define-foreign-type AVCodecContext (c-pointer "AVCodecContext")
  (lambda (x)   (and x   (AVCodecContext-ptr x)))
  (lambda (ptr) (and ptr (make-AVCodecContext ptr))))

(define-foreign-type AVFrame (c-pointer "AVFrame")
  (lambda (x)   (and x   (AVFrame-ptr x)))
  (lambda (ptr) (and ptr (make-AVFrame ptr))))

(define-foreign-type AVMediaType int
  (lambda (sym) (AVMediaType->int sym))
  (lambda (int) (int->AVMediaType int)))

(define-foreign-type AVPictureType int
  (lambda (sym) (AVPictureType->int sym))
  (lambda (int) (int->AVPictureType int)))

(define-foreign-type AVCodec (c-pointer "AVCodec")
  (lambda (x)   (and x   (AVCodec-ptr x)))
  (lambda (ptr) (and ptr (make-AVCodec ptr))))

(define-foreign-type AVFilter (c-pointer "AVFilter")
  (lambda (x)   (and x   (AVFilter-ptr x)))
  (lambda (ptr) (and ptr (make-AVFilter ptr))))

(define-foreign-type AVFilterContext (c-pointer "AVFilterContext")
  (lambda (x)   (and x   (AVFilterContext-ptr x)))
  (lambda (ptr) (and ptr (make-AVFilterContext ptr))))

(define-foreign-type AVFilterGraph (c-pointer "AVFilterGraph")
  (lambda (x)   (and x   (AVFilterGraph-ptr x)))
  (lambda (ptr) (and ptr (make-AVFilterGraph ptr))))

(define-foreign-type AVInputFormat (c-pointer "AVInputFormat")
  (lambda (x)   (and x   (AVInputFormat-ptr x)))
  (lambda (ptr) (and ptr (make-AVInputFormat ptr))))

(define-foreign-type AVOutputFormat (c-pointer "AVOutputFormat")
  (lambda (x)   (and x   (AVOutputFormat-ptr x)))
  (lambda (ptr) (and ptr (make-AVOutputFormat ptr))))

(define-foreign-type AVCodecParameters (c-pointer "AVCodecParameters")
  (lambda (x)   (and x   (AVCodecParameters-ptr x)))
  (lambda (ptr) (and ptr (make-AVCodecParameters ptr))))

(define-foreign-type AVCodecID int
  (lambda (sym) (if (symbol? sym) (AVCodecID->int sym) sym))
  (lambda (int) (int->AVCodecID int)))

(define-foreign-type AVPixelFormat int
  (lambda (sym) (if (symbol? sym) (AVPixelFormat->int sym) sym))
  (lambda (int) (int->AVPixelFormat int)))

(define-foreign-type AVSampleFormat int
  (lambda (sym) (if (symbol? sym) (AVSampleFormat->int sym) sym))
  (lambda (int) (int->AVSampleFormat int)))

(define-foreign-type AVRational s32vector)

(define-syntax ƒget
  (syntax-rules ()
    ((_ rtype ((type arg)) body_str)
     ((foreign-lambda* rtype ((type arg)) "return(" body_str ");")
      arg))))

;; ==================== AVPacket ====================

(define (packet-init! pkt) (foreign-lambda* void ((AVPacket pkt)) "av_init_packet(pkt);"))

;; (define (av-new-packet pkt size)
;;   (wrap-send/receive ((foreign-lambda int av_new_packet) pkt size)
;;                      pkt 'av-new-packet))

(define (make-packet #!optional buf)
  (define pkt ((foreign-lambda* AVPacket () "return(av_packet_alloc());")))
  (packet-init! pkt)
  (when buf (packet-data-set! pkt buf))
  (set-finalizer!
   pkt
   (lambda (pkt)
     (print "freeing " pkt)
     ((foreign-lambda* void ((AVPacket pkt)) "av_packet_free(&pkt);") pkt))))

(define packet-unref (foreign-lambda void "av_packet_unref" AVPacket))

(define-getters ((AVPacket x))
  (packet-pts          integer64 "x->pts")
  (packet-dts          integer64 "x->dts")
  (packet-data*        c-pointer "x->data")
  (packet-stream-index int       "x->stream_index")
  (packet-size         int       "x->size")
  (packet-stream-index int       "x->stream_index")
  (packet-flags        int       "x->flags")
  (packet-duration     int       "x->duration")
  (packet-pos          int       "x->pos"))

(define input-buffer-padding-size
  (foreign-value "AV_INPUT_BUFFER_PADDING_SIZE" int))

;; TODO: is packet-buffer-ref is writeable and big enough for data,
;; just copy the data over without re-allocating.
(define (packet-data-set! pkt data)
  (wrap-send/receive
   ((foreign-lambda* int ((AVPacket pkt)
                          (u8vector data)
                          (int size))
                     "av_packet_unref(pkt);"
                     "uint8_t *ptr = malloc(size + AV_INPUT_BUFFER_PADDING_SIZE);"
                     "memcpy(ptr, data, size);"
                     "return(av_packet_from_data(pkt, ptr, size));")
    pkt data (u8vector-length data))
   (void) 'packet-data-set!))

(define packet-data
  (getter-with-setter
   (lambda (pkt)
     (if pkt
         (begin
           (define buf (make-u8vector (packet-size pkt)))
           ((foreign-lambda* void (((c-pointer "AVPacket") pkt) (u8vector buf) (int size))
                             "memcpy(buf, pkt->data, size);")
            (AVPacket-ptr pkt)
            buf
            (packet-size pkt)))
         #f)
     buf)
   packet-data-set!))

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



(define (wrap-send/receive ret success loc)
  (cond ((>= ret 0) success)
        ((= ret (foreign-value "AVERROR(EAGAIN)" int)) #f)
        ((= ret (foreign-value "AVERROR_EOF" int))     #!eof)
        ((= ret (foreign-value "AVERROR(EINVAL)" int)) (error loc "einval"))
        ((= ret (foreign-value "AVERROR(ENOMEM)" int)) (error loc "nonmem"))
        (else (error loc "unknown error" ret))))

(define (fmtx-read fmtx #!optional (pkt (make-packet)))
  (wrap-send/receive ((foreign-lambda* int ((AVFormatContext fmtx)
                              (AVPacket pkt))
                         "return(av_read_frame(fmtx, pkt));")
                      fmtx pkt)
                     pkt
                     'fmtx-read))

;; ==================== AVCodecContext ====================

(define-getters ((AVFormatContext x))
  (fmtx-output-format    AVOutputFormat "x->oformat")
  (fmtx-input-format     AVInputFormat  "x->iformat")
  (fmtx-filename         c-string       "x->filename" #f))

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
  (codecpar-sample-aspect-ratio    AVRational                           "cp->sample_aspect_ratio")
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
  ;;(frame-data*                     c-pointer                                                 "x->data")
  ;;(frame-linesize*                 c-pointer                                "x->linesize")
  ;;(frame-**extended-data                uint8_t                            "x->**extended_data")
  (frame-width                          int                                "x->width")
  (frame-height                         int                                "x->height")
  (frame-sample-count                   int                                "x->nb_samples")
  (frame-format*                        int                                "x->format")
  (frame-key-frame                      int                                "x->key_frame")
  (frame-pict-type                      AVPictureType                      "x->pict_type")
  (frame-sample-aspect-ratio            AVRational                         "x->sample_aspect_ratio")
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

(define-getters ((AVCodecContext x))
  (codecx-width                  int            "x->width")
  (codecx-height                 int            "x->height")
  (codecx-pix-fmt                AVPixelFormat  "x->pix_fmt")
  (codecx-bit-rate               integer64      "x->bit_rate")
  (codecx-time-base              AVRational     "x->time_base")
  (codecx-framerate              AVRational     "x->framerate")
  (codecx-slices                 int            "x->slices")
  (codecx-sample-rate            int            "x->sample_rate")
  (codecx-channels               int            "x->channels")
  (codecx-sample-fmt             AVSampleFormat "x->sample_fmt")
  (codecx-frame-size             int            "x->frame_size")
  (codecx-frame-number           int            "x->frame_number")
  (codecx-channel-layout         unsigned-integer64 "x->channel_layout")
  (codecx-request-channel-layout unsigned-integer64 "x->request_channel_layout")
  (codecx-qcompress              float          "x->qcompress")
  (codecx-qblur                  float          "x->qblur")
  (codecx-qmin                   int            "x->qmin")
  (codecx-qmax                   int            "x->qmax")
  (codecx-max-qdiff              int            "x->max_qdiff")
  (codecx-qcompress              float          "x->qcompress")
  (codecx-qblur                  float          "x->qblur")
  (codecx-qmin                   int            "x->qmin")
  (codecx-qmax                   int            "x->qmax")
  (codecx-max-qdiff              int            "x->max_qdiff")
  (codecx-qcompress              float          "x->qcompress")
  (codecx-qblur                  float          "x->qblur")
  (codecx-qmin                   int            "x->qmin")
  (codecx-qmax                   int            "x->qmax")
  (codecx-max-qdiff              int            "x->max_qdiff")
  (codecx-rc-buffer-size         int            "x->rc_buffer_size")
  (codecx-rc-max-rate            integer64      "x->rc_max_rate")
  (codecx-rc-min-rate            integer64      "x->rc_min_rate")
  (codecx-stats-out              c-string       "x->stats_out")
  (codecx-stats-in               c-string       "x->stats_in"))

(define-getters ((AVCodec x))
  (codec-name      c-string  "x->name")
  (codec-long-name c-string  "x->long_name")
  (codec-type      AVMediaType "x->type")
  (codec-id*       int         "x->id")
  (codec-id        AVCodecID   "x->id"))


(define-getters ((AVFilter x))
  (filter-name        c-string  "x->name")
  (filter-description c-string  "x->description"))

(define-getters ((AVFilterContext x))
  (filterx-name        c-string  "x->name")
  (filterx-filter      AVFilter  "x->filter"))
;; ^ TODO: inputs outputs

(define-getters ((AVFilterGraph x))
  (filterg-filter-count    unsigned-int  "x->nb_filters")
  (filterg-sink-links-count int "x->sink_links_count"))
;; ^ TODO: filters

(define-getters ((AVInputFormat x))
  (input-format-name        c-string  "x->name"))

(define-getters ((AVOutputFormat x))
  (output-format-name        c-string  "x->name"))


;; ==================== AVFrame ====================

(define (make-frame #!key w h fmt)
  (set-finalizer! ((foreign-lambda AVFrame "av_frame_alloc"))
                  (lambda (x)
                    (print "freeing frame " x)
                    ((foreign-lambda* void ((AVFrame frame)) "av_frame_free(&frame);") x))))

(define frame-unref (foreign-lambda void "av_frame_unref" AVFrame))


(define (frame-linesize frame plane)
  ((foreign-lambda* int ((AVFrame frame) (int plane)) "return(frame->linesize[plane]);") frame plane))


(define (frame-data* frame plane)
  ((foreign-lambda* c-pointer ((AVFrame frame) (int plane)) "return(frame->data[plane]);")
   frame plane))

(define (frame-data frame plane)

  (define size (* (frame-height frame) (frame-linesize frame plane)))
  (define buf (make-u8vector size))

  ((foreign-lambda* void ((AVFrame frame)
                          (int plane)
                          (u8vector buf)
                          (int size))
                    "memcpy(buf, frame->data[plane], size);")
   frame plane buf size)

  buf)

(define (frame-line frame plane line)
  (define linesize (frame-linesize frame plane))
  (define buf (make-u8vector linesize))
  ((foreign-lambda* void ((AVFrame frame)
                          (int plane)
                          (int line)
                          (u8vector buf)
                          (int linesize))
                    "memcpy(buf, frame->data[plane] + linesize * line, linesize);")
   frame plane line buf linesize)
  buf)

(define frame-format
  (getter-with-setter
   (lambda (frame)
     (define format (frame-format* frame))
     ;; trying to guess whether frame is video/audio
     (cond ((and (not (zero? (frame-width  frame)))
                 (not (zero? (frame-height frame)))) (int->AVPixelFormat format))
           ((not (zero? (frame-sample-rate frame))) (int->AVSampleFormat format))
           (else format)))
   (lambda (frame x)
     (set! (frame-format* frame)
           (cond ((AVPixelFormat->int* x)) ;; * => no error if missing
                 ((AVSampleFormat->int x))
                 (else x))))))

(define codecpar-format
  (getter-with-setter
   (lambda (cp)
     (define format (codecpar-format* cp))
     (case (codecpar-type cp)
       ((video) (int->AVPixelFormat format))
       ((audio) (int->AVSampleFormat format))
       (else format)))
   (lambda (cp x)
     (case (codecpar-type cp)
       ((video) (set! (codecpar-format* cp) (AVPixelFormat->int x)))
       ((audio) (set! (codecpar-format* cp) (AVSampleFormat->int x)))
       (else (set! (codecpar-format* cp) x))))))

(define (assert-ret-zero? ret) (unless (zero? ret) (error "fail" ret)))

(define (frame-make-writable frame)
  (wrap-send/receive
   ((foreign-lambda int "av_frame_make_writable" AVFrame) frame)
   frame 'frame-make-writable))

(define (frame-writeable? frame)
  (= 1 ((foreign-lambda int "av_frame_is_writable" AVFrame) frame)))

;; ==================== AVStream accessors ====================

(define-getters ((AVStream x))
  (stream-index            int               "x->index")
  (stream-time-base        AVRational       "x->time_base")
  (stream-avg-frame-rate   AVRational       "x->avg_frame_rate")
  (stream-id               int               "x->id")
  (stream-codecpar         AVCodecParameters "x->codecpar"))

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
    (display (codec-type x) p) (display ":" p) (display (codec-id x) p)
    (display " \"" p) (display (codec-long-name x) p) (display "\"" p)
    (display ">" p)))

(define-record-printer AVFilter
  (lambda (x p)
    (display "#<AVFilter " p)
    (display (filter-name x) p)
    (display " \"" p) (display (filter-description x) p) (display "\"" p)
    (display ">" p)))

(define-record-printer AVFilterContext
  (lambda (x p)
    (display "#<AVFilterContext " p)
    (display (filter-name (filterx-filter x)) p)
    (display " " p)
    (display (filterx-name x) p)
    (display ">" p)))

(define-record-printer AVFilterGraph
  (lambda (x p)
    (display "#<AVFilterGraph " p)
    (display (filterg-filter-count x) p)
    (display " " p)
    (display (filterg-sink-links-count x) p)
    (display ">" p)))

(define-record-printer AVInputFormat
  (lambda (x p)
    (display "#<AVInputFormat " p)
    (display (input-format-name x) p)
    (display ">" p)))

(define-record-printer AVOutputFormat
  (lambda (x p)
    (display "#<AVOutputFormat " p)
    (display (output-format-name x) p)
    (display ">" p)))

(define-record-printer AVCodecContext
  (lambda (x p)
    (display "#<AVCodecContext " p)
    (display (AVCodecContext-ptr x) p)
    (display ">" p)))

(define-record-printer AVFormatContext
  (lambda (x p)
    (display "#<AVFormatContext " p)
    (when (fmtx-output-format x) (display (output-format-name (fmtx-output-format x)) p))
    (when (fmtx-input-format  x) (display (input-format-name (fmtx-input-format x)) p))
    (display " \"") (display (fmtx-filename x) p) (display "\" " p)
    (display (fmtx-streams x) p)
    (display ">" p)))

(define-record-printer AVFrame
  (lambda (x p)
    (display "#<AVFrame" p)
    (display " format:" p) (display (frame-format x) p)
    (when (and (not (zero? (frame-width x)))
               (not (zero? (frame-height x))))
      (display " size:") (display (frame-width x) p) (display "x" p) (display (frame-height x) p))
    (when (and (not (zero? (frame-sample-count x))))
      (display " samples: " (frame-sample-count x)))
    (display ">" p)))

(define (av_dump_format! fmtx #!optional (filename ""))
  ((foreign-lambda* void ((AVFormatContext fmtx) (c-string filename))
                    "av_dump_format(fmtx, 0, filename, 0);")
   fmtx filename))

(define (avformat_find_stream_info! fmtx)
  ((foreign-lambda* void ((AVFormatContext fmtx))
                    "avformat_find_stream_info(fmtx, 0);")
   fmtx))

(define (avformat-open-input url #!optional ifmt #!key (find-stream-info? #t))

  ;; no finalizer yet because it's freed by avformat_open_input on error
  (define fmtx ((foreign-lambda* AVFormatContext () "return(avformat_alloc_context());")))

  (when (string? ifmt)
    (set! ifmt (find-input-format ifmt)))

  (define ret
    ((foreign-lambda* int ((AVFormatContext fmtx)
                           (c-string url)
                           (AVInputFormat ifmt))
                      "return(avformat_open_input(&fmtx, url, ifmt, NULL));")
     fmtx url ifmt))

  (when (< ret 0)
    ;; fmtx freed by avformat-open-input
    (error "cannot open input file" ret))

  (when find-stream-info?
    (avformat_find_stream_info! fmtx))

  (set-finalizer!
   fmtx
     (lambda (fmtx)
       (print "closing " fmtx)
       ((foreign-lambda* void ((AVFormatContext fmtx)) "avformat_close_input(&fmtx);")
        fmtx))))

(define (avformat-open-output format filename)

  (define fmx
    ((foreign-lambda*
      AVFormatContext ((AVOutputFormat ofmt)
                       (c-string ofmts) ;; format name
                       (c-string filename))
      "AVFormatContext *fmx = NULL;"
      "avformat_alloc_output_context2(&fmx, ofmt, ofmts, filename);"
      "return(fmx);")
     (if (string? format) #f format) ;; AVOutputFormat
     (if (string? format) format #f)
     filename))

  (unless fmx
    (error 'avformat-open-output
           "could not open output format context"))

  (set-finalizer!
   fmx
   (lambda (fmx)
     (print "freeing " fmx)
     ((foreign-lambda* void ((AVFormatContext fmx))
                       "
if (!(fmx->oformat->flags & AVFMT_NOFILE))
   avio_closep(&fmx->pb);

avformat_free_context(fmx);")
      fmx))))

(define (avformat-new-stream! fmx #!optional codec)
  (or ((foreign-lambda AVStream "avformat_new_stream"
                       AVFormatContext AVCodec)
       fmx codec)
      (error 'avformat-new-stream! "could create stream from" fmx)))

(define (avformat-write-header fmx)
  (wrap-send/receive
   ((foreign-lambda* int ((AVFormatContext fmx))
                     "
    if (!(fmx->oformat->flags & AVFMT_NOFILE)) {
        int ret = avio_open(&fmx->pb, fmx->filename, AVIO_FLAG_WRITE);
        if (ret < 0) {
            fprintf(stderr, \"Could not open output file '%s'\", fmx->filename);
            return -100;
        }
    }
    return(avformat_write_header(fmx, NULL));
") fmx)
   fmx
   'avformat-write-header))

(define fmtx-write
  (foreign-lambda int "av_interleaved_write_frame"
                  AVFormatContext AVPacket))

;; why are the prefixes avformat_ vs av_ not consistent?
(define avformat-write-trailer
  (foreign-lambda int "av_write_trailer" AVFormatContext))


(define (guess-format short-name #!optional filename mime/type)
  ((foreign-lambda AVOutputFormat "av_guess_format"
                   (const c-string)
                   (const c-string)
                   (const c-string))
   short-name filename mime/type))

(define (guess-codec fmt media-type #!optional short-name filename mime/type)
  ((foreign-lambda AVCodecID "av_guess_codec"
                   AVOutputFormat
                   c-string c-string c-string
                   AVMediaType)
   fmt short-name filename mime/type media-type))


(define (find-decoder c)
  (if (string? c)
      ((foreign-lambda AVCodec "avcodec_find_decoder_by_name" c-string) c)
      ((foreign-lambda AVCodec "avcodec_find_decoder" AVCodecID) c)))

(define (find-encoder c)
  (if (string? c)
      ((foreign-lambda AVCodec "avcodec_find_encoder_by_name" c-string) c)
      ((foreign-lambda AVCodec "avcodec_find_encoder" AVCodecID) c)))

(define (find-filter c)
  ((foreign-lambda AVFilter "avfilter_get_by_name" c-string) c))

(define (find-input-format c) ((foreign-lambda AVInputFormat "av_find_input_format" c-string) c))

(define avcodec_parameters_to_context
  (foreign-lambda int "avcodec_parameters_to_context"
                  AVCodecContext AVCodecParameters))

(define (avcodec-open cx #!optional codec)
  (print "avcodec_open2: "
         ((foreign-lambda* int ((AVCodecContext cx) (AVCodec codec))
                           "return(avcodec_open2(cx, codec, NULL));")
          cx codec)))

(define (avcodec-alloc-context codec)
  (set-finalizer!
   ((foreign-lambda* AVCodecContext ((AVCodec codec))
                     "return(avcodec_alloc_context3(codec));")
    codec)
   (lambda (cx)
     (print "freeing " cx)
     ((foreign-lambda* void ((AVCodecContext cx)) "avcodec_free_context(&cx);") cx))))

(define (codecx stream/codec #!optional cp (open? #t))

  (define codec
    (if (AVStream? stream/codec)
        (begin (set! cp (stream-codecpar stream/codec))
               (find-decoder (codecpar-id cp)))
        stream/codec))

  (assert (AVCodec? codec))
  (define cx (avcodec-alloc-context codec))
  (when cp (avcodec_parameters_to_context cx cp))
  (when open? (avcodec-open cx codec))
  cx)

(define (avcodec-send-packet cx pkt)
  (wrap-send/receive ((foreign-lambda int "avcodec_send_packet"
                                      AVCodecContext AVPacket)
                      cx pkt)
                     pkt   'avcodec-send-packet))

(define (avcodec-send-frame cx frame)
  (wrap-send/receive ((foreign-lambda int "avcodec_send_frame"
                                      AVCodecContext AVFrame)
                      cx frame)
                     frame 'avcodec-send-frame))

(define (avcodec-receive-packet cx #!optional (pkt (make-packet)))
  (wrap-send/receive ((foreign-lambda int "avcodec_receive_packet"
                                      AVCodecContext AVPacket)
                      cx pkt)
                     pkt   'avcodec-receive-packet))

(define (avcodec-receive-frame cx #!optional (frame (make-frame)))
  (wrap-send/receive ((foreign-lambda int "avcodec_receive_frame"
                                      AVCodecContext AVFrame)
                      cx frame)
                     frame 'avcodec-receive-frame))

(define (image-get-buffer-size format w h align)
  (let ((ret ((foreign-lambda int "av_image_get_buffer_size" AVPixelFormat int int int)
              format w h align)))
    (if (< ret 0)
        (error 'image-get-buffer-size "unknown error" ret)
        ret)))

(define (frame-get-buffer frame align)
  (wrap-send/receive
   ((foreign-lambda int "av_frame_get_buffer" AVFrame int) frame align)
   #t 'frame-get-buffer))

(define (make-flg)
  (set-finalizer!
   ((foreign-lambda AVFilterGraph "avfilter_graph_alloc"))
   (lambda (x)
     (print "freeing " x)
     ((foreign-lambda* void ((AVFilterGraph x)) "avfilter_graph_free(&x);") x))))

(define (make-flx fg filter name #!optional (init-str ""))
  (define flx
    ((foreign-lambda AVFilterContext "avfilter_graph_alloc_filter"
                     AVFilterGraph
                     AVFilter
                     c-string)
     fg
     (if (string? filter) (find-filter filter) filter)
     name))
  (avfilter-init-str flx init-str) ;; segfaults if this isn't run
  flx)

(define (avfilter-graph-config fg)
  (wrap-send/receive ((foreign-lambda int "avfilter_graph_config" AVFilterGraph c-pointer) fg #f)
                     #t 'avfilter-graph-config))

(define (avfilter-link src srcpad dst dstpad)
  (wrap-send/receive ((foreign-lambda int "avfilter_link"
                                      AVFilterContext unsigned-int
                                      AVFilterContext unsigned-int)
                      src srcpad dst dstpad)
                     #t
                     'avfilter-link))

(define (av-buffersink-get-frame fx #!optional (frame (make-frame)))
  (wrap-send/receive ((foreign-lambda int "av_buffersink_get_frame" AVFilterContext AVFrame)
                      fx frame)
                     frame 'av-buffersink-get-frame))

(define (av-buffersrc-add-frame bufferfx frame #!optional (flags buffersrc/keep-ref))
  (wrap-send/receive
   ((foreign-lambda int "av_buffersrc_add_frame_flags"
                    AVFilterContext AVFrame int)
    bufferfx frame flags)
   frame
   'av-buffersrc-add-frame))

(define (av-frame-get-buffer frame align)
  (wrap-send/receive ((foreign-lambda int "av_frame_get_buffer" AVFrame int) frame align)
                     #t 'av-frame-get-buffer))

(define (avfilter-init-str fx c)
  (wrap-send/receive ((foreign-lambda int "avfilter_init_str" AVFilterContext c-string) fx c)
                     #t 'avfilter-init-str))

;; (define (avfilter-graph-parse-ptr )
;;   ((foreign-lambda int "avfilter_graph_parse_ptr" AVFilterGraph c-string AVFilterInOut AVFilterInOut)))

(define (avfilter-graph-dump fg)
  ((foreign-lambda* c-string* ((AVFilterGraph fg)
                               ;;((c-pointer c-string) options)
                               )
                    "char *buf, *dst;"
                    "buf = avfilter_graph_dump(fg, NULL);"
                    "dst = malloc(strlen(buf) + 1);"
                    "strcpy(dst, buf);"
                    "av_free(buf);"
                    "return(dst);")
   fg))

(define av-buffersink-type (foreign-lambda AVMediaType "av_buffersink_get_type" AVFilterContext))


(define (obj->ptr obj)
  ((cond
    ((AVPacket? obj) AVPacket-ptr)
    ((AVFormatContext? obj) AVFormatContext-ptr)
    ((AVStream? obj) AVStream-ptr)
    ((AVFrame? obj) AVFrame-ptr)
    ((AVCodec? obj) AVCodec-ptr)
    ((AVCodecContext? obj) AVCodecContext-ptr)
    ((AVCodecParameters? obj) AVCodecParameters-ptr)
    ((AVFilter? obj) AVFilter-ptr)
    ((AVFilterContext? obj) AVFilterContext-ptr)
    ((AVFilterGraph? obj) AVFilterGraph-ptr)
    ((AVInputFormat? obj) AVInputFormat-ptr)
    ((AVOutputFormat? obj) AVOutputFormat-ptr)
    (else values))
   obj))

;; TODO: pick pointers for known types
(define (opt-serialize obj #!optional (opt_flags 0) (flags 0)
                       (key_val_sep #\=) (pairs_sep #\;))
 ((foreign-lambda* c-string* ((c-pointer         obj)
                              (int          opt_flags)
                              (int          flags)
                              ((const char) key_val_sep)
                              ((const char) pairs_sep))
                   "char* buffer;"
                   "av_opt_serialize(obj, opt_flags, flags, &buffer, key_val_sep, pairs_sep);"
                   "return(buffer);")
  (obj->ptr obj) opt_flags flags key_val_sep pairs_sep))


(define (opt-set obj name val #!optional (search-flags opt/search-children))
  (wrap-send/receive ((foreign-lambda int "av_opt_set_bin"
                                      c-pointer
                                      (const c-string) ;; name
                                      u8vector         ;; val
                                      int              ;; size
                                      int)
                      (obj->ptr obj) name val (u8vector-length val) search-flags)
                     #t 'opt-set))

(define (opt-set-pix-fmts obj pix-fmts)
  (opt-set obj "pix_fmts"
           (blob->u8vector/shared
            (s32vector->blob/shared
             (list->s32vector (map AVPixelFormat->int pix-fmts))))))

(define pix-fmt->tag
  (foreign-lambda int "avcodec_pix_fmt_to_codec_tag" AVPixelFormat))


(define av-interleaved-write-uncoded-frame
  (foreign-lambda int "av_interleaved_write_uncoded_frame"
                  AVFormatContext ;; *s
                  int             ;; stream_index
                  AVFrame))       ;; *frame

(define av-write-uncoded-frame-query
  (foreign-lambda int "av_write_uncoded_frame_query"
                  AVFormatContext ;; *s
                  int))           ;; stream_index

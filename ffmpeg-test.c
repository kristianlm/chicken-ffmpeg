
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
//#include <libavdevice/avdevice.h>
//#include <libavutil/time.h>


typedef struct PlayerState {

  AVFormatContext *fmt_ctx;
  AVInputFormat *iformat;
  AVCodec *codec;

  AVFrame *frame;
  AVStream *stream;

} PlayerState;


#define TRY(x) { if(( x ) != 0) {fprintf(stderr, "*** error in "#x"\n"); exit(-1);} }


static void pgm_save(unsigned char *buf, int wrap, int xsize, int ysize, char* filename)
{
    FILE *f;
    int i;

    f = fopen(filename, "w");
    fprintf(f, "P5\n%d %d\n%d\n", xsize, ysize, 255);
    for (i = 0; i < ysize; i++)
        fwrite(buf + i * wrap, 1, xsize, f);
    fclose(f);
}


int main() {
  PlayerState is = {};
  char filename[] = "/store/backup/nemo/Videos/Camera/20150616_002.rot.mp4";
  //char filename[] = "/tmp/testsrc.mp4";
  printf("welcome\n");

  //avdevice_register_all();
  av_register_all();
  avformat_network_init();

  // read header, find out what's going on
  TRY(avformat_open_input(&is.fmt_ctx, filename, NULL, NULL));

  // find stream index
  TRY(avformat_find_stream_info(is.fmt_ctx, 0));
  av_dump_format(is.fmt_ctx, 0, filename, 0);
  int stream_idx = av_find_best_stream(is.fmt_ctx, AVMEDIA_TYPE_VIDEO, -1, -1, &is.codec, 0);
  printf("stream index: %d\n", stream_idx);

  // open codec
  printf("codec: %p\n", is.codec);
  AVStream *stream = is.fmt_ctx->streams[stream_idx];
  AVCodecContext *codec_ctx = avcodec_alloc_context3(is.codec);
  if(avcodec_parameters_to_context(codec_ctx, is.fmt_ctx->streams[stream_idx]->codecpar) < 0) {
    fprintf(stderr, "avcodec_parameters_to_context failed");
    return 120;
  }
  TRY(avcodec_open2(codec_ctx, is.codec, NULL));
  
  printf("codec size: %dx%d\n", codec_ctx->width, codec_ctx->height);
  
  
  AVFrame *frame = av_frame_alloc();
  if(!frame) {
    fprintf(stderr, "could not allocate video frame\n");
    return 123;
  }

  AVPacket pkt;
  av_init_packet(&pkt);
  pkt.data = NULL;
  pkt.size = 0;

  printf("iformat '%s' [%s]\n", is.fmt_ctx->iformat->name, is.fmt_ctx->iformat->long_name);

  int ret = 0;
  char buf[2046];
  printf("init done\n\n");
  while (ret >= 0) {
    ret  = av_read_frame(is.fmt_ctx, &pkt);
    //av_pkt_dump2(stderr, &pkt, 0, is.fmt_ctx->streams[pkt.stream_index]);
    fprintf(stderr, "pkt stream #%d: %6d bytes: ", pkt.stream_index, pkt.size);

    if(pkt.stream_index == stream_idx) {
      int rrr = avcodec_send_packet(codec_ctx, &pkt);
      if(rrr != 0) {
        printf("bad avcodec_send_packet %x\n", rrr);
        switch(rrr) {
        case AVERROR(EAGAIN): printf("(eagain send)"); break;
        case AVERROR(EINVAL): printf("(av_send_packet enval)"); break;
        }
        break;
      }
      
      int ret2 = 0;
      while(ret2 >= 0) {
        ret2 = avcodec_receive_frame(codec_ctx, frame);
        if (ret2 == AVERROR(EAGAIN)) {
          fprintf(stderr, "(eagain frame)");
          break;
        } else if(ret2 == AVERROR_EOF) {
          fprintf(stderr, "eof\n");
          break;
        } else if (ret2 < 0) {
          fprintf(stderr, "Error during decoding\n");
          break;
        }
        fprintf(stderr, "FRAME %3d ", codec_ctx->frame_number);
        //if(codec_ctx->frame_number > 50) {ret = -1;}

        char buf[1024];
        snprintf(buf, sizeof(buf), "out-%06d.pbm", codec_ctx->frame_number);
        pgm_save(frame->data[0], frame->linesize[0],
                 frame->width, frame->height, buf);

        
        av_frame_unref(frame);
        /* the picture is allocated by the decoder. no need to
           free it */
        //snprintf(buf, sizeof(buf), "%s-%d", filename, codec_ctx->frame_number);
        //pgm_save(frame->data[0], frame->linesize[0], frame->width, frame->height, buf);
      }
    }
    printf("\n");
    av_packet_unref(&pkt);
  }

 /* while(1) { */
 /*    int ret = avcodec_receive_frame(codec_ctx, frame); */
 /*    printf("packet: %d \n", ret); */
 /*    if(ret < 0) { */
 /*      if(ret == AVERROR_EOF) { printf("eof\n");break; } */
 /*      else { */
 /*        printf("unknown error %d\n", ret); */
 /*        break; */
 /*      } */
 /*    } */
 /*  } */

  printf("done\n");
  av_packet_unref(&pkt);
  avcodec_free_context(&codec_ctx);
  av_frame_free(&frame);
  avformat_close_input(&is.fmt_ctx);
  return 0;
}


/* void PARSER_test(char* filename, PlayerState is) { */
  
/*   FILE* f = fopen(filename, "rb"); */
/*   const int INBUF_SIZE = 4096; */
/*   uint8_t inbuf[INBUF_SIZE + AV_INPUT_BUFFER_PADDING_SIZE]; */
/*   uint8_t *data; */
/*   size_t data_size; */
/*   int ret = 0; */
/*   AVCodecParserContext *parser; */

  
/*     parser = av_parser_init(is.codec->id); */
/*     if (!parser) { */
/*         fprintf(stderr, "parser not found\n"); */
/*         exit(1); */
/*     } */

/*       while (!feof(f)) { */
/*         /\* read raw data from the input file *\/ */
/*         data_size = fread(inbuf, 1, INBUF_SIZE, f); */
/*         if (!data_size) { */
/*           printf("eof fread\n"); */
/*             break; */
/*         } */
/*         printf("read %d bytes\n", data_size); */

/*         /\* use the parser to split the data into frames *\/ */
/*         data = inbuf; */
/*         while (data_size > 0) { */
/*             ret = av_parser_parse2(parser, codec_ctx, &pkt.data, &pkt.size, */
/*                                    data, data_size, AV_NOPTS_VALUE, AV_NOPTS_VALUE, 0); */
/*             if (ret < 0) { */
/*                 fprintf(stderr, "Error while parsing\n"); */
/*                 exit(1); */
/*             } */
/*             data      += ret; */
/*             data_size -= ret; */

/*             if (pkt.size) */
/*               printf("got packet of size %d\n", pkt.size); */
/*               //decode(c, frame, pkt, outfilename); */
/*         } */
/*     } */
/* } */

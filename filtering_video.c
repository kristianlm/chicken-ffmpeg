/*
 * Copyright (c) 2010 Nicolas George
 * Copyright (c) 2011 Stefano Sabatini
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

/**
 * @file
 * API example for decoding and filtering
 * @example filtering_video.c
 */

#define _XOPEN_SOURCE 600 /* for usleep */
#include <unistd.h>

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavfilter/avfiltergraph.h>
#include <libavfilter/buffersink.h>
#include <libavfilter/buffersrc.h>
#include <libavutil/opt.h>

AVFilterContext *buffersink_ctx;
AVFilterContext *testsrc_ctx;
AVFilterGraph *filter_graph;
static int64_t last_pts = AV_NOPTS_VALUE;

static int init_filters()
{
    int ret = 0;
    //exit(-1);
 end:
    return ret;
}

static void display_frame(const AVFrame *frame, AVRational time_base)
{
    int x, y;
    uint8_t *p0, *p;
    int64_t delay;

    if (frame->pts != AV_NOPTS_VALUE) {
        if (last_pts != AV_NOPTS_VALUE) {
            /* sleep roughly the right amount of time;
             * usleep is in microseconds, just like AV_TIME_BASE. */
            delay = av_rescale_q(frame->pts - last_pts,
                                 time_base, AV_TIME_BASE_Q);
            if (delay > 0 && delay < 1000000)
              usleep(delay);
        }
        last_pts = frame->pts;
    }

    /* Trivial ASCII grayscale display. */
    p0 = frame->data[0];
    puts("\033c");
    for (y = 0; y < frame->height; y++) {
        p = p0;
        for (x = 0; x < frame->width; x++)
            putchar(" .-+#"[*(p++) / 52]);
        putchar('\n');
        p0 += frame->linesize[0];
    }
    fflush(stdout);
}

int main(int argc, char **argv)
{
    int ret;
    AVFrame *filt_frame = av_frame_alloc();

    if (!filt_frame) {
        perror("Could not allocate frame");
        exit(1);
    }

    av_register_all();
    avfilter_register_all();

    AVFilter *testsrc  = avfilter_get_by_name("testsrc");
    AVFilter *buffersink = avfilter_get_by_name("buffersink");


    filter_graph = avfilter_graph_alloc();
    if (!filter_graph) {
        ret = AVERROR(ENOMEM);
        goto end;
    }

    testsrc_ctx = avfilter_graph_alloc_filter(filter_graph, testsrc, "in");
    avfilter_init_str(testsrc_ctx, "duration=5");

    buffersink_ctx = avfilter_graph_alloc_filter(filter_graph, buffersink, "out");
    avfilter_init_str(buffersink_ctx, "");
    
    enum AVPixelFormat pix_fmts[] = { AV_PIX_FMT_GRAY8, AV_PIX_FMT_NONE };
    ret = av_opt_set_int_list(buffersink_ctx, "pix_fmts", pix_fmts,
                              AV_PIX_FMT_NONE, AV_OPT_SEARCH_CHILDREN);
    if (ret < 0) {
        av_log(NULL, AV_LOG_ERROR, "Cannot set output pixel format\n");
        goto end;
    }

    avfilter_link(testsrc_ctx, 0, buffersink_ctx, 0);

    if ((ret = avfilter_graph_config(filter_graph, NULL)) < 0)
        goto end;

    printf(avfilter_graph_dump(filter_graph, NULL));

    printf("starting\n");
    while(1) {
      ret = av_buffersink_get_frame(buffersink_ctx, filt_frame);
      printf("%d\n", filt_frame->format);
      if (ret == AVERROR(EAGAIN)) {
        printf("eagain\n");
      } else if(ret == AVERROR_EOF) {
        printf("eof\n");
        break;
      } else if (ret < 0) {
        printf("BAD %d!\n", ret);
        goto end;
      }
      //display_frame(filt_frame, buffersink_ctx->inputs[0]->time_base);
      av_frame_unref(filt_frame);
    }
 end:
    
    avfilter_graph_free(&filter_graph);
    av_frame_free(&filt_frame);

    if (ret < 0 && ret != AVERROR_EOF) {
        fprintf(stderr, "Error occurred: %s\n", av_err2str(ret));
        exit(1);
    }

    exit(0);
}


  [ffmpeg]: http://ffmpeg.org
  [CHICKEN Scheme]: http://call-cc.org

# chicken-ffmpeg

[ffmpeg] 3.4 bindings for [CHICKEN Scheme] 4.

The status of this egg is early alpha. [ffmpeg] is an ambitious API
and not all of it is accessible from this egg. However, it's complete
enough to do some basic operations.

## Examples

Please see the example sources for how to use this egg.

## Memory

[ffmpeg] decouples some structures like `AVPacket` and `AVFrame` with
the actual data-buffers. Sometimes you have to call `packet-unref` and
`frame-unref` to avoid memory leakage.

## Big Buck Bunny

The sample movie clip `big-buck-bunny.mp4` was taken from blender like this:

```sh
$ ffmpeg -i "http://download.blender.org/peach/bigbuckbunny_movies/big_buck_bunny_480p_surround-fix.avi" \
    -vf scale=256x144\
    -an \
    -b:v 20k \
    -t 02:00 \
    big-buck-bunny.mp4
```

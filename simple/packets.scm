(use ports)
(load "enum-pixfmts.so")
(load "ffmpeg.so")

(define fmtx (avformat-open-input "/store/backup/nemo/Videos/Camera/20150616_002.rot.mp4"))

(port-for-each
 (lambda (pkt)
   (print "pkt: " pkt))
 (let ((pkt (make-packet)))
   (lambda ()
     (packet-unref pkt) ;; unref previous instance, otherwise we leak
     (fmtx-read fmtx pkt))))

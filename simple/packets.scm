(use ports ffmpeg)
;;(load "enum-pixfmts.so")
;;(load "ffmpeg.so")

(define fmtx (avformat-open-input (list-ref (command-line-arguments) 0)))
(print fmtx)
(port-for-each
 (lambda (pkt)
   (print "pkt: " pkt))
 (let ((pkt (make-packet)))
   (lambda ()
     (packet-unref pkt) ;; unref previous instance, otherwise we leak
     (fmtx-read fmtx pkt))))

(use ports ffmpeg)

(unless (= 1 (length (command-line-arguments)))
  (print "usage: csi -s packets.scm <media-file>
  reads all packets and print their info")
  (exit))

(define fmx (avformat-open-input (list-ref (command-line-arguments) 0)))
(print "fmx: " fmx)
(port-for-each
 (lambda (pkt)
   (print "pkt: " pkt))
 (let ((pkt (make-packet)))
   (lambda ()
     (packet-unref pkt) ;; unref previous instance, otherwise we leak
     (fmtx-read fmx pkt))))

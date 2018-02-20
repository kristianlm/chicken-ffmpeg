;; tired of recompiling ffmpeg.scm to test something? the
;; define-getters part can be tested in isolation here.
;;
;; csc getset-test && ./getset-test
(include "getset.scm")

(foreign-declare "
struct Point {
  int x, y, z;
};
")

(define-getters point (((c-pointer "struct Point") x))
  (x int "x->x")
  (y int "x->y")
  (z int "x->z"))

(define point ((foreign-lambda* c-pointer () "return(malloc(sizeof(struct Point)));")))
(print "point: " point)
(set! (point-x point) 400)
(print "point-fields: " (point-fields point))
(set! (point-fields point) '(z: -1 y: -20))
(print "point-fields: " (point-fields point))


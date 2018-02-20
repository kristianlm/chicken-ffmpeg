(use (only srfi-69 make-hash-table hash-table-ref hash-table-set! hash-table-fold))

;; example usecase:
;;
;; (foreign-declare "
;; struct Point {
;;   int x, y, z;
;; };
;; ")
;; (define-getters point (((c-pointer "struct Point") x))
;;   (x int "x->x")
;;   (y int "x->y")
;;   (z int "x->z"))
;; (define point ((foreign-lambda* c-pointer () "return(malloc(sizeof(struct Point)));")))
;; (print "point: " point)
;; (set! (point-x point) 400)
;; (print "point-fields: " (point-fields point))
;; (set! (point-fields point) '(z: -1 y: -20))
;; (print "point-fields: " (point-fields point))


(define-syntax syntax-concat
  (er-macro-transformer
   (lambda (x r t)
     (string->symbol (apply conc (cdr x))))))

;; (define-concat (a "-b") #t) ==> a-b
(define-syntax define-concat
  (er-macro-transformer
   (lambda (x r t)
     (let ((name (string->symbol (apply conc (cadr x)))))
       `(,(r 'define) ,name ,@(cddr x))))))

(define-syntax getter-add-field!
  (syntax-rules ()
    ((_ record field)
     (hash-table-set! (syntax-concat "*" record "-fields*")
                      (string->keyword (symbol->string 'field))
                      (syntax-concat record "-" field)))))

(define-syntax define-fields-getter
  (syntax-rules ()
    ((_ record)
     (define-concat (record "-fields")
       (getter-with-setter
        (lambda (x)
          ;; return plist, eg (x: 10 y: 1)
          (hash-table-fold (syntax-concat "*" record "-fields*")
                           (lambda (k v s) (cons k (cons (v x) s)))
                           '()))
        (lambda (x plist)
          (let loop ((plist plist))
            (when (pair? plist)
              (let ((key (car plist))
                    (val (cadr plist)))
                (set! ((hash-table-ref (syntax-concat "*" record "-fields*") key
                                       (lambda () (error (conc "unknown field for " 'record) key)))
                       x) val)
                (loop (cddr plist)))))))))))

(define-syntax define-getters*
  (syntax-rules (AVRational)

    ((_ record ((argtype argname)) ) (begin))

    ((_ record ((argtype argname)) (field rtype  str no-setter) rest ...)
     (begin (define-concat (record "-" field)
              (foreign-lambda* rtype ((argtype argname)) "return(" str ");"))
            (define-getters* record ((argtype argname)) rest ...)) )

    ((_ record ((argtype argname)) (field AVRational  str) rest ...)
     (begin
       (define-concat (record "-" field)
         (getter-with-setter
          (lambda (x) (error "TODO: sorry, no AVRational return type yet"))
          (lambda (x v)
            ((foreign-lambda* void ((argtype argname) (AVRational val))
                                     str " = (AVRational){val[0],val[1]};") x v))))
       (define-getters* record ((argtype argname)) rest ...)))

    ((_ record ((argtype argname)) (field  rtype  str) rest ...)
     (begin
       (define-concat (record "-" field)
         (getter-with-setter
          (foreign-lambda* rtype ((argtype argname)) "return(" str ");")
          (lambda (x v)
            ((foreign-lambda* void ((argtype argname) (rtype val))
                                     str " = val;") x v))))
       (getter-add-field! record field)
       (define-getters* record ((argtype argname)) rest ...)))))

(define-syntax define-getters
  (syntax-rules ()
    ((_ record ((argtype argname)) specs ...)
     (begin
       (define-concat ("*" record "-fields*") (make-hash-table))
       (define-getters* record ((argtype argname)) specs ...)
       (define-fields-getter record)))))


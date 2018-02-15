
(define-syntax foreign-enum
  (syntax-rules ()
    ((_ ) '())

    ((_ (type ENUM  description) rest ...)
     (cons (cons 'type (foreign-value ENUM int))
           (foreign-enum rest ...)))
    
    ((_ (type ENUM CHECK description) rest ...)
     (cons (cons 'type ((foreign-lambda* scheme-object ()
                                         "
#if " CHECK "
 C_return (C_fix(" ENUM "));
#endif
C_return (C_SCHEME_FALSE);
")))
           (foreign-enum rest ...)))))

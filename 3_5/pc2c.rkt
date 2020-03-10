#lang racket

(provide pc2c compile/run)

(define reg-funcs '())
(define reg-pc 'no-pc)
(define dismount-var 'no-dis)
(define construct-var 'no-const)

(define add-func
  (lambda (func)
    (set! reg-funcs (cons func reg-funcs))))

(define is-func?
  (lambda (func)
    (assv func reg-funcs)))

(define reg-unions '())

(define check-args
  (lambda (union args)
    (cond
     [(null? args) #t]
     [(memq (car args) (cdr args))
      (error 'define-union "duplicated variant `~a' in union `~a'\n" (car args) (car union))]
     [else (check-args union (cdr args))])))

(define add-union
  (lambda (union)
    (if (not (lookup-union (car union)))
        (begin (check-args union (cadr union)) 
               (set! reg-unions (cons union reg-unions)))
        (error 'define-union "duplicated definition of union-type `~a'\n" (car union)))))

(define reg-regs '())

(define init-storage
  (lambda ()
    (set! reg-funcs '())
    (set! reg-unions '())
    (set! reg-regs '())))

(define new-safe-char
  (lambda (char)
    (cond
      [(eq? #\? char) "r__q__"]
      [(eq? #\! char) "r__f__"]
      [(eq? #\. char) "r__d__"]
      [(eq? #\+ char) "r__p__"]
      [(eq? #\- char) "r__m__"]
      [(eq? #\* char) "r__t__"]
      [(eq? #\/ char) "r__di__"]
      [(eq? #\< char) "r__lt__"]
      [(eq? #\> char) "r__gt__"]
      [(eq? #\: char) "r__co__"]
      [(eq? #\$ char) "r__do__"]
      [(eq? #\% char) "r__pe__"]
      [(eq? #\^ char) "r__ex__"]
      [(eq? #\& char) "r__am__"]
      [(eq? #\~ char) "r__ti__"]
      [(eq? #\_ char) "r_"]
      [(and (char>=? char #\0) (char<=? char #\9))
       (string-append "r" (list->string `(,char)))]
      [else (list->string `(,char))])))

(define safe 
  (lambda (sym)
    (if (symbol? sym)
     (let loop ([l (string->list (symbol->string sym))])
       (cond
         [(null? l) ""]
         [else (string-append (new-safe-char (car l)) (loop (cdr l)))]))
        sym)))

(define join
  (lambda (lst separater)
    (let loop ([lst lst]
               [result ""]
               [is-first? #t])
      (cond
        [(null? lst) result]
        [is-first? (loop (cdr lst) (format "~a" (car lst)) #f)]
        [else (loop (cdr lst) (string-append result
                                (format "~a~a" separater (car lst))) #f)]))))

(define file->list
  (lambda (fname)
    (let ([file (open-input-file fname)])
      (let ([data
             (let recurse ([decl (read file)])
               (if (eof-object? decl)
		   '()
		   (cons decl (recurse (read file)))))])
        (close-input-port file)
	data))))

(define pc2c
  (lambda (file-name source-name header-name)
    ;; WARNING: pc2c will erase existing files when generating new ones!
    (when (file-exists? source-name) (delete-file source-name))
    (when (file-exists? header-name) (delete-file header-name))
    (init-storage)
    (let ([decl* (file->list file-name)])
      (let ([src (open-output-file source-name)]
            [hdr (open-output-file header-name)])
        (dynamic-wind
            (lambda () #f)
            (lambda ()
              ;; write a generated header file to header-name
              (display (pc2c-header decl*) hdr)
              (check-correct-info)
              ;; write a generated source file source-name
              (display (pc2c-source header-name) src))
          (lambda ()
            (close-output-port src)
            (close-output-port hdr)))))))

(define check-correct-info
  (lambda ()
    (begin
      (if (null? reg-regs)
          (display "Warning: you have defined no registers.\n")
          (void)))))

(define pc2c-append
  (lambda args
    (apply string-append
           (map (lambda (elt)
                  (cond
                    [(symbol? elt) (format "~a" elt)]
                    [(number? elt) (format "~s" elt)]
                    [(string? elt) elt]
                    [else (error 'pc2c-append "Invalid argument ~s" elt)]))
                args))))

(define pc2c-gen-unions
  (lambda (union)
    (let ([name (safe (car union))]
          [tag* (cadr union)]
          [field** (caddr union)])
      (apply string-append
             (map (lambda (tag field*)
                    (let ([tag (safe tag)])
                      (pc2c-append
                        (pc2c-fn-proto (pc2c-append name "r_" tag) field*) " {\n"
                        name "* _data = (" name "*)malloc(sizeof(" name "));\n"
                        "if(!_data) {\n"
                        "  fprintf(stderr, \"Out of memory\\n\");\n"
                        "  exit(1);\n"
                        "}\n"
                        "  _data->tag = _" tag "_" name ";\n"
                        (apply string-append
                               (map (lambda (field)
                                      (let ([field (safe field)])
                                        (format "  _data->u._~a._~a = ~a;\n"
                                                tag field field)))
                                    field*))
                        "  return (void *)_data;\n"
                        "}\n\n")))
                    tag* field**)))))

;; added by wy for constructor argument name binding
;; lookup-arg looks up the argument name of name.tag at position pos
(define lookup-union
  (lambda (name)
    (let loop ([reg reg-unions])
      (cond
       [(null? reg) #f]
       [(eq? name (caar reg)) (car reg)]
       [else (loop (cdr reg))]))))

(define get-arg-list
  (lambda (name tag)
    (let ([u (lookup-union name)])
      (if (not u) (error 'lookup-union 
                           "union type `~a' not defined\n" name)
          (let loop ([tags (cadr u)] [args (caddr u)])
            (cond
             [(null? tags) 
              (error 'lookup-arg
                     "union type `~a' doesn't have a tag `~a'~n" name tag)]
             [(eq? tag (car tags)) (car args)]
             [else (loop (cdr tags) (cdr args))]))))))

(define lookup-arg
  (lambda (name tag pos)
    (list-ref (get-arg-list name tag) pos)))

(define check-union-case
  (lambda (expr name type case)
    (cond
     [(and (null? type) (not (null? case)))
      (let ([s (open-output-string)])
        (pretty-print expr s)
        (error 'union-case  "~a\nsuperfluous cases for union type `~a': ~a"
               (get-output-string s) name case))]
     [(and (null? case) (not (null? type)))
      (let ([s (open-output-string)])
        (pretty-print expr s)
        (error 'union-case  "~a\nunmatched cases for union type `~a': ~a"
               (get-output-string s) name type))]
     [(and (null? type) (null? case)) #t]
     [(not (memq (car case) type))
      (let ([s (open-output-string)])
        (pretty-print expr s)
        (error 'union-case "~a\nvariant `~a' is not in union type `~a'"
               (get-output-string s) (car case) name))]
     [(memq (car case) (cdr case))
      (let ([s (open-output-string)])
        (pretty-print expr s)
        (error 'union-case  "~a\nduplicated cases `~a' in union-case of type `~a'"
               (get-output-string s) (car case) name))]
     [else (check-union-case expr name (remq (car case) type) (cdr case))])))

(define case-env
  (lambda (env var*)
    (let loop ([env env] [var* var*])
      (if (null? var*)
          env
          (extend-env (car var*) (car var*) (loop env (cdr var*)))))))

(define handle-union-case-case
  (lambda (name env)
    (lambda (template body)
      (match template 
        [`(,tag . ,var*) #:when (list? var*)
         (let ([sname (safe name)]
               [stag (safe tag)])
           (let ([given (length var*)] [expected (length (get-arg-list name tag))])
             (if (not (= given expected))
                 (error 'union-case
                        "~a\nwrong number of arguments to constructor `~a' of union-type `~a': expected: ~a, given: ~a"
                        template tag name expected given)
                 (pc2c-append
                  "case _" stag "_" sname ": {\n"
                  (let loop ([var* var*] [n 0])
                    (cond
                     [(null? var*) ""]
                     [else (string-append 
                            (pc2c-append
                             "void *" (safe (car var*)) " = _c->u._" stag
                             "._" (safe (lookup-arg name tag n)) ";\n")
                            (loop (cdr var*) (add1 n)))]))
                  ((parse-function-body #t (case-env env var*)) body)
                  "break; }\n"))))]
        ;; Cannot possibly be effective, commented JBH 12/13
        ;; [else (string-append "default {\n"
        ;;         ((parse-function-body #t (case-env env var*)) body)
        ;;         "}\n")]
        ))))

(define get-last
  (lambda (ls)
    (cond
      ((null? ls) #f)
      ((null? (cdr ls)) (car ls))
      (else (get-last (cdr ls))))))

;; this is for error checking
(define get-body
  (lambda (c)
    (match c
      [`(,test ,body) body])))

(define remove-last
  (lambda (ls)
    (match ls
      [`((else ,body)) '()]
      [`((,test ,body) . ,c*) `((,test ,body) . ,(remove-last c*))])))

(define apply-env
  (lambda (env x)
    (match env
        [`(empty-env) (error 'empty-env "unbound variable: ~s" x)]
        [`(extend-env ,x^ ,a ,env)
         (if (eq? x^ x) a (apply-env env x))])))

(define extend-env
  (lambda (x a env)
    `(extend-env ,x ,a ,env)))

(define empty-env
  (lambda ()
    `(empty-env)))

(define parse-function-body
  (lambda (tail env)
    (if tail
        (lambda (expr)
          (match expr
            [`(error ,name ,msg) 
             (pc2c-append
              "fprintf(stderr, \"" msg "\");\n exit(1);\n")]
            [`(if ,test ,conseq ,alt) 
             (let ((test ((parse-function-body #f env) test))
                   (conseq ((parse-function-body #t env) conseq))
                   (alt ((parse-function-body #t env) alt)))
               (pc2c-append
                "if(" test ") {\n"
                "  " conseq "\n"
                "} else {\n"
                "  " alt "\n"
                "}\n"))]
            [`(cond (else ,body)) 
             (let ((body ((parse-function-body #t env) body)))
               body)]
            [`(cond . ,c*) 
             (let ((last (get-last c*))
                   (c* (remove-last c*)))
               (cond
                 [(eq? (car last) 'else)
                  (let* ((test0 ((parse-function-body #f env) (caar c*)))
                         (body0 ((parse-function-body #t env) (get-body (car c*))))
                         (test* (map (parse-function-body #f env) (map car (cdr c*))))
                         (body* (map (parse-function-body #t env) (map get-body (cdr c*))))
                         (body ((parse-function-body #t env) (cadr last))))
                    (pc2c-append
                     "if(" test0 ") {\n"
                     "  " body0 "\n"
                     "}"
                     (apply string-append
                            (map (lambda (x y)
                                   (pc2c-append " else if(" x ") {\n"
                                                y
                                                "}\n"))
                                 test* body*))
                     " else {\n"
                     "  " body "\n"
                     "}\n"))]
                 [else
                  (let* ((test0 ((parse-function-body #f env) (caar c*)))
                         (body0 ((parse-function-body #t env) (cadar c*)))
                         (test* (map (parse-function-body #f env) (map car (cdr c*))))
                         (body* (map (parse-function-body #t env) (map cadr (cdr c*)))))
                    (pc2c-append
                     "if(" test0 ") {\n"
                     "  " body0 "\n"
                     "}"
                     (apply string-append
                            (map (lambda (x y)
                                   (pc2c-append "else if(" x ") {\n"
                                                y
                                                "}\n"))
                                 test* body*))))]))]
            [`(begin . ,expr*) 
             (apply string-append (map (parse-function-body #t env) expr*))]
            [`(set! ,var ,var1) #:when (eq? var var1) ""]
            [`(set! ,var ,val)
             (let ((val ((parse-function-body #f env) val)))
               (if (equal? (safe var) reg-pc)
                   (pc2c-append (safe var) " = &" val ";\n")
                   (pc2c-append (safe var) " = (void *)" val ";\n")))]
            [`(union-case ,val ,name . ,c*)
             (let ((template* (map car c*))
                   (body* (map get-body c*)))
               (if (not (check-union-case expr name
                                          (cadr (or (lookup-union name)
                                                    (error 'lookup-union 
                                                           "union type `~a' not defined ~n" name)))
                                          (map car template*)))
                   (error 'union-case "union-case doesn't match definition: `~a'\n"
                          name)
                   (let ([sname (safe name)]
                         [val (safe val)])
                     (pc2c-append
                      sname "* _c = (" sname "*)" val ";\n"
                      "switch (_c->tag) {\n"
                      (apply string-append
                             (map (handle-union-case-case name env) template* body*))
                      "}\n"
                      ))))]
            [`(let ,bind* ,body) 
             (let ((lhs* (map car bind*))
                   (rhs* (map (parse-function-body #f env) (map cadr bind*))))
               (pc2c-append
                "{\n"
                (apply string-append
                       (map (lambda (x y)
                              (pc2c-append
                               "void *" (safe x) " = (void *)" y ";\n"))
                            lhs* rhs*))
                body
                "}\n"))]
            [`(printf ,str . ,parms*) 
             (let ([str (list->string
                         (let loop ([str (string->list str)])
                           (if (null? str)
                               '()
                               (if (char=? (car str) #\~)
                                   (if (and (not (null? (cdr str)))
                                            (not (char=? (cadr str) #\ )))
                                       (cons #\% (cons #\d (loop (cddr str))))
                                       (cons #\% (loop (cdr str))))
                                   (cons (car str) (loop (cdr str)))))))]
                   [parms (map safe parms*)])
               (string-append
                "printf(" (join (cons (format "~s" str) parms)
                                ", (int)") ");"))]
            [`(mount-trampoline ,construct ,dismount ,pc) 
             (set! construct-var (safe construct))
             (set! dismount-var (safe dismount))
             (pc2c-append
              "mount_tram();\n")]
            [`(dismount-trampoline ,dismount) 
             (pc2c-append
              "_trstr *trstr = (_trstr *)" (safe dismount) ";\n"
              "longjmp(*trstr->jmpbuf, 1);\n"
              )]
            [`(,func) #:when (is-func? func)
             (pc2c-append reg-pc " = &" (safe func) ";\n")]
            [`,elsee
             (let ((elsee ((parse-function-body #f env) elsee)))
               (pc2c-append "return(" elsee ");\n"))]
            ))
        (lambda (expr)
          (match expr
	    ;; [(error ,name ,msg) 
            ;;  (pc2c-append
            ;;   "fprintf(stderr, \"" msg "\");\n exit(1);\n")]
            [`#t  (pc2c-append "(void *)" 1)]
            [`#f  (pc2c-append "(void *)" 0)]
            [`,x #:when (symbol? x) (safe (apply-env env x))]
            [`,x #:when (integer? x) (pc2c-append "(void *)" x)]
            [`(zero? ,x) 
             (let ((x ((parse-function-body #f env) x)))
               (pc2c-append "(" x " == 0)"))]
            [`(and ,a ,b) 
             (let ((a ((parse-function-body #f env) a))
                   (b ((parse-function-body #f env) b)))
               (pc2c-append "(" a " && " b ")"))]
            [`(or ,a ,b) 
             (let ((a ((parse-function-body #f env) a))
                   (b ((parse-function-body #f env) b)))
               (pc2c-append "(" a " || " b ")"))]
            [`(not ,x) 
             (let ((x ((parse-function-body #f env) x)))
               (pc2c-append "(!" x ")"))]
            [`(< ,a ,b) 
             (let ((a ((parse-function-body #f env) a))
                   (b ((parse-function-body #f env) b)))
               (pc2c-append "(" a " < " b ")"))]
            [`(> ,a ,b) 
             (let ((a ((parse-function-body #f env) a))
                   (b ((parse-function-body #f env) b)))
               (pc2c-append "(" a " > " b ")"))]
            [`(<= ,a ,b) 
             (let ((a ((parse-function-body #f env) a))
                   (b ((parse-function-body #f env) b)))
               (pc2c-append "(" a " <= " b ")"))]
            [`(>= ,a ,b) 
             (let ((a ((parse-function-body #f env) a))
                   (b ((parse-function-body #f env) b)))
               (pc2c-append "(" a " >= " b ")"))]
            [`(+ ,a ,b) 
             (let ((a ((parse-function-body #f env) a))
                   (b ((parse-function-body #f env) b)))
               (pc2c-append "(void *)((int)" a " + (int)" b ")"))]
            [`(* ,a ,b) 
             (let ((a ((parse-function-body #f env) a))
                   (b ((parse-function-body #f env) b)))
               (pc2c-append "(void *)((int)" a " * (int)" b ")"))]
            [`(- ,a ,b) 
             (let ((a ((parse-function-body #f env) a))
                   (b ((parse-function-body #f env) b)))
               (pc2c-append "(void *)((int)" a " - (int)" b ")"))]
            [`(/ ,a ,b) 
             (let ((a ((parse-function-body #f env) a))
                   (b ((parse-function-body #f env) b)))
               (pc2c-append "(void *)((int)" a " / (int)" b ")"))]
            [`(sub1 ,a) 
             (let ((a ((parse-function-body #f env) a)))
               (pc2c-append "(void *)((int)" a " - 1)"))]
            [`(add1 ,a) 
             (let ((a ((parse-function-body #f env) a)))
               (pc2c-append "(void *)((int)" a " + 1)"))]
            [`(random ,x) 
             (let ((x ((parse-function-body #f env) x)))
               (pc2c-append "(void *)(rand() % (int)" x ")"))]
            [`(if ,test ,conseq ,alt) 
             (let ((test ((parse-function-body #f env) test))
                   (conseq ((parse-function-body #f env) conseq))
                   (alt ((parse-function-body #f env) alt)))
               (pc2c-append "(" test " ? " conseq " : " alt ")"))]
            [`(,func . ,args*) #:when (symbol? func)
             (let ((args* (map (parse-function-body #f env) args*)))
               (pc2c-append
                (safe func) "(" (join args* ",") ")"))])))))

(define pc2c-gen-funcs
  (lambda (env)
    (lambda (func)
      (let ([name (safe (car func))]
            [body (cadr func)])
        (pc2c-append
         (if (equal? name "main")
             "int "
             "void ") name "()\n"
         "{\n"
         ((parse-function-body #t env) body)
         "}\n\n"
         )))))

(define global-env
  (lambda ()
    (let loop ([env (empty-env)] 
               [reg (append (map car reg-funcs) reg-regs)])
      (if (null? reg)
          env
          (extend-env (car reg) (car reg) (loop env (cdr reg)))))))

(define pc2c-source
  (lambda (header-name)
    (let* ([s1 (apply string-append (map pc2c-gen-unions reg-unions))]
           [s2 (apply string-append (map (pc2c-gen-funcs (global-env)) reg-funcs))])
      (let ([s3 (pc2c-append
                 "int mount_tram ()\n"
                 "{\n"
                 "srand (time (NULL));\n"
                 "jmp_buf jb;\n"
                 "_trstr trstr;\n"
                 "void *dismount;\n"
                 "int _status = setjmp(jb);\n"
                 "trstr.jmpbuf = &jb;\n"
                 "dismount = &trstr;\n"
                 "if(!_status) {\n"
                 dismount-var "= (void *)" construct-var "(dismount);\n"
                 "for(;;) {\n"
                 reg-pc "();\n"
                 "}\n"
                 "}\n"
                 "return 0;\n"
                 "}\n")])
        (string-append
         "#include <setjmp.h>\n"
         "#include <assert.h>\n"
         "#include <stdlib.h>\n" ;; for malloc
         "#include <stdio.h>\n"
         "#include \"" header-name  "\"\n"
         "\n"
         s1
         s2
         s3)))))

(define pc2c-header
  (lambda (decl*)
    (string-append
      (apply string-append
             (map pc2c-header-parse decl*))
      "int mount_tram();\n\n"
      "struct _trstr;\n"
      "typedef struct _trstr _trstr;\n"
      "struct _trstr {\n"
      "  jmp_buf *jmpbuf;\n"
      "  int value;\n"
      "};\n\n")))

(define pc2c-header-parse
  (lambda (decl)
    (match decl
      [`(load ,file . ,file*)  ""]
      [`(exit)  ""]
      [`(display ,anything . ,anything*)  ""]
      [`(pretty-print ,anything . ,anything*)  ""]
      [`(define-registers . ,reg*) 
       (set! reg-regs reg*)
       (if (null? reg*)
           ""
           (string-append
             "void *"
             (join (map safe reg*) ", *")
             ";\n\n"))]
      [`(define-program-counter ,pc)  (set! reg-pc (safe pc))
       (string-append "void (*" reg-pc ")();\n\n")]
      [`(define-union ,name . ,c*) 
       (let ((tag* (map car c*))
             (field** (map cdr c*)))
         (add-union `(,name ,tag* ,field**))
         (let ([name (safe name)])
           (let ([enum-tags
                  (join
                   (map (lambda (tag)
                          (pc2c-append "_" (safe tag) "_" name)) tag*)
                   ",\n    ")]
                 [structs
                  (apply string-append
                         (map
                          (lambda (tag field*)
                            (let ([tag (safe tag)])
                              (if (null? field*) 
                                  (format
                                   "    struct { char dummy; } _~a;\n" tag)
                                  (string-append
                                   "    struct {"
                                   (apply string-append
                                          (map
                                           (lambda (field)
                                             (format " void *_~a;" (safe field)))
                                           field*))
                                   (format " } _~a;\n" tag)))))
                          tag* field**))]
                 [structors
                  (apply string-append
                         (map
                          (lambda (tag field*)
                            (let ([tag (safe tag)])
                              (string-append
                               (pc2c-fn-proto (pc2c-append name "r_" tag)
                                              field*)
                               ";\n")))
                          tag* field**))])
             (pc2c-append
              "struct " name ";\n"
              "typedef struct " name " " name ";\n"
              "struct " name " {\n"
              "  enum {\n"
              "    " enum-tags "\n"
              "  } tag;\n"
              "  union {\n"
              structs
              "  } u;\n"
              "};\n\n"
              structors "\n"))))]
      [`(define-label ,name ,body) 
       (begin (add-func `(,name ,body))
              (string-append (if (equal? (safe name) "main")
                                 "int "
                                 "void ") (safe name) "();\n"))])))

(define pc2c-fn-proto
  (lambda (fn-name param*)
    (let ([declare-params
           (lambda (param*)
             (join (map (lambda (param)
                          (format "void *~a" (safe param))) param*) ", "))])
      (pc2c-append
        "void *" (safe fn-name) "(" (declare-params param*)  ")"))))

(define compile/run
  (lambda (base-name)
    (let ([pc-file (string-append base-name ".pc")]
                   [c-file (string-append base-name ".c")]
                   [header-file (string-append base-name ".h")])
      (pc2c pc-file c-file header-file)
      (let ([compile-command (string-append "gcc -o " base-name " " c-file)]
            [indent-command (string-append "indent -bad -bap -br -nbs -ce"
                                           "-cli1 -di1 -i2 -npro -npsl ")])
        (let ([compile-val (system compile-command)])
          (unless (eqv? compile-val #t)
            (error 'compile/run "Compilation command '~s' returned ~s"
                   compile-command compile-val))
          (system (string-append indent-command c-file))
          (system (string-append indent-command header-file))
          (system (string-append "./" base-name))
          (void))))))

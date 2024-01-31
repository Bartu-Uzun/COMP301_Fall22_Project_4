(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))

        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### value-of cases for new expressions, remember
        ; ###### that you need to use memory functionalities. 
        ; #####################################################

        ;new-vector-exp : ExpVal x ExpVal -> VecVal
        (new-vector-exp (exp1 exp2)
          (let ((len (expval->num (value-of exp1 env)))
                (value (value-of exp2 env)))
            (vec-val (a-vector len (new-vector-helper value len)))))

        ; update-vector-exp : VecVal x ExpVal -> Unspecified
        (update-vector-exp (exp1 exp2 exp3)
          (let ((vect (expval->vec (value-of exp1 env)))
                (index (expval->num (value-of exp2 env)))
                (val (value-of exp3 env)))
            (begin
              
              (update-vector-helper vect index val)              
              (num-val 24))))
                

        ; read-vector-exp : VecVal x ExpVal -> ExpVal
        (read-vector-exp (exp1 exp2)
          (let ((vect (expval->vec (value-of exp1 env)))
                (index (expval->num (value-of exp2 env))))
            (begin
              (read-vector-helper vect index))))

        ; length-vector-exp : VecVal -> ExpVal
        (length-vector-exp (exp1)
          (let ((vect (expval->vec (value-of exp1 env))))
            (cases vec vect
              (a-vector (length first)
                (num-val length))
              (else 'error))))

        ; swap-vector-exp : VecVal x ExpVal x ExpVal -> Unspecified
        (swap-vector-exp (exp1 exp2 exp3)
          (let ((vect (expval->vec (value-of exp1 env)))
                (ind1 (expval->num (value-of exp2 env)))
                (ind2 (expval->num (value-of exp3 env))))
            (begin
              (swap-vector-helper vect ind1 ind2)
              (num-val 24))))

        ; copy-vector-exp : VecVal -> VecVal
        (copy-vector-exp (exp1)
          (let ((vect (expval->vec (value-of exp1 env))))
            (copy-vector-helper vect)))

        ;; STACK RELATED
        
        ; new-stack-exp : ExpVal -> VecVal
        ; stack is a vector that's first element holds the total number of elements it has
        (new-stack-exp (exp1)
          (let ((length (+ 1 (expval->num (value-of exp1 env)))))
            (value-of (new-vector-exp (const-exp length) (const-exp 0)) env)))

        ;;push-stack-exp : VecVal x ExpVal -> unspecified
        (push-stack-exp (exp1 exp2)
           (let ((vect-stack (expval->vec (value-of exp1 env)))
                 (val (value-of exp2 env)))
             (cases vec vect-stack
               (a-vector (length first)
                 (let ((current-size (expval->num (deref first))))
                   (if (>= current-size (- length 1)) ;if the stack is full
                       'stack_overflow_error
                       (begin
                         (setref! (+ first current-size 1) val)
                         (setref! first (num-val (+ current-size 1)))
                         (num-val 24)))))
            
               (else 'error))))

        ;;pop-stack-exp : VecVal -> unspecified
        (pop-stack-exp (exp1)
          (let ((vect-stack (expval->vec (value-of exp1 env))))
            (cases vec vect-stack
              (a-vector (length first)
                (let ((current-size (expval->num (deref first))))
                  (if (= current-size 0) ;if stack is empty
                      'stack_underflow_error
                      (begin
                        (setref! first (num-val (- current-size 1)))
                        (num-val 24)))))
              (else 'error))))

        ;;stack-size-exp : VecVal -> ExpVal
        (stack-size-exp (exp1)
           (let ((vect-stack (expval->vec (value-of exp1 env))))
             (cases vec vect-stack
               (a-vector (length first)
                  (deref first))
               (else 'error))))

        ;;peek-stack-exp : VecVal -> ExpVal
        (peek-stack-exp (exp1)
          (let ((vect-stack (expval->vec (value-of exp1 env))))
             (cases vec vect-stack
               (a-vector (length first)
                  (let ((current-size (expval->num (deref first))))
                    (if (= current-size 0)
                        'stack_empty
                        (deref (+ first current-size)))))
               (else 'error))))

        

        ;;print-stack-exp : VecVal -> unspecified
        (print-stack-exp (exp1)
          (let ((vect-stack (expval->vec (value-of exp1 env))))
            (cases vec vect-stack
              (a-vector (length first)
                (let ((current-size (expval->num (deref first))))
                  (print-stack first current-size)))
              (else 'error))))
                     

        ;;empty-stack?-exp : VecVal -> ExpVal
        (empty-stack?-exp (exp1)
           (let ((vect-stack (expval->vec (value-of exp1 env))))
            (cases vec vect-stack
              (a-vector (length first)
                 (let ((current-size (expval->num (deref first))))
                   (if (= current-size 0) (bool-val #t) (bool-val #f))))
              (else 'error))))

        ;(initialize-store!)
        ;(value-of (let-exp 'x (new-vector-exp (const-exp 4) (const-exp 2)) (update-vector-exp (var-exp 'x) (const-exp 1) (const-exp 9))) (init-env))
      ;(value-of (let-exp 'x (new-vector-exp (const-exp 2) (const-exp 13)) (read-vector-exp (var-exp 'x) (const-exp 1))) (init-env))
      ;(value-of (let-exp 'z (new-vector-exp (const-exp 4) (const-exp 2)) (begin-exp (update-vector-exp (var-exp 'z) (const-exp 1) (const-exp 33)) (list (swap-vector-exp (var-exp 'z) (const-exp 1) (const-exp 2))))) (init-env))
      ;(get-store-as-list)
      
        ; #####################################################
        )))

  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE

  ; for faster debugging-------
  (define (x) (initialize-store!))
  (define (lst) (get-store-as-list))
  ; for faster debugging-------

  (define new-vector-helper
    (lambda (value len)
      (car (create-new-vector value len))))
        

  (define create-new-vector
    (lambda (value len)
      (if (= len 1)
          (newref value)
          (list (newref value) (create-new-vector value (- len 1))))))



  (define update-vector-helper
    (lambda (vect index val)
      (cases vec vect
        (a-vector (length first)
          (if (or (>= index length) (< index 0))
              'index_out_of_bounds_error
              (setref! (+ first index) val)))
                  
        (else 'error))))
  
  (define read-vector-helper
   (lambda (vect index)
     (cases vec vect
       (a-vector (length first)
         (if (or (>= index length) (< index 0))
         'index_out_of_bounds_error
         (deref (+ first index))))
       (else 'error))))

  (define swap-vector-helper
    (lambda (vect ind1 ind2)
      (cases vec vect
        (a-vector (length first)
                  (if (or (>= ind1 length) (>= ind2 length) (< ind1 0) (< ind2 0))
                      'index_out_of_bounds_error
                      (let ((val1 (deref (+ first ind1)))
                            (val2 (deref (+ first ind2))))
                        (begin
                          
                          (setref! (+ first ind1) val2)
                          (setref! (+ first ind2) val1)))))
        (else 'error))))

  (define copy-vector-helper
    (lambda (vect)
      (cases vec vect
        (a-vector (length first)
                  (begin
                    (let ((copy-vect (deep-copy-vector length first 0)))
                      (vec-val (a-vector
                       length
                       (car copy-vect))))))
        (else 'error))))

  (define deep-copy-vector
    (lambda (length first counter)
      (if (= counter (- length 1))
          (newref (deref (+ counter first)))
          (list (newref (deref (+ counter first))) (deep-copy-vector length first (+ counter 1))))))

  
  (define print-stack
    (lambda (first size)
      (if (> size 1)
          (begin
            (display (expval->num (deref (+ first size))))
            (display " ")
            (print-stack first (- size 1)))
          (display (expval->num (deref (+ first size)))))))
          
          
          
          
             
    


     
      
        
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal 
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 
  )
  


  

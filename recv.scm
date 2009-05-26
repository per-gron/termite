(import (only: match match/action)
        exception)

;; All hail the RECV form
(define-syntax recv
  (sc-macro-transformer
   (lambda (form env)
     (let ((clauses (cdr form)))
       
       ;; check the last clause to see if it's a timeout
       (let ((sesualc (reverse clauses)))
         (if (and (pair? (car sesualc))
                  (eq? (caar sesualc) 'after))
             
             (let ((clauses (map (lambda (x)
                                   (make-syntactic-closure env '() x))
                                 (reverse (cdr sesualc))))
                   ;; the code to compute the timeout
                   (init (make-syntactic-closure
                          env
                          '()
                          (cadar sesualc)))
                   ;; the code to be executed on a timeout
                   (on-timeout (map (lambda (x)
                                      (make-syntactic-closure env '() x))
                                    (cddar sesualc))))
               
               ;; RECV code when there is a timeout
               `(let ((timeout ,init))
                  (with-exception-catcher
                   (lambda (e)
                     (if (mailbox-receive-timeout-exception? e)
                         (begin
                           (thread-mailbox-rewind)
                           ,@on-timeout)
                         (raise e)))
                   (lambda ()
                     (let loop ((msg (thread-mailbox-next timeout)))
                          (match/action
                           (thread-mailbox-extract-and-rewind)
                           (loop 
                            (thread-mailbox-next timeout))
                           msg
                           ;; extra clause to handle system events
                           (event 
                            (where (termite-exception? event))
                            (handle-exception-message event))
                           ;; the user clauses
                           ,@clauses))))))
             
             ;; RECV code when there is no timeout
             `(let loop ((msg (thread-mailbox-next)))
                (match/action
                 (thread-mailbox-extract-and-rewind)
                 (loop
                  (thread-mailbox-next))
                 msg
                 ;; extra clause to handle system events
                 (event 
                  (where (termite-exception? event))
                  (handle-exception-message event))
                 ;; the user clauses
                 ,@(map (lambda (x)
                          (make-syntactic-closure env '() x))
                        clauses)))))))))

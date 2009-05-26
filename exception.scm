
;; NOTE It might be better to integrate with Gambit's exception mechanism
(define-type termite-exception
			 id: 6a3a285f-02c4-49ac-b00a-aa57b1ad02cf
			 origin
			 reason
			 object)


;; Default callback for received exceptions.
(define (handle-exception-message event)
  (raise event))

;; utils

;; ----------------------------------------------------------------------------
;; Some basic utilities

(define (find pred? lst)
  (let loop ((lst lst))
    (and (not (null? lst))
         (if (pred? (car lst))
             (car lst)
             (loop (cdr lst))))))

(define (assoc-pred x lst pred?)
  (find (lambda (entry)
          (pred? x (car entry)))
        lst))

(define (filter pred? lst)
  (cond
   ((null? lst) '())
   
   ((pred? (car lst))
    (cons (car lst)
          (filter pred? (cdr lst))))
   (else
    (filter pred? (cdr lst)))))

(define (remove pred? lst)
  (filter (lambda (x)
            (not (pred? x)))
          lst))

; touring machine implementation in Scheme R5RS
; - pure functional
; - state transition specifications are executable scheme programs
; - the tape consists of current, left and right lists
; - left, right and write operations on the tape are O(1)
; - the length of the tape is bound by the number of cons cells
; - recursive calls are in tail position

; move one slot to the left
(define (left tape)
  (let ((current (car tape))
        (left (cadr tape))
        (right (caddr tape)))
    (list (if (null? left) 0 (car left))
          (if (null? left) '() (cdr left))
          (cons current right))))

; move one slot to the right
(define (right tape)
  (let ((current (car tape))
        (left (cadr tape))
        (right (caddr tape)))
    (list (if (null? right) 0 (car right))
          (cons current left)
          (if (null? right) '() (cdr right)))))

; write the value
(define (write tape value)
  (cons value (cdr tape)))

; count the ones
(define (checksum tape)
  (+ (car tape)
     (apply + (cadr tape))
     (apply + (caddr tape))))

(define (print tape)
  (display (reverse (cadr tape)))
  (display " [")
  (display (car tape))
  (display "] ")
  (display (caddr tape))
  (newline))

(define (run-steps state-function tape steps)
  (if (zero? steps)
      (cons state-function tape)
      (let ((next (state-function tape)))
          (run-steps (car next) (cdr next) (- steps 1)))))

(define touring-machine-1
  (let ()
    (define (A tape)
      (case (car tape)
        ((0) (cons B (right (write tape 1))))
        ((1) (cons B (left (write tape 0))))))
    (define (B tape)
      (case (car tape)
        ((0) (cons A (left (write tape 1))))
        ((1) (cons A (right (write tape 1))))))
    A))

(define touring-machine-2
  (let ()
    (define (A tape)
      (case (car tape)
        ((0) (cons B (right (write tape 1))))
        ((1) (cons E (left (write tape 0))))))
    (define (B tape)
      (case (car tape)
        ((0) (cons C (left (write tape 1))))
        ((1) (cons A (right (write tape 0))))))
    (define (C tape)
      (case (car tape)
        ((0) (cons D (left (write tape 1))))
        ((1) (cons C (right (write tape 0))))))
    (define (D tape)
      (case (car tape)
        ((0) (cons E (left (write tape 1))))
        ((1) (cons F (left (write tape 0))))))
    (define (E tape)
      (case (car tape)
        ((0) (cons A (left (write tape 1))))
        ((1) (cons C (left (write tape 1))))))
    (define (F tape)
      (case (car tape)
        ((0) (cons E (left (write tape 1))))
        ((1) (cons A (right (write tape 1))))))
    A))

(define initial-tape (list 0 '() '()))
(print (cdr (run-steps touring-machine-1 initial-tape 0)))
(print (cdr (run-steps touring-machine-1 initial-tape 1)))
(print (cdr (run-steps touring-machine-1 initial-tape 2)))
(print (cdr (run-steps touring-machine-1 initial-tape 3)))
(print (cdr (run-steps touring-machine-1 initial-tape 4)))
(print (cdr (run-steps touring-machine-1 initial-tape 5)))
(print (cdr (run-steps touring-machine-1 initial-tape 6)))
(display (checksum (cdr (run-steps touring-machine-1 initial-tape 6))))
(newline)
(display (checksum (cdr (run-steps touring-machine-2 initial-tape 12208951))))
(newline)

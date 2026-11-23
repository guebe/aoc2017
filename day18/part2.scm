(define first car)
(define second cadr)

; special registers
(define PC 'x) ; program counter

; registers are stored in a vector - this function provides the mapping
(define (reg->idx sym) (- (char->integer (string-ref (symbol->string sym) 0)) 97))

(define (cpu-instr prog state recv debug)
  (define send '())
  (let* (; register setters and getters
         (reg (lambda (x) (vector-ref state (reg->idx x))))
         (val (lambda (x) (if (number? x) x (reg x))))
         (put! (lambda (x v) (vector-set! state (reg->idx x) v)))
         (advance-n! (lambda (n) (put! PC (+ (reg PC) n))))
         (advance! (lambda () (advance-n! 1)))

         ; instruction implementations
         (snd (lambda (x)
		(set! send (cons (val (first x)) send))
		(advance!)))
         (set (lambda (x)
		(put! (first x) (val (second x)))
		(advance!)))
         (add (lambda (x)
		(put! (first x) (+ (reg (first x)) (val (second x))))
		(advance!)))
         (mul (lambda (x)
		(put! (first x) (* (reg (first x)) (val (second x))))
		(advance!)))
         (mod (lambda (x)
		(put! (first x) (modulo (reg (first x)) (val (second x))))
		(advance!)))
         (rcv (lambda (x)
		(cond ((not (null? recv))
		       (begin
			 (put! (first x) (car recv))
			 (set! recv (cdr recv))
			 (advance!)))
		      (else (reverse send)))))
         (jgz (lambda (x)
		(advance-n! (if (> (val (first x)) 0)
			        (val (second x))
				1))))

         (debug-print (lambda ()
                        (let ((pos (reg PC)))
                          (display pos)
                          (display " ")
                          (write (vector-ref prog pos))
                          (display " ")
                          (write state)
                          (newline))))

         ; exec one instruction
         (exec (lambda (instr)
		 (let ((args (cdr instr)))
		   (case (car instr)
		     ((snd) (snd args))
		     ((set) (set args))
		     ((add) (add args))
		     ((mul) (mul args))
		     ((mod) (mod args))
		     ((rcv) (rcv args))
		     ((jgz) (jgz args)))))))

    ; execute program
    (let loop ()
      (let ((pos (reg PC)))
        (and (>= pos 0)
             (< pos (vector-length prog))
             (begin
               (if debug (debug-print))
	       (let ((result (exec (vector-ref prog pos))))
		 (if (list? result)
		     result
		     (loop)))))))))

;(include "example_part2.scm")
(include "input.scm")
(let ((state0 (make-vector 26 0))
      (state1 (make-vector 26 0))
      (recv0 '())
      (recv1 '())
      (cnt 0))
  (vector-set! state1 (reg->idx 'p) 1)
  (let loop ()
      (set! recv1 (cpu-instr program state0 recv0 #f))
      (set! recv0 (cpu-instr program state1 recv1 #f))
      (set! cnt (+ cnt (length recv0)))
      (if (not (and (null? recv1) (null? recv0))) (loop)))
  (display cnt)
  (newline))

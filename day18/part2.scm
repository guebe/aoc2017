(define first car)
(define second cadr)

; program counter and last-played-frequency are handled like registers
; unique register names are used
(define PC 'x)

; registers are stored in a vector - this function provides the mapping
(define (reg->idx sym) (- (char->integer (string-ref (symbol->string sym) 0)) 97))

(define (cpu-instr prog state send recv debug)
  (let* (; register setters and getters
         (reg (lambda (x) (vector-ref state (reg->idx x))))
         (val (lambda (x) (if (number? x) x (reg x))))
         (put! (lambda (x v) (vector-set! state (reg->idx x) v)))

         ; instruction implementations
         (snd (lambda (x) (set! send (append send (list (val (first x))))) #f))
         (set (lambda (x) (put! (first x) (val (second x))) #f))
         (add (lambda (x) (put! (first x) (+ (reg (first x)) (val (second x)))) #f))
         (mul (lambda (x) (put! (first x) (* (reg (first x)) (val (second x)))) #f))
         (mod (lambda (x) (put! (first x) (modulo (reg (first x)) (val (second x)))) #f))
         (rcv (lambda (x) (cond ((not (null? recv)) 
				  (begin 
				    (put! (first x) (car recv))
				    (set! recv (cdr recv))
				    #f))
				(else send))))
         (jgz (lambda (x) (put! PC (+ (reg PC) (if (> (val (first x)) 0) (val (second x)) 1))) #f))

         ; instruction list
         (insns
           (list (cons 'snd snd)
                 (cons 'set set)
                 (cons 'add add)
                 (cons 'mul mul)
                 (cons 'mod mod)
                 (cons 'rcv rcv)
                 (cons 'jgz jgz)))

         (debug-print (lambda ()
                        (let ((pos (reg PC)))
                          (display pos)
                          (display " ")
                          (write (vector-ref prog pos))
                          (display " ")
                          (write state)
                          (newline))))

         ; exec one instruction and maintain PC
         (exec (lambda (instr)
                 (unless (eq? (car instr) 'jgz) (put! PC (+ (reg PC) 1)))
                 ((cdr (assoc (car instr) insns)) (cdr instr)))))

    ; execute program
    (let loop ()
      (let ((pos (reg PC)))
        (and (>= pos 0)
             (< pos (vector-length prog))
             (begin
	       (if debug (debug-print))
               (or (exec (vector-ref prog pos))
                   (loop))))))))

;(include "example_part2.scm")
(include "input.scm")
(let ((state0 (make-vector 26 0))
      (state1 (make-vector 26 0))
      (recv0 '())
      (recv1 '())
      (cnt 0))
  (vector-set! state1 (reg->idx 'p) 1)
  (set! recv1 (cpu-instr program state0 '() recv0 #f))
  (display recv1)
  (newline)
  (set! recv0 (cpu-instr program state1 '() recv1 #f))
  (display recv0)
  (set! cnt (+ cnt (length recv0)))
  (newline)
  (let loop ()
      (vector-set! state0 (reg->idx PC) (- (vector-ref state0 (reg->idx PC)) 1))
      (set! recv1 (cpu-instr program state0 '() recv0 #f))
      (display recv1)
      (newline)
      (vector-set! state1 (reg->idx PC) (- (vector-ref state1 (reg->idx PC)) 1))
      (set! recv0 (cpu-instr program state1 '() recv1 #f))
      (display recv0)
      (set! cnt (+ cnt (length recv0)))
      (newline)
      (if (and (null? recv1) (null? recv0)) #f (loop)))
  (display cnt)
  (newline))

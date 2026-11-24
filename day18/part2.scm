(define first car)
(define second cadr)

; special registers
(define PC 'x) ; program counter

; registers are stored in a vector - this function provides the mapping
(define (reg->idx sym) (- (char->integer (string-ref (symbol->string sym) 0)) (char->integer #\a)))

(define (cpu-instr prog state recv debug)
  (define send '())
  (let* (; register setters and getters
         (reg (lambda (x) (vector-ref state (reg->idx x))))
         (val (lambda (x) (if (number? x) x (reg x))))
         (put! (lambda (x v) (vector-set! state (reg->idx x) v)))
         (advance-n! (lambda (n) (put! PC (+ (reg PC) n))
                                 (if (and (>= (reg PC) 0)
                                          (< (reg PC) (vector-length prog)))
                                     'running
                                     'terminated)))
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
                (if (null? recv)
                    'blocked
                    (begin
                      (put! (first x) (car recv))
                      (set! recv (cdr recv))
                      (advance!)))))
         (jgz (lambda (x)
                (advance-n! (if (> (val (first x)) 0)
                                (val (second x))
                                1))))

         (debug-print (lambda ()
                        (if debug
                            (let ((pos (reg PC)))
                              (display pos)
                              (display " ")
                              (write (vector-ref prog pos))
                              (display " ")
                              (write state)
                              (newline)))))

         ; exec one instruction
         (exec (lambda (instr)
                 (case (car instr)
                   ((snd) (snd (cdr instr)))
                   ((set) (set (cdr instr)))
                   ((add) (add (cdr instr)))
                   ((mul) (mul (cdr instr)))
                   ((mod) (mod (cdr instr)))
                   ((rcv) (rcv (cdr instr)))
                   ((jgz) (jgz (cdr instr)))))))

    ; execute program
    (let loop ()
      (begin
        (debug-print)
        (case (exec (vector-ref prog (reg PC)))
          ((blocked) (reverse send))
          ((terminated) 'terminated)
          ((running) (loop)))))))

;(include "example_part2.scm")
(include "input.scm")
(let ((state0 (make-vector 26 0))
      (state1 (make-vector 26 0)))
  (vector-set! state1 (reg->idx 'p) 1)
  (let loop ((queue '()) (cnt 0))
    (let ((queue (cpu-instr program state1 (cpu-instr program state0 queue #f) #f)))
      (if (not (null? queue))
          (loop queue (+ cnt (length queue)))
          (begin
            (display cnt)
            (newline))))))


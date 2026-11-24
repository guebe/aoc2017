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

         (debug-print (lambda ()
                        (if debug
                            (let ((pos (reg PC)))
                              (display pos) (display " ")
                              (write (vector-ref prog pos))
                              (display " ") (write state)
                              (newline)))))

         ; instruction implementations
         (exec (lambda (instr)
                 (let ((r (cadr instr))
                       (v (if (null? (cddr instr)) '() (val (caddr instr)))))
                   (case (car instr)
                     ((snd) (set! send (cons (val r) send))
                            (advance!))
                     ((set) (put! r v)
                            (advance!))
                     ((add) (put! r (+ (reg r) v))
                            (advance!))
                     ((mul) (put! r (* (reg r) v))
                            (advance!))
                     ((mod) (put! r (modulo (reg r) v))
                            (advance!))
                     ((rcv) (if (null? recv)
                                'blocked
                                (begin
                                  (put! r (car recv))
                                  (set! recv (cdr recv))
                                  (advance!))))
                     ((jgz) (advance-n! (if (> (val r) 0) v 1))))))))

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


(define (reg-ref regs sym)
  (let ((entry (assq sym regs)))
    (if entry (cdr entry) 'invalid-reg)))

(define (reg-set regs sym value)
  (cons (cons sym value) regs))

(define (operand-value regs operand)
  (if (number? operand) operand (reg-ref regs operand)))

(define (exec instruction pc regs in out)
  (let ((r (cadr instruction))
        (v (if (null? (cddr instruction)) '() (operand-value regs (caddr instruction)))))
    (case (car instruction)
      ((snd) (list (+ pc 1) regs in (cons (operand-value regs r) out)))
      ((set) (list (+ pc 1) (reg-set regs r v) in out))
      ((add) (list (+ pc 1) (reg-set regs r (+ (reg-ref regs r) v)) in out))
      ((mul) (list (+ pc 1) (reg-set regs r (* (reg-ref regs r) v)) in out))
      ((mod) (list (+ pc 1) (reg-set regs r (modulo (reg-ref regs r) v)) in out))
      ((rcv) (if (null? in)
                 (list pc regs in out)
		 (list (+ pc 1) (reg-set regs r (car in)) (cdr in) out)))
      ((jgz) (list (+ pc (if (> (operand-value regs r) 0) v 1)) regs in out)))))

(define (debug-print pc instruction machine)
  (begin (display pc) (display " ") (write instruction)
	 (display " ") (write machine) (newline)))

(define (run program machine)
  (let* ((pc (car machine))
	 (instruction (vector-ref program pc))
         (result (apply exec instruction machine))
         (new-pc (car result)))
      (if #f (debug-print pc instruction result))
      (if (or (= pc new-pc) (< new-pc 0) (>= new-pc (vector-length program)))
	  result
	  (run program result))))

(define (deliver m0 m1)
  (list (car m0) (cadr m0) (cadddr m1) (cadddr m0)))

(define (schedule machine0 machine1 cnt)
  (let* ((result0 (run program (deliver machine0 machine1)))
	 (result1 (run program (deliver machine1 result0))))
      (if (null? (cadddr result1))
	  cnt
          (schedule (list result0 result1 (+ cnt (length (cadddr result1))))))))

;(include "example_part2.scm")
(include "input.scm")
(display (schedule (machine0 (list 0 '((p . 0)) '() '()))
	           (machine1 (list 0 '((p . 1)) '() '()))
	           0))
(newline)

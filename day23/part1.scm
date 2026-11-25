(define (reg-ref regs sym)
  (let ((entry (assq sym regs)))
    (if entry (cdr entry) 0)))

(define (reg-remove regs sym)
  (cond ((null? regs) '())
        ((eq? (caar regs) sym) (cdr regs))
        (else (cons (car regs) (reg-remove (cdr regs) sym)))))

(define (reg-set regs sym value)
  (cons (cons sym value) (reg-remove regs sym)))

(define (operand-value regs operand)
  (if (number? operand) operand (reg-ref regs operand)))

(define (exec instruction pc regs cnt)
  (let ((X (cadr instruction))
        (Y (if (null? (cddr instruction)) '() (operand-value regs (caddr instruction)))))
    (case (car instruction)
      ((set) (list (+ pc 1) (reg-set regs X Y) cnt))
      ((sub) (list (+ pc 1) (reg-set regs X (- (reg-ref regs X) Y)) cnt))
      ((mul) (list (+ pc 1) (reg-set regs X (* (reg-ref regs X) Y)) (+ cnt 1)))
      ((jnz) (list (+ pc (if (zero? (operand-value regs X)) 1 Y)) regs cnt))
      ((jgz) (list (+ pc (if (> (operand-value regs X) 0) 1 Y)) regs cnt)))))

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

(include "input.scm")
(display (caddr (run program (list 0 '() 0))))
;(display (reg-ref (cadr (run program (list 0 (list (cons 'a 1)) 0))) 'h))
(newline)

; R5RS compatible DSL for aoc 2017 day 8
; macro-only implementation
(define-syntax == (syntax-rules () ((_ x y) (= x y))))
(define-syntax != (syntax-rules () ((_ x y) (not (= x y)))))
(define-syntax inc (syntax-rules () ((_ x y) (+ x y))))
(define-syntax dec (syntax-rules () ((_ x y) (- x y))))

(define-syntax run-instruction
  (syntax-rules (if)
    ((_ (register op value if lhs cmp rhs) peak)
     (if (cmp lhs rhs)
         (begin
	   (set! register (op register value))
	   (set! peak (max peak register)))))))

(define-syntax run-program
  (syntax-rules ()
    ((_ (register ...)
        instruction ...)
     (let ((register 0) ...
	   (peak 0))
       (run-instruction instruction peak) ...
       (display (max register ...))
       (newline)
       (display peak)
       (newline)))))

(run-program
  (a b c)
  (b inc 5 if a > 1)
  (a inc 1 if b < 5)
  (c dec -10 if a >= 1)
  (c inc -20 if c == 10))

(include "input.scm")

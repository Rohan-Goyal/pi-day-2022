(require 'litex-mode)
(cl-defmacro series (expr &optional (var i) (lower 0) (upper n) (op ''+)) "Given a symbolic expression in terms of a variable like n or i, return a func which returns the long form as a symbolic expression"
             `(lambda (lnum unum)
                (cons ,op (-map (lambda (,var) ,expr) (number-sequence lnum unum)))
                )
             )

(defun odds (n)
  (-take n (-filter (lambda (x) (not (math-evenp x))) (number-sequence 0 (* 2 n))))
  )
(defun square (n)
  (expt n 2))
(defun sqrt (n)
  (expt n 0.5))

(cl-defun fib (n &optional (a 0) (b 1))
  (if (<= n 1)
      b
    (fib (- n 1) b (+ a b)))
  )
(setq LIM 50)

(let* (
       (riemann (series `(/ ,(float 1) (square ,i)) i 1 n))
       (wrapped `(* (sqrt 6) (sqrt ,(funcall riemann 1 LIM))))
       (tex (s-wrap (litex-lisp2latex-all wrapped ) "$" "$"))
       (evalled (eval wrapped))
       )
  (list tex evalled))

(let* (
       (wallis (series `(* (/ ,(* 2.0 i) ,(- (* 2.0 i) 1)) (/ ,(* 2.0 i) ,(+ (* 2.0 i) 1))) i 1 n '*) )
       (wrapped `(* 2 ,(funcall wallis 1 LIM)))
       (tex (s-wrap (litex-lisp2latex-all wrapped) "$" "$"))
       (evalled (eval wrapped))
       )
  (list tex evalled)
  )

(cl-defun brouncker (n &optional (i 0))
  (if (>= i n) n
    `(/ ,(square (float (+ (* 2 i) 1))) (+ 2 ,(brouncker n (+ 1 i))))))
(let* (
       (br (brouncker 5))
       (wrapped `(/ 4 (+ 1 ,br)))
       (tex (s-wrap (litex-lisp2latex-all wrapped ) "$" "$"))
       (evalled (eval wrapped))
       )
  (list tex evalled))

(let* (
       (leibniz (series `(,(if (math-evenp i) '+ '-) (/ ,(float 4) ,(+ (* 2 i) 1))) i 0 n))
       (wrapped (funcall leibniz 0 LIM))
       (tex (s-wrap (litex-lisp2latex-all wrapped ) "$" "$"))
       (evalled (eval wrapped))
       )
  (list tex evalled))

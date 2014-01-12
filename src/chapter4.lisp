(in-package :cl-user)
(defpackage the-little-schemer-cl.chapter4
  (:use :cl)
  (:export :pick
           :rempick
           :eqan?))
(in-package :the-little-schemer-cl.chapter4)

(defun o+ (n m)
  (cond
   ((zerop m) n)
   (t (1+ (o+ n (1- m))))))

(defun o- (n m)
  (cond
   ((zerop m) n)
   (t (1- (o- n (1- m))))))

;;; The First Commandment (first revision)
;;; When recurring on a list of atoms, lat, ask two questions about it:
;;; (null lat) and t.
;;; When recurring on a number, n, ask two questions about it:
;;; (zerop n) and t.

(defun addtup (tup)
  (cond
    ((null tup) 0)
    (t (o+ (car tup)
           (addtup (cdr tup))))))

;;; The Fourth Commandment (first revision)
;;; Always change at least one argument while recurring. It must be changed
;;; to be closer to termination. The changing argument must be tested in the
;;; termination condition:
;;; when using cdr, test termination with null and
;;; when using 1-, test termination with zerop.

(defun o* (n m)
  (cond
    ((zerop m) 0)
    (t (o+ n
           (o* n (1- m))))))

;;; The Fifth Commandment
;;; When building a value with +, always use 0 for the value of the terminating
;;; line, for adding 0 does not change the value of an addition.
;;; When building a value with *, always use 1 for the value of the terminating
;;; line, for multiplying by 1 does not change the value of a multiplication.
;;; When building a value with cons, always consider () for the value of the
;;; terminating line.

;; (defun tup+ (tup1 tup2)
;;   (cond
;;     ((and (null tup1) (null tup2)) '())
;;     (t (cons (o+ (car tup1) (car tup2))
;;              (tup+ (cdr tup1) (cdr tup2))))))

(defun tup+ (tup1 tup2)
  (cond
    ((null tup1) tup2)
    ((null tup2) tup1)
    (t (cons (o+ (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2))))))

(defun o> (n m)
  (cond
    ((zerop n) nil)
    ((zerop m) t)
    (t (o> (1- n) (1- m)))))

(defun o< (n m)
  (cond
    ((zerop m) nil)
    ((zerop n) t)
    (t (o< (1- n) (1- m)))))

(defun o= (n m)
  (cond
    ((o> n m) nil)
    ((o< n m) nil)
    (t t)))

(defun my-expt (n m)
  (cond
    ((zerop m) 1)
    (t (o* n (my-expt n (1- m))))))

(defun quotient (n m)
  (cond
    ((o< n m) 0)
    (t (1+ (quotient (o- n m) m)))))

(defun my-length (lat)
  (cond
    ((null lat) 0)
    (t (1+ (length (cdr lat))))))

(defun pick (n lat)
  (cond
    ((zerop (1- n)) (car lat))
    (t (pick (1- n) (cdr lat)))))

;; (defun rempick (n lat)
;;   (cond
;;     ((zerop (1- n)) (cdr lat))
;;     (t (cons (car lat)
;;              (rempick (1- n) (cdr lat))))))

(defun no-nums (lat)
  (cond
    ((null lat) '())
    (t (cond
         ((numberp (car lat)) (no-nums (cdr lat)))
         (t (cons (car lat)
                  (no-nums (cdr lat))))))))

(defun all-nums (lat)
  (cond
    ((null lat) '())
    (t (cond
         ((numberp (car lat)) (cons (car lat)
                                    (all-nums (cdr lat))))
         (t (all-nums (cdr lat)))))))

(defun eqan? (a1 a2)
  (cond
    ((and (numberp a1) (numberp a2)) (= a1 a2))
    ((or (numberp a1) (numberp a2)) nil)
    (t (eq a1 a2))))

(defun occur (a lat)
  (cond
    ((null lat) 0)
    (t (cond
         ((eq (car lat) 0) (1+ (occur a (cdr lat))))
         (t (occur a (cdr lat)))))))

(defun one? (n)
  (= n 1))

(defun rempick (n lat)
  (cond
    ((one? n) (cdr lat))
    (t (cons (car lat)
             (rempick (1- n) (cdr lat))))))

(in-package :cl-user)
(defpackage the-little-schemer-cl.chapter2
  (:use :cl)
  (:export :lat?
           :member?))
(in-package :the-little-schemer-cl.chapter2)

(defun lat? (l)
  (cond
    ((null l) t)
    ((atom (car l)) (lat? (cdr l)))
    (t nil)))

(defun member? (a lat)
  (cond
    ((null lat) nil)
    (t (or (eq (car lat) a)
           (member? a (cdr lat))))))

;;; The First Commandment (preliminary)
;;; Always ask null as the first question in expressing any function.

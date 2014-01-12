(in-package :cl-user)
(defpackage the-little-schemer-cl.chapter6
  (:use :cl))
(in-package :the-little-schemer-cl.chapter6)

(defun numbered? (aexp)
  (cond
    ((atom aexp) (numberp aexp))
    (t (and (numbered? (car aexp))
            (numbered? (caddr aexp))))))

;;; The Seventh Commandment
;;; Recur on the subparts that are of the same nature:
;;; On the sublists of a list.
;;; On the subexpressions of an arithmetic expression.

;; (defun value (nexp)
;;   (cond
;;     ((atom nexp) nexp)
;;     ((eq (car nexp) '+) (+ (value (cdr nexp))
;;                            (value (caddr nexp))))
;;     ((eq (car nexp) '*) (* (value (cdr nexp))
;;                            (value (caddr nexp))))
;;     (t (expt (value (cdr nexp))
;;              (value (caddr nexp))))))

(defun 1st-sub-exp (aexp)
  (cadr aexp))

(defun 2nd-sub-exp (aexp)
  (caddr aexp))

(defun operator (aexp)
  (car aexp))

(defun value (nexp)
  (cond
    ((atom nexp) nexp)
    ((eq (operator nexp) '+) (+ (value (1st-sub-exp nexp))
                                (value (2nd-sub-exp nexp))))
    ((eq (operator nexp) '*) (* (value (1st-sub-exp nexp))
                                (value (2nd-sub-exp nexp))))
    (t (expt (value (1st-sub-exp nexp))
             (value (2nd-sub-exp nexp))))))

;;; The Eighth Commandment
;;; Use help functions to abstract from representation.

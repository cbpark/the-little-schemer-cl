(in-package :cl-user)
(defpackage the-little-schemer-cl.chapter3
  (:use :cl)
  (:export :firsts
           :multirember))
(in-package :the-little-schemer-cl.chapter3)

;;; The Second Commandment
;;; Use cons to build lists.

(defun rember (a lat)
  (cond
    ((null lat) '())
    ((eq (car lat) a) (cdr lat))
    (t (cons (car lat)
             (rember a (cdr lat))))))

;;; The Third Commandment
;;; When building a list, describe the first typical element, and then cons it
;;; onto the natural recursion.

(defun firsts (l)
  (cond
    ((null l) '())
    (t (cons (caar l)
             (firsts (cdr l))))))

(defun insertR (new old lat)
  (cond
    ((null lat) '())
    (t (cond
         ((eq (car lat) old) (cons old
                                   (cons new
                                         (cdr lat))))
         (t (cons (car lat)
                  (insertR new old (cdr lat))))))))

(defun insertL (new old lat)
  (cond
    ((null lat) '())
    (t (cond
         ((eq (car lat) old) (cons new
                                   (cons old
                                         (cdr lat))))
         (t (cons (car lat)
                  (insertL new old (cdr lat))))))))

(defun my-subst (new old lat)
  (cond
    ((null lat) '())
    (t (cond
         ((eq (car lat) old) (cons new
                                   (cdr lat)))
         (t (cons (car lat)
                  (my-subst new old (cdr lat))))))))

(defun subst2 (new o1 o2 lat)
  (cond
    ((null lat) '())
    (t (cond
         ((eq (car lat) o1) (cons new (cdr lat)))
         ((eq (car lat) o2) (cons new (cdr lat)))
         (t (cons (car lat)
                  (subst2 new o1 o2 (cdr lat))))))))

(defun multirember (a lat)
  (cond
    ((null lat) '())
    (t (cond
         ((eq (car lat) a) (multirember a (cdr lat)))
         (t (cons (car lat)
                  (multirember a (cdr lat))))))))

(defun multiinsertR (new old lat)
  (cond
    ((null lat) '())
    (t (cond
         ((eq (car lat) old) (cons (car lat)
                                   (cons new
                                         (multiinsertR new old (cdr lat)))))
         (t (cons (car lat)
                  (multiinsertR new old (cdr lat))))))))

(defun multiinsertL (new old lat)
  (cond
    ((null lat) '())
    (t (cond
         ((eq (car lat) old) (cons new
                                   (cons old
                                         (multiinsertL new old (cdr lat)))))
         (t (cons (car lat)
                  (multiinsertL new old (cdr lat))))))))

;;; The Fourth Commandment (preliminary)
;;; Always change at least one argument while recurring. It must be changed to
;;; be closer to termination. The changing argument must be tested in the
;;; termination condition:
;;; when using cdr, test termination with null.

(defun multisubst (new old lat)
  (cond
    ((null lat) '())
    (t (cond
         ((eq (car lat) old) (cons new
                                   (multisubst new old (cdr lat))))
         (t (cons (car lat)
                  (multisubst new old (cdr lat))))))))

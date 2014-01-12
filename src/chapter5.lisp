(in-package :cl-user)
(defpackage the-little-schemer-cl.chapter5
  (:use :cl)
  (:import-from :the-little-schemer-cl.chapter4
                :eqan?))
(in-package :the-little-schemer-cl.chapter5)

(defun rember* (a l)
  (cond
   ((null l) '())
   ((atom (car l)) (cond
                    ((eq (car l) a) (rember* a (cdr l)))
                    (t (cons (car l)
                             (rember* a (cdr l))))))
   (t (cons (rember* a (car l))
            (rember* a (cdr l))))))

(defun insertR* (new old l)
  (cond
   ((null l) '())
   ((atom (car l)) (cond
                    ((eq (car l) old) (cons old
                                            (cons new
                                                  (insertR* new old (cdr l)))))
                    (t (cons (car l)
                             (insertR* new old (cdr l))))))
   (t (cons (insertR* new old (car l))
            (insertR* new old (cdr l))))))

;;; The First Commandment (final revision)
;;; When recurring on a list of atoms, lat, ask two questions about it:
;;; (null lat) and t.
;;; When recurring on a number, n, ask two questions about it:
;;; (zerop n) and t.
;;; When recurring on a list of S-expressions, l, ask three questions
;;; about it:
;;; (null l), (atom (car l)), and t.

;;; The Fourth Commandment (final revision)
;;; Always change at least one argument while recurring. When recurring on a
;;; list of atoms, lat, use (cdr lat). When recurring on a number, n, use
;;; (1- n). And when recurring on a list of S-expressions, l, use (car l)
;;; and (cdr l) if neither (null l) nor (atom (car l)) are true.
;;; It must be changed to be closer to termination. The changing argument
;;; must be tested in the termination condition:
;;; when using cdr, test termination with null and
;;; when using 1-, test termination with zerop.

(defun occur* (a l)
  (cond
   ((null l) 0)
   ((atom (car l)) (cond
                     ((eq (car l) a) (1+ (occur* a (cdr l))))
                     (t (occur* a (cdr l)))))
   (t (+ (occur* a (car l))
         (occur* a (cdr l))))))

(defun subst* (new old l)
  (cond
    ((null l) '())
    ((atom (car l)) (cond
                      ((eq (car l) old) (cons new
                                              (subst* new old (cdr l))))
                      (t (cons (car l)
                               (subst* new old (cdr l))))))
    (t (cons (subst* new old (car l))
             (subst* new old (cdr l))))))

(defun insertL* (new old l)
  (cond
    ((null l) '())
    ((atom (car l)) (cond
                      ((eq (car l) old) (cons new
                                              (cons old
                                                    (insertL* new old (cdr l)))))
                      (t (cons (car l)
                               (insertL* new old (cdr l))))))
    (t (cons (insertL* new old (car l))
             (insertL* new old (cdr l))))))

(defun member* (a l)
  (cond
    ((null l) nil)
    ((atom (car l)) (or (eq (car l) a)
                        (member* a (cdr l))))
    (t (or (member* a (car l))
           (member* a (cdr l))))))

(defun leftmost (l)
  (cond
    ((atom (car l)) (car l))
    (t (leftmost (car l)))))

;; (defun eqlist? (l1 l2)
;;   (cond
;;     ((and (null l1) (null l2)) t)
;;     ((or (null l1) (null l2)) nil)
;;     ((and (atom (car l1)) (atom (car l2)))
;;      (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
;;     ((or (atom (car l1)) (atom (car l2))) nil)
;;     (t (and (eqlist? (car l1) (car l2))
;;             (eqlist? (cdr l1) (cdr l2))))))

(defun equal? (s1 s2)
  (cond
    ((and (atom s1) (atom s2)) (eqan? s1 s2))
    ((or (atom s1) (atom s2)) nil)
    (t (eqlist? s1 s2))))

(defun eqlist? (l1 l2)
  (cond
    ((and (null l1) (null l2)) t)
    ((or (null l1) (null l2)) nil)
    (t (and (equal? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))))

;;; The Sixth Commandment
;;; Simplify only after the function is correct.

(defun rember-equal (s l)
  (cond
    ((null l) '())
    ((equal? (car l) s) (cdr l))
    (t (cons (car l)
             (rember-equal s (cdr l))))))

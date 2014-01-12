(in-package :cl-user)
(defpackage the-little-schemer-cl.chapter7
  (:use :cl)
  (:import-from :the-little-schemer-cl.chapter2
                :member?)
  (:import-from :the-little-schemer-cl.chapter3
                :multirember
                :firsts)
  (:export :build
           :revpair
           :a-pair?))
(in-package :the-little-schemer-cl.chapter7)

(defun set? (lat)
  (cond
    ((null lat) t)
    ((member? (car lat) (cdr lat)) nil)
    (t (set? (cdr lat)))))

(defun makeset (lat)
  (cond
    ((null lat) '())
    (t (cons (car lat)
             (makeset (multirember (car lat) (cdr lat)))))))

(defun subset? (set1 set2)
  (cond
    ((null set1) t)
    (t (and (member? (car set1) set2)
            (subset? (cdr set1) set2)))))

(defun eqset? (set1 set2)
  (and (subset? set1 set2)
       (subset? set2 set1)))

(defun intersect? (set1 set2)
  (cond
    ((null set1) nil)
    (t (or (member? (car set1) set2)
           (intersect? (cdr set1) set2)))))

(defun intersect (set1 set2)
  (cond
    ((null set1) '())
    ((member? (car set1) set2) (cons (car set1)
                                     (intersect (cdr set1) set2)))
    (t (intersect (cdr set1) set2))))

(defun my-union (set1 set2)
  (cond
    ((null set1) set2)
    ((member? (car set1) set2) (my-union (cdr set1) set2))
    (t (cons (car set1)
             (my-union (cdr set1) set2)))))

(defun intersectall (l-set)
  (cond
    ((null (cdr l-set)) (car l-set))
    (t (intersect (car l-set)
                  (intersectall (cdr l-set))))))

(defun a-pair? (x)
  (cond
    ((atom x) nil)
    ((null x) nil)
    ((null (cdr x)) nil)
    ((null (caddr x)) t)
    (t nil)))

(defun build (s1 s2)
  (cond
    (t (cons s1
             (cons s2 '())))))

(defun fun? (rel)
  (set? (firsts rel)))

;; (defun revrel (rel)
;;   (cond
;;     ((null rel) '())
;;     (t (cons (build (second (car rel))
;;                     (first (car rel)))
;;              (revrel (cdr rel))))))

(defun revpair (pair)
  (build (second pair) (first pair)))

(defun revrel (rel)
  (cond
    ((null rel) '())
    (t (cons (revpair (car rel))
             (revrel (cdr rel))))))

(defun seconds (l)
  (cond
    ((null l) '())
    (t (cons (cadar l)
             (seconds (cdr l))))))

(defun fullfun? (fun)
  (set? (seconds fun)))

(defun one-to-one? (fun)
  (fun? (revrel fun)))

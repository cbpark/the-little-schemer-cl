(in-package :cl-user)
(defpackage the-little-schemer-cl.chapter9
  (:use :cl)
  (:import-from :the-little-schemer-cl.chapter4
                :pick)
  (:import-from :the-little-schemer-cl.chapter7
                :build
                :a-pair?
                :revpair))
(in-package :the-little-schemer-cl.chapter9)

(defun looking (a lat)
  (keep-looking a (pick 1 lat) lat))

(defun keep-looking (a sorn lat)
  (cond
    ((numberp sorn) (keep-looking a (pick sorn lat) lat))
    (t (eq sorn a))))

(defun eternity (x)
  (eternity x))

(defun shift (pair)
  (build (first (first pair))
         (build (second (first pair))
                (second pair))))

;; (print (shift '((a b) (c d))))

(defun align (pora)
  (cond
    ((atom pora) pora)
    ((a-pair? (first pora)) (align (shift pora)))
    (t (build (first pora)
              (align (second pora))))))

(defun length* (pora)
  (cond
    ((atom pora) 1)
    (t (+ (length* (first pora))
          (length* (second pora))))))

(defun weight* (pora)
  (cond
    ((atom pora) 1)
    (t (+ (* (weight* (first pora)) 2)
          (weight* (second pora))))))

;; (print (weight* '((a b) c)))
;; ((weight* '(a (b c))))

(defun shuffle (pora)
  (cond
    ((atom pora) pora)
    ((a-pair? (first pora)) (shuffle (revpair pora)))
    (t (build (first pora)
              (shuffle (second pora))))))

;; (print (shuffle '(a (b c))))
;; (print (shuffle '(a b)))

(defun A (n m)
  (cond
    ((zerop n) (1+ m))
    ((zerop m) (A (1- n) 1))
    (t (A (1- n)
          (A n (1- m))))))

;; (print (A 1 0))
;; (print (A 1 1))
;; (print (A 2 2))

;; Y combinator in Common Lisp.
(defun Y (le)
  ((lambda (f) (funcall f f))
   (lambda (f)
     (funcall le (lambda (&rest x)
                   (apply (funcall f f) x))))))

(defun fac (f)
  #'(lambda (n)
      (if (zerop n)
          1
          (* n (funcall f (1- n))))))

;; (print (mapcar (Y #'fac) '(1 2 3 4 5 6 7 8 9 10)))

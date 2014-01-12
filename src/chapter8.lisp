(in-package :cl-user)
(defpackage the-little-schemer-cl.chapter8
  (:use :cl))
(in-package :the-little-schemer-cl.chapter8)

(defun eq?-c (a)
  #'(lambda (x)
      (eq x a)))

(defparameter eq?-salad nil) ; to remove warning of undefined variable.
(setf eq?-salad (eq?-c 'salad))

;; (print (funcall eq?-salad 'salad))
;; (print (funcall eq?-salad 'tuna))

(defun rember-f (test?)
  #'(lambda (a l)
      (cond
        ((null l) '())
        ((funcall test? (car l) a) (cdr l))
        (t (cons (car l)
                 (funcall (rember-f test?) a (cdr l)))))))

;; (print (funcall (rember-f #'eq) 'tuna '(shrimp salad and tuna salad)))
;; (print (funcall (rember-f #'eq) 'eq '(equal eq eqan eqlist eqpair)))

(defparameter rember-eq? nil)
(setf rember-eq? (rember-f #'eq))

;; (print (funcall rember-eq? 'tuna '(tuna salad is good)))

;; Another way of the implementation for rember-f.
;; (defun rember-f (test? a l &optional (acc nil))
;;   (cond
;;    ((null l) (reverse acc))
;;    (t (rember-f test? a (cdr l) (let ((it (car l)))
;;                                   (cond
;;                                     ((funcall test? a it) acc)
;;                                     (t (cons it
;;                                              acc))))))))

;; (print (rember-f #'(lambda (x y)
;;                      (eq x y)) 'tuna '(shrimp salad and tuna salad)))

(defun insertL-f (test?)
  #'(lambda (new old l)
      (cond
        ((null l) '())
        ((funcall test? (car l) old) (cons new
                                           (cons old
                                                 (cdr l))))
        (t (cons (car l)
                 (funcall (insertL-f test?) new old (cdr l)))))))

(defun insertR-f (test?)
  #'(lambda (new old l)
      (cond
        ((null l) '())
        ((funcall test? (car l) old) (cons old
                                           (cons new
                                                 (cdr l))))
        (t (cons (car l)
                 (funcall (insertR-f test?) new old (cdr l)))))))

(defun seqL (new old l)
  (cons new (cons old l)))

(defun seqR (new old l)
  (cons old (cons new l)))

(defun insert-g (seq)
  #'(lambda (new old l)
      (cond
        ((null l) '())
        ((eq (car l) old) (funcall seq new old (cdr l)))
        (t (cons (car l)
                 (funcall (insert-g seq) new old (cdr l)))))))

(defparameter insertL nil)
;; (setf insertL (insert-g 'seqL))
(setf insertL (insert-g #'(lambda (new old l)
                            (cons new (cons old l)))))

(defparameter insertR nil)
;; (setf insertR (insert-g 'seqR))
(setf insertR (insert-g #'(lambda (new old l)
                            (cons old (cons new l)))))

;;; The Ninth Commandment
;;; Abstract common patterns with a new function.

(defun multirember-f (test?)
  #'(lambda (a lat)
      (cond
        ((null lat) '())
        ((funcall test? a (car lat)) (funcall (multirember-f test?) a (cdr lat)))
        (t (cons (car lat)
                 (funcall (multirember-f test?) a (cdr lat)))))))

;; (print (funcall (multirember-f 'eq) 'tuna '(shrimp salad tuna salad and tuna)))

(defparameter multirember-eq? nil)
(setf multirember-eq? (multirember-f #'eq))

(defparameter eq?-tuna nil)
(setf eq?-tuna (eq?-c 'tuna))

;; (print (funcall eq?-tuna 'tuna))
;; (print (funcall eq?-tuna 'salad))

(defun multiremberT (test? lat)
  (cond
    ((null lat) '())
    ((funcall test? (car lat)) (multiremberT test? (cdr lat)))
    (t (cons (car lat)
             (multiremberT test? (cdr lat))))))

;; (print (multiremberT eq?-tuna '(shrimp salad tuna salad and tuna)))

(defun multirember&co (a lat col)
  (cond
    ((null lat) (funcall col '() '()))
    ((eq (car lat) a)
     (multirember&co a (cdr lat) #'(lambda (newlat seen)
                                     (funcall col newlat (cons (car lat)
                                                               seen)))))
    (t (multirember&co a (cdr lat) #'(lambda (newlat seen)
                                       (funcall col (cons (car lat)
                                                          newlat)
                                                seen))))))

(defun a-friend (x y)
  (declare (ignore x))
  (null y))

(defun new-friend (newlat seen)
  (funcall #'a-friend newlat (cons 'tuna seen)))

(defun latest-friend (newlat seen)
  (funcall #'a-friend (cons 'and newlat) seen))

(defun last-friend (x y)
  (declare (ignore y))
  (length x))

;;; The Tenth Commandment
;;; Build functions to collect more than one value at a time.

(defun multiinsertLR (new oldL oldR lat)
  (cond
    ((null lat) '())
    ((eq (car lat) oldL) (cons new
                               (cons oldL
                                     (multiinsertLR new oldL oldR (cdr lat)))))
    ((eq (car lat) oldR) (cons oldR
                               (cons new
                                     (multiinsertLR new oldL oldR (cdr lat)))))
    (t (cons (car lat)
             (multiinsertLR new oldL oldR (cdr lat))))))

(defun multiinsertLR&co (new oldL oldR lat col)
  (cond
    ((null lat) (funcall col '() 0 0))
    ((eq (car lat) oldL)
     (multiinsertLR&co new oldL oldR (cdr lat)
                       #'(lambda (newlat L R)
                           (funcall col (cons new
                                              (cons oldL
                                                    newlat))
                                    (1+ L) R))))
    ((eq (car lat) oldR)
     (multiinsertLR&co new oldL oldR (cdr lat)
                       #'(lambda (newlat L R)
                           (funcall col (cons oldR
                                              (cons new
                                                    newlat))
                                    L (1+ R)))))
    (t
     (multiinsertLR&co new oldL oldR (cdr lat)
                       #'(lambda (newlat L R)
                           (funcall col (cons (car lat)
                                              newlat)
                                    L R))))))

(defun col (x y z)
  (cons y
        (cons z x)))

;; (print (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) #'col))

(defun evens-only* (l)
  (cond
    ((null l) '())
    ((atom (car l)) (cond
                      ((evenp (car l)) (cons (car l)
                                             (evens-only* (cdr l))))
                      (t (evens-only* (cdr l)))))
    (t (cons (evens-only* (car l))
             (evens-only* (cdr l))))))

;; (print (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)))

(defun evens-only*&co (l col)
  (cond
   ((null l) (funcall col '() 1 0))
   ((atom (car l)) (cond
                    ((evenp (car l))
                     (evens-only*&co (cdr l) #'(lambda (newl p s)
                                                 (funcall col (cons (car l)
                                                                    newl)
                                                          (* (car l) p) s))))
                    (t (evens-only*&co (cdr l) #'(lambda (newl p s)
                                                   (funcall col newl
                                                            p (+ (car l) s)))))))
   (t (evens-only*&co (car l) #'(lambda (al ap as)
                                  (evens-only*&co (cdr l)
                                                  #'(lambda (dl dp ds)
                                                      (funcall col (cons al dl)
                                                               (* ap dp)
                                                               (+ as ds)))))))))

(defun the-last-friend (newl product sum)
  (cons sum
        (cons product
              newl)))

;; (print (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) #'the-last-friend))

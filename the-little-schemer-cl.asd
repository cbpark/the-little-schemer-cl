(in-package :cl-user)
(defpackage the-little-schemer-cl-asd
  (:use :cl :asdf))
(in-package :the-little-schemer-cl-asd)

(defsystem the-little-schemer-cl
  :version "0.1"
  :author "Chan Beom Park"
  :license "BSD"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "chapter2")
                 (:file "chapter3")
                 (:file "chapter4")
                 (:file "chapter5" :depends-on ("chapter4"))
                 (:file "chapter6")
                 (:file "chapter7" :depends-on ("chapter2"
                                                "chapter3"))
                 (:file "chapter8")
                 (:file "chapter9" :depends-on ("chapter4"
                                                "chapter7")))))
  :description "Codes from The Little Schemer in Common Lisp"
  :long-description "")

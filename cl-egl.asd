;;;; cl-egl.asd

(asdf:defsystem #:cl-egl
  :description "Common Lisp wrapper for libEGL"
  :author "Malcolm Still"
  :license "BSD3"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
               (:file "cl-egl")))


;;;; cl-egl.asd

(asdf:defsystem #:cl-egl
  :description "Common Lisp wrapper for libEGL"
  :author "Malcolm Still"
  :license "BSD 3-Clause"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
               (:file "cl-egl")))


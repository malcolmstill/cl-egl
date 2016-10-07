;;;; cl-egl.asd

(asdf:defsystem #:cl-egl
  :description "Describe cl-egl here"
  :author "Malcolm Still"
  :license "Specify license here"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
               (:file "cl-egl")))


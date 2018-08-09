;;;; package.lisp

(defpackage :egl
  (:use :common-lisp :cffi)
  (:export
   get-display
   initialize
   bind-api
   choose-config
   create-context
   create-window-surface
   make-current
   swap-buffers
   destroy-surface
   destroy-context
   terminate
   EGLenum))



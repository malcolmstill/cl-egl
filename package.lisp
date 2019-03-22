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
   create-image
   destroy-image
   get-proc-address
   init-egl-wayland
   bind-wayland-display
   query-wayland-buffer
   image-target-texture-2DOES))



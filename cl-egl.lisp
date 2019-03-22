
(in-package :egl)

(define-foreign-library libegl
  (:unix (:or "libEGL.so.1"))
  (t (:default "libEGL")))

(use-foreign-library libegl)

(defctype EGLBoolean :uint)
(defctype EGLDisplay :pointer)
(defctype EGLConfig :pointer)
(defctype EGLSurface :pointer)
(defctype EGLContext :pointer)
(defctype EGLint :int32)

(defcenum (eglenum EGLint)
  (:surface-type #x3033)
  (:window-bit #x0004)
  (:red-size #x3024)
  (:blue-size #x3022)
  (:green-size #x3023)
  (:depth-size #x3025)
  (:renderable-type #x3040)
  (:opengl-bit #x0008)
  (:opengl-es-bit #x0001)
  (:opengl-api #x30A2)
  (:context-major-version #x3098)
  (:context-minor-version #x30FB)
  (:width #x3057)
  (:height #x3056)
  (:none #x3038))

(defcfun ("eglGetError" get-error) EGLint)

(defcfun ("eglGetDisplay" get-display) EGLDisplay
  (display-id :pointer))

(defcfun "eglInitialize" EGLBoolean
  (display EGLDisplay)
  (major :pointer)
  (minor :pointer))

(defun initialize (display)
  (with-foreign-objects
      ((major 'EGLint 1)
       (minor 'EGLint 1))
    (when (= (eglInitialize display major minor) 0)
      (terminate display)
      (error "Failed to initialize EGL with code ~d" (get-error))
      )
    (format t "~A~%" (get-error))
    (values (mem-aref major 'EGLint)
	    (mem-aref minor 'EGLint))))

(defcfun "eglChooseConfig" EGLBoolean
  (display EGLDisplay)
  (attrib-list (:pointer EGLint))
  (configs (:pointer EGLConfig))
  (config-size EGLint)
  (num-config (:pointer EGLint)))

(defun choose-config (display config-size &rest config-attribs)
  (with-foreign-objects
      ((requested-attribs 'EGLint (length config-attribs))
       (available-configs '(:pointer EGLConfig) 1)
       (num-configs 'EGLint 1))
    (loop :for i :from 0 :to (- (length config-attribs) 1)
       :do (setf (mem-aref requested-attribs 'EGLint i)
		 (if (keywordp (nth i config-attribs))
		     (foreign-enum-value 'eglenum (nth i config-attribs))
		     (nth i config-attribs))))
    (eglchooseconfig display requested-attribs available-configs config-size num-configs)
    (loop :for i :from 0 :to (- (mem-aref num-configs 'EGLint) 1)
       :collecting (mem-aref available-configs :pointer i))))

(defcfun "eglCreateContext" EGLContext
  (display EGLDisplay)
  (config EGLConfig)
  (share-context EGLContext)
  (attrib-list (:pointer EGLint)))

(defun create-context (display config share-context &rest attribs)
  (with-foreign-objects
      ((requested-attribs 'EGLint (length attribs)))
    (loop :for i :from 0 :to (- (length attribs) 1)
       :do (setf (mem-aref requested-attribs 'EGLint i)
		 (if (keywordp (nth i attribs))
		     (foreign-enum-value 'eglenum (nth i attribs))
		     (nth i attribs))))
    (eglcreatecontext display config share-context requested-attribs)))

(defcfun ("eglCreateWindowSurface" create-window-surface) EGLSurface
  (display EGLDisplay)
  (config EGLConfig)
  (win :pointer)
  (attrib-list (:pointer EGLint)))

(defcfun ("eglTerminate" terminate) EGLBoolean
  (display EGLDisplay))

(defcfun "eglBindAPI" EGLBoolean
  (api :uint))

(defun bind-api (api)
  (eglbindapi (foreign-enum-value 'eglenum api)))
  
(defcfun ("eglMakeCurrent" make-current) EGLBoolean
  (display EGLDisplay)
  (draw EGLSurface)
  (read EGLSurface)
  (context EGLContext))

(defcfun ("eglSwapBuffers" swap-buffers) EGLBoolean
  (display EGLDisplay)
  (surface EGLSurface))

(defcfun ("eglDestroySurface" destroy-surface) EGLBoolean
  (display EGLDisplay)
  (surface EGLSurface))

(defcfun ("eglDestroyContext" destroy-context) EGLBoolean
  (display EGLDisplay)
  (context EGLContext))

(defcfun ("eglCreateImage" create-image) :pointer
  (display EGLDisplay)
  (context EGLContext)
  (target EGLint)
  (buffer :pointer)
  (attrib-list (:pointer EGLint)))

(defcfun ("eglDestroyImage" destroy-image) EGLBoolean
  (display EGLDisplay)
  (image :pointer))

(defcfun ("eglGetProcAddress" get-proc-address) :pointer
  (name :string))

(defvar *query-wayland-buffer* nil)
(defvar *bind-wayland-display* nil)
(defvar *image-target-texture-2DOES* nil)

(defun init-egl-wayland ()
  (setf *bind-wayland-display* (get-proc-address "eglBindWaylandDisplayWL"))
  (setf *query-wayland-display* (get-proc-address "eglQueryWaylandBufferWL"))
  (setf *image-target-texture-2DOES* (get-proc-address "glEGLImageTargetTexture2DOES")))

(defun bind-wayland-display (egl-display wl-display)
  (foreign-funcall-pointer *bind-wayland-display* ()
			   :pointer egl-display
			   :pointer wl-display
			   :void))

(defun query-wayland-buffer (egl-display buffer attribute value)
  (foreign-funcall-pointer *query-wayland-display* ()
			   :pointer egl-display
			   :pointer buffer
			   EGLint attribute
			   :pointer value
			   EGLBoolean))

(defun image-target-texture-2DOES (attribute egl-image)
  (foreign-funcall-pointer *image-target-texture-2DOES* ()
			   EGLint attribute
			   :pointer egl-image
			   :void))

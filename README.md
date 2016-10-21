
# cl-egl

EGL is a cross-platform between OpenGL / OpenGL ES and native platform windowing systems. On Linux it allows using OpenGL without depending on X. `cl-egl` is a Common Lisp wrapper for libEGL.

## Status

`cl-egl` is being developed primarily in support of [ulubis](https://github.com/malcolmstill/ulubis) and is therefor feature incomplete. Pull requests adding more of the API are more than welcome.

## Requiremnts

`cl-egl` (obiously) requires libEGL. It is likely that libEGL already exists on your Linux installation if it is recent.

## Installation

```
CL-USER> (ql:quickload :cl-egl)
```

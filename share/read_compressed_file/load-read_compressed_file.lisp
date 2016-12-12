(in-package #-gcl #:maxima #+GCL "MAXIMA")

#+ecl
($load "lisp-utils/defsystem.lisp")

(load (merge-pathnames (make-pathname :name "read_compressed_file" :type "system") (maxima-load-pathname-directory)))

(mk:oos "read_compressed_file" :compile)

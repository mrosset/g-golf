;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2018
;;;; Free Software Foundation, Inc.

;;;; This file is part of GNU G-Golf

;;;; GNU G-Golf is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU Lesser General Public License as
;;;; published by the Free Software Foundation; either version 3 of the
;;;; License, or (at your option) any later version.

;;;; GNU G-Golf is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.

;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with GNU G-Golf.  If not, see
;;;; <https://www.gnu.org/licenses/lgpl.html>.
;;;;

;;; Commentary:

;;; Code:


(define-module (g-golf gi types)
  #:use-module (oop goops)  
  #:use-module (g-golf support enum)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (%g-golf-bi-info-type))


;;;
;;; Base Info
;;;

(define %g-golf-bi-info-type
  (make <gi-enum>
    #:gi-name "GIInfoType"
    #:scm-name "gi-info-type"
    #:enum-set '(invalid
                 function
                 callback
                 struct
                 boxed
                 enum
                 flags
                 object
                 interface
                 constant
                 error-domain ;; invalid_0 - deleted
                 union
                 value
                 signal
                 vfunc
                 property
                 field
                 arg
                 type
                 unresolved)))

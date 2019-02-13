;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2019
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


(define-module (g-golf gi common-types)
  #:use-module (oop goops)  
  #:use-module (g-golf support enum)
  ;; #:use-module (g-golf gi utils)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (%gi-type-tag
	    %gi-array-type))


;;;
;;; Common Types
;;;

(define %gi-type-tag
  (make <gi-enum>
    #:gi-name "GITypeTag"
    #:enum-set '(void
                 boolean
                 int8
                 uint8
                 int16
                 uint16
                 int32
                 uint32
                 int64
                 uint64
                 float
                 double
                 gtype
                 utf8
                 filename
                 array
                 interface
                 glist
                 gslist
                 ghash
                 error
                 unichar)))

(define %gi-array-type
  (make <gi-enum>
    #:gi-name "GIArrayType"
    #:enum-set '(c
                 array
                 ptr-array
                 byte-array)))

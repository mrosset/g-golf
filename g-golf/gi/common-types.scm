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
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-golf support enum)
  #:use-module (g-golf support union)
  #:use-module (g-golf gi utils)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (%gi-type-tag
	    %gi-array-type
            %gi-argument-desc
            %gi-argument-fields
            %gi-argument-types
            %gi-argument-size
            make-gi-argument
            gi-argument-ref
            gi-argument-set!))


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

(define %gi-argument-desc
  `((v-boolean . ,int)
    (v-int8 . ,int8)
    (v-uint8 . ,uint8)
    (v-int16 . ,int16)
    (v-uint16 . ,uint16)
    (v-int32 . ,int32)
    (v-uint32 . ,uint32)
    (v-int64 . ,int64)
    (v-uint64 . ,uint64)
    (v-float . ,float)
    (v-double . ,double)
    (v-short . ,short)
    (v-ushort . ,unsigned-short)
    (v-int . ,int)
    (v-uint . ,unsigned-int)
    (v-long . ,long)
    (v-ulong . ,unsigned-long)
    (v-ssize . ,int)
    (v-size . ,unsigned-int)
    (v-string . ,'*)
    (v-pointer . ,'*)))

(define %gi-argument-fields
  (map car %gi-argument-desc))

(define %gi-argument-types
  (map cdr %gi-argument-desc))

(define %gi-argument-size
  (apply max (map sizeof %gi-argument-types)))

(define (make-gi-argument)
  (make-c-union %gi-argument-types))

(define (gi-argument-ref gi-argument field)
  (let* ((type (assq-ref %gi-argument-desc field))
         (val (if type
                  (c-union-ref gi-argument %gi-argument-size type)
                  (error "No such field: " field))))
    (case field
      ((v-boolean)
       (gi->scm val 'boolean))
      ((v-string)
       (gi->scm val 'string))
      ((v-pointer)
       (gi->scm val 'pointer))
      (else
       val))))

(define (gi-argument-set! gi-argument field val)
  (let ((type (assq-ref %gi-argument-desc field)))
    (if type
        (let ((u-val (case field
                       ((v-boolean)
                        (if val 1 0))
                       ((v-string)
                        (string->pointer val))
                       (else
                        val))))
          (c-union-set! gi-argument %gi-argument-size type u-val))
        (error "No such field: " field))))

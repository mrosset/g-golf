;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2019
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


(define-module (g-golf support c-union)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)

  #:export (make-c-union
            c-union-ref
            c-union-set!))


;;;
;;; Guile C union type
;;;

(define %readers
  (@@ (system foreign) *readers*))

(define %writers
  (@@ (system foreign) *writers*))

(define %align
  (@@ (system foreign) align))

(define* (make-c-union types #:optional (type #f) (val #f))
  (let* ((size (apply max (map sizeof types)))
         (bv (make-bytevector size 0))
         (foreign (bytevector->pointer bv)))
    (if type
        (c-union-set! foreign size type val))
    (values foreign size)))

(define (c-union-ref foreign size type)
  (let ((bv (pointer->bytevector foreign size))
        (offset (%align 0 (alignof type))))
    ((assv-ref %readers type) bv offset)))

(define (c-union-set! foreign size type val)
  (let ((bv (pointer->bytevector foreign size))
        (offset (%align 0 (alignof type))))
    ((assv-ref %writers type) bv offset val)))

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


(define-module (g-golf support bytevector)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)

  #:export (bv-ptr-ref
            bv-ptr-set!))


(define %align
  (@@ (system foreign) align))

(define %bv-ptr-ref
  (@@ (system foreign) bytevector-pointer-ref))

(define %bv-ptr-set!
  (@@ (system foreign) bytevector-pointer-set!))

(define (bv-ptr-ref foreign)
  (let* ((size (sizeof '*))
         (bv (pointer->bytevector foreign size))
         (offset (%align 0 (alignof '*))))
    (%bv-ptr-ref bv offset)))

(define (bv-ptr-set! foreign val)
  (let* ((size (sizeof '*))
         (bv (pointer->bytevector foreign size))
         (offset (%align 0 (alignof '*))))
    (%bv-ptr-set! bv offset val)))

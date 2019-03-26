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


(define-module (g-golf hl-api function)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-golf support)
  #:use-module (g-golf gi)
  #:use-module (g-golf glib)
  #:use-module (g-golf gobject)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)
  
  #:export (gi-import-function))


#;(g-export )


;;;
;;; 
;;;

(define (gi-import-function info)
  (let ((name (g-function-info-get-symbol info))
        (flags (g-function-info-get-flags info))
        (n-args (g-callable-info-get-n-args info))
        (i-transfert (g-callable-info-get-caller-owns info))
        (null-allowed? (g-callable-info-may-return-null info)))
    (dimfi name n-args i-transfert null-allowed?)
    (do ((i 0
            (+ i 1)))
        ((= i n-args))
      (let* ((arg (g-callable-info-get-arg info i))
             (name (g-base-info-get-name arg))
             (a-closure (g-arg-info-get-closure arg))
             (a-destroy (g-arg-info-get-destroy arg))
             (a-direction (g-arg-info-get-direction arg))
             (a-transfert (g-arg-info-get-ownership-transfer arg))
             (a-scope (g-arg-info-get-scope arg))
             (a-type (g-arg-info-get-type arg))
             #;(a-type-string (g-info-type-to-string a-type))
             (a-type-tag (g-type-info-get-tag a-type))
             (a-type-is-pointer? (g-type-info-is-pointer a-type))
             (a-null-allowed? (g-arg-info-may-be-null arg))
             (a-is-caller-allocate? (g-arg-info-is-caller-allocates arg))
             (a-is-optional? (g-arg-info-is-optional arg))
             (a-is-return-value? (g-arg-info-is-return-value arg))
             (a-is-skip (g-arg-info-is-skip arg)))
        (dimfi "  " name
               a-closure
               a-destroy
               a-direction
               a-transfert
               a-scope
               #;a-type
               #;a-type-string
               a-type-tag
               #;(case a-type-tag
                 ((interface)
                  (gi-interface-g-type a-type))
                 (else
                  (symbol->g-type a-type-tag)))
               a-type-is-pointer?
               a-null-allowed?
               a-is-caller-allocate?
               a-is-optional?
               a-is-return-value?)))))

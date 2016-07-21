;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016
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


(define-module (gbank gi typelib)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (gbank gi init)
  #:use-module (gbank gi glib)
  #:use-module (gbank gi utils)

  #:export (gbank-typelib-new
	    call-with-input-typelib
	    gbank-tl-new-from-memory
	    gbank-tl-free
	    ;; gbank-tl-symbol
	    gbank-tl-get-name-space))


;;;
;;; Utils [None GI Low Level API]
;;;

(define (gbank-typelib-new filename)
  (let* ((bv (call-with-input-file filename get-bytevector-all #:binary #t))
	 (bv-pointer (bytevector->pointer bv))
	 (bv-length (bytevector-length bv))
	 (g-bv (gbank-gl-memdup bv-pointer bv-length)))
    (with-gerror gerror
		 (gbank-tl-new-from-memory g-bv bv-length gerror))))

(define (call-with-typelib proc typelib)
  (call-with-values
      (lambda () (proc typelib))
    (lambda vals
      (gbank-tl-free typelib)
      (apply values vals))))

(define (call-with-input-typelib file proc)
  (call-with-typelib proc (gbank-typelib-new file)))


;;;
;;; Low level API
;;;

(define (gbank-tl-new-from-memory bv-pointer bv-length gerror)
  (g-typelib-new-from-memory bv-pointer bv-length gerror))

(define (gbank-tl-free typelib)
  (g-typelib-free typelib))

(define (gbank-tl-get-name-space typelib)
  (gbank-gtype->scm (g-typelib-get-namespace typelib)
		    'gchar*))


;;;
;;; GI Bindings
;;;

(define g-typelib-new-from-memory
  (pointer->procedure '*
                      (dynamic-func "g_typelib_new_from_memory"
				    %libgirepository)
                      (list '* int '*)))

(define g-typelib-free
  (pointer->procedure void
                      (dynamic-func "g_typelib_free"
				    %libgirepository)
                      (list '*)))

(define g-typelib-get-namespace
  (pointer->procedure '*
                      (dynamic-func "g_typelib_get_namespace"
				    %libgirepository)
                      (list '*)))

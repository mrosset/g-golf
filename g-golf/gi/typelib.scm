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


(define-module (g-golf gi typelib)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  #:use-module (g-golf glib)
  #:use-module (g-golf gi utils)

  #:export (g-golf-typelib-new
	    call-with-input-typelib
	    g-golf-tl-new-from-memory
	    g-golf-tl-free
	    ;; g-golf-tl-symbol
	    g-golf-tl-get-name-space))


;;;
;;; Utils [None GI Low Level API]
;;;

(define (g-golf-typelib-new filename)
  (let* ((bv (call-with-input-file filename get-bytevector-all #:binary #t))
	 (bv-pointer (bytevector->pointer bv))
	 (bv-length (bytevector-length bv))
	 (g-bv (g-golf-gl-memdup bv-pointer bv-length)))
    (with-gerror gerror
		 (g-golf-tl-new-from-memory g-bv bv-length gerror))))

(define (call-with-typelib proc typelib)
  (call-with-values
      (lambda () (proc typelib))
    (lambda vals
      (g-golf-tl-free typelib)
      (apply values vals))))

(define (call-with-input-typelib file proc)
  (call-with-typelib proc (g-golf-typelib-new file)))


;;;
;;; Low level API
;;;

(define (g-golf-tl-new-from-memory bv-pointer bv-length gerror)
  (g-typelib-new-from-memory bv-pointer bv-length gerror))

(define (g-golf-tl-free typelib)
  (g-typelib-free typelib))

(define (g-golf-tl-get-name-space typelib)
  (g-golf-gtype->scm (g-typelib-get-namespace typelib)
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

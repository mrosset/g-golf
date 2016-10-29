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


(define-module (g-golf glib mem-alloc)
  #:use-module (system foreign)
  #:use-module (g-golf init)

  #:export (g-golf-gl-malloc
	    g-golf-gl-malloc0
	    g-golf-gl-free
	    g-golf-gl-memdup))


;;;
;;; Glib Low level API
;;;

(define (g-golf-gl-malloc n)
  (g-malloc n))

(define (g-golf-gl-malloc0 n)
  (g-malloc0 n))

(define (g-golf-gl-free pointer)
  (g-free pointer))

(define (g-golf-gl-memdup pointer n)
  (g-memdup pointer n))


;;;
;;; Glib Bindings
;;;

(define g-malloc
  (pointer->procedure '*
                      (dynamic-func "g_malloc"
				    %libglib)
                      (list int)))

(define g-malloc0
  (pointer->procedure '*
                      (dynamic-func "g_malloc0"
				    %libglib)
                      (list int)))

(define g-free
  (pointer->procedure void
                      (dynamic-func "g_free"
				    %libglib)
                      (list '*)))

(define g-memdup
  (pointer->procedure '*
                      (dynamic-func "g_memdup"
				    %libglib)
                      (list '* int)))

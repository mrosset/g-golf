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


(define-module (g-golf glib quarks)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf init)

  #:export (g-quark-from-string
            g-quark-to-string))


;;;
;;; Glib Low level API
;;;

(define (g-quark-from-string str)
  (g_quark_from_string (scm->gi str 'string)))

(define (g-quark-to-string g-quark)
  (gi->scm (g_quark_to_string g-quark) 'string))


;;;
;;; Glib Bindings
;;;

(define g_quark_from_string
  (pointer->procedure uint32
                      (dynamic-func "g_quark_from_string"
				    %libglib)
                      (list '*)))	;; string

(define g_quark_to_string
  (pointer->procedure '*
                      (dynamic-func "g_quark_to_string"
				    %libglib)
                      (list uint32)))	;; g-quark

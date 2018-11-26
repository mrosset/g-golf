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


(define-module (g-golf gobject params-vals)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  #:use-module (g-golf support enum)
  #:use-module (g-golf gobject enum-flags)
  #:use-module (g-golf gobject generic-values)
  #:use-module (g-golf gobject type-info)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-value->g-type
            g-value-ref
            g-value-set!
            g-value-get-int
            g-value-set-int
            g-value-get-float
            g-value-set-float
            g-value-get-string
            g-value-set-string))


;;;
;;; G-Golf Low Level API
;;;

(define (g-value->g-type g-value)
  (match (parse-c-struct g-value
                         (list unsigned-long double double))
    ((g-type _ _)
     (g-type->symbol g-type))))

(define (g-value-ref g-value)
  (case (g-value->g-type g-value)
    ((int)
     (g-value-get-int g-value))
    ((float)
     (g-value-get-float g-value))
        ((string)
     (g-value-get-string g-value))
    (else
     (error "Not implemented:" (g-value->g-type g-value)))))

(define (g-value-set! g-value value)
  (case (g-value->g-type g-value)
    ((int)
     (g-value-set-int g-value value))
    ((float)
     (g-value-set-float g-value value))
    ((string)
     (g-value-set-string g-value value))
    (else
     (error "Not implemented:" (g-value->g-type g-value)))))


;;;
;;; GObject Low level API
;;;

(define (g-value-get-int g-value)
  (g_value_get_int g-value))

(define (g-value-set-int g-value int)
  (g_value_set_int g-value int))

(define (g-value-get-float g-value)
  (g_value_get_float g-value))

(define (g-value-set-float g-value float)
  (g_value_set_float g-value float))

(define (g-value-get-string g-value)
  (let ((pointer (g_value_get_string g-value)))
    (if (null-pointer? pointer)
        ""
        (pointer->string pointer))))

(define (g-value-set-string g-value str)
  (g_value_set_string g-value
                      (string->pointer str)))


;;;
;;; GObject Bindings
;;;

(define g_value_get_string
  (pointer->procedure '*
                      (dynamic-func "g_value_get_string"
				    %libgobject)
                      (list '*)))

(define g_value_set_string
  (pointer->procedure void
                      (dynamic-func "g_value_set_string"
				    %libgobject)
                      (list '*
                            '*)))

(define g_value_get_int
  (pointer->procedure int
                      (dynamic-func "g_value_get_int"
				    %libgobject)
                      (list '*)))

(define g_value_set_int
  (pointer->procedure void
                      (dynamic-func "g_value_set_int"
				    %libgobject)
                      (list '*
                            int)))

(define g_value_get_float
  (pointer->procedure float
                      (dynamic-func "g_value_get_float"
				    %libgobject)
                      (list '*)))

(define g_value_set_float
  (pointer->procedure void
                      (dynamic-func "g_value_set_float"
				    %libgobject)
                      (list '*
                            float)))

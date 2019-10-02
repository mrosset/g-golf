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


(define-module (g-golf hl-api closure)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
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
  
  #:export (<g-closure>
            <closure>
            
            g-closure-marshal))


(g-export !g-closure
          !return-val
          !n-param
          !param-vals
          !invocation-hint
          !marshal-data
          
          !function
          !return-type
          !param-types)

(define-class <g-closure> ()
  (g-closure #:accessor !g-closure)
  (return-val #:accessor !return-val)
  (n-param #:accessor !n-param)
  (param-vals #:accessor !param-vals)
  (invocation-hint #:accessor !invocation-hint)
  (marshal-data #:accessor !marshal-data))

(define-class <closure> (<g-closure>)
  (function #:accessor !function #:init-keyword #:function)
  (return-type #:accessor !return-type #:init-keyword #:return-type)
  (param-types #:accessor !param-types #:init-keyword #:param-types))

(define-method (initialize (self <closure>) initargs)
  (let ((function (or (get-keyword #:function initargs #f)
                      (error "Missing #:function initarg: " initargs)))
        (return-type (or (get-keyword #:return-type initargs #f)
                         (error "Missing #:return-type initarg: " initargs)))
        (param-types (or (get-keyword #:param-types initargs #f)
                         (error "Missing #:param-types initarg: " initargs)))
        (g-closure (g-closure-new-simple (g-closure-size) #f)))
    (next-method)
    (set! (!g-closure self) g-closure)
    (g-closure-ref g-closure)
    (g-closure-sink g-closure)
    ;; ... wip
    ))

(define (g-closure-marshal g-closure
                           return-val
                           n-param
                           param-vals
                           invocation-hint
                           marshal-data)
  #f)


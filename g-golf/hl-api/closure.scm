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
  
  #:export (<closure>

            g-closure-marshal
            %g-closure-marshal
            g-closure-free
            %g-closure-free

            %g-closure-function-cache-default-size
            %g-closure-function-cache
            g-closure-function-cache-ref
            g-closure-function-cache-set!
            g-closure-function-cache-remove!
            g-closure-function-cache-for-each
            g-closure-function-cache-show))


(g-export !g-closure
          !function
          !return-type
          !param-types)


(define-class <closure> ()
  (g-closure #:accessor !g-closure)
  (function #:accessor !function #:init-keyword #:function)
  (return-type #:accessor !return-type #:init-keyword #:return-type)
  (param-types #:accessor !param-types #:init-keyword #:param-types))

(define-method (initialize (self <closure>) initargs)
  (let* ((function (or (get-keyword #:function initargs #f)
                       (error "Missing #:function initarg: " initargs)))
         (return-type (or (get-keyword #:return-type initargs #f)
                          (error "Missing #:return-type initarg: " initargs)))
         (param-types (or (get-keyword #:param-types initargs #f)
                          (error "Missing #:param-types initarg: " initargs)))
         (g-closure (g-closure-new-simple (g-closure-size) #f)))
    (next-method)
    (set! (!g-closure self) g-closure)
    (g-closure-function-cache-set! g-closure function)
    (g-closure-ref g-closure)
    (g-closure-sink g-closure)
    (g-closure-set-marshal g-closure %g-closure-marshal)
    (g-closure-add-invalidate-notifier g-closure #f %g-closure-free)))

(define (g-closure-marshal g-closure
                           return-val
                           n-param
                           param-vals
                           invocation-hint
                           marshal-data)
  (let* ((function (g-closure-function-cache-ref g-closure))
         (args (let loop ((i 0)
                          (g-values-ptr param-vals)
                          (results '()))
                 (if (= i n-param)
                     (reverse! results)
                     (loop (+ i 1)
                           (gi-pointer-inc g-values-ptr)
                           (cons (g-value-ref g-values-ptr)
                                 results)))))
         (result (apply function args)))
    (g-value-set! return-val result)
    (values)))

(define %g-closure-marshal
  (procedure->pointer int
                      g-closure-marshal
                      (list '*			;; gclosure
                            '*			;; return-value
                            unsigned-int	;; n-param
                            '*			;; param-values
                            '*			;; invocation-hint
                            '*)))		;; marshal-data

(define (g-closure-free data g-closure)
  (g-closure-function-cache-remove! g-closure))

(define %g-closure-free
  (procedure->pointer void
                      g-closure-free
                      (list '*		;; data
                            '*)))	;; gclosure



;;;
;;; The g-closure-function-cache
;;;

(define %g-closure-function-cache-default-size 1013)

(define %g-closure-function-cache
  (make-hash-table %g-closure-function-cache-default-size))

(define (g-closure-function-cache-ref g-closure)
  (hashq-ref %g-closure-function-cache
             (pointer-address g-closure)))

(define (g-closure-function-cache-set! g-closure function)
  (hashq-set! %g-closure-function-cache
              (pointer-address g-closure)
              function))

(define (g-closure-function-cache-remove! g-closure)
  (hashq-remove! %g-closure-function-cache
                 (pointer-address g-closure)))

(define (g-closure-function-cache-for-each proc)
  (hash-for-each proc
                 %g-closure-function-cache))

(define %g-closure-function-cache-show-prelude
  "The g-closure function cache entries are")

(define* (g-closure-function-cache-show #:optional
                                        (port (current-output-port)))
  (format port "~A~%"
          %g-closure-function-cache-show-prelude)
  (letrec ((show (lambda (key value)
                   (format port "  ~S  -  ~S~%"
                           key
                           value))))
    (g-closure-function-cache-for-each show)))

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


(define-module (g-golf gobject signals)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support enum)
  #:use-module (g-golf support flag)
  #:use-module (g-golf gobject type-info)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf init)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-signal-query
            g-signal-lookup

            %g-signal-flags))


;;;
;;; Low level API
;;;

(define %g-signal-query-struct
  (list unsigned-int	;; id
        '*		;; name
        unsigned-long	;; g-type
        unsigned-int	;; flags
        unsigned-long	;; return-type
        unsigned-int	;; n-param
        '*))		;; param-types

(define (g-signal-query-parse g-signal-query)
  (parse-c-struct g-signal-query
                  %g-signal-query-struct))

(define (g-signal-query-make)
  (make-c-struct %g-signal-query-struct
                 (list 0
                       %null-pointer
                       0
                       0
                       0
                       0
                       %null-pointer)))

#;(define (g-signal-query->id g-signal-query)
  (match (g-signal-query-parse g-signal-query)
    ((id _ _ _ _ _ _) id)))

(define (g-signal-query id)
  (let ((gsq (g-signal-query-make)))
    (g_signal_query id gsq)
    (match (parse-c-struct gsq
                           %g-signal-query-struct)
      ((id name g-type flags return-type n-param param-types)
       (list id
             (gi->scm name 'string)
             g-type
             (gi-integer->gflags %g-signal-flags flags)
             (g-type->symbol return-type)
             n-param
             (decode-param-types n-param param-types))))))

(define (g-signal-lookup name g-type)
  (let ((gsl (g_signal_lookup (scm->gi name 'string)
                              g-type)))
    (case gsl
      ((0)
       (error "No such g-type signal: " g-type name))
      (else
       gsl))))


;;;
;;; Utils
;;;

(define (decode-param-types n-param param-types)
  (let* ((p-size (sizeof unsigned-long))
         (bv (pointer->bytevector param-types
                                  (* n-param p-size))))
    (let loop ((i 0)
               (results '()))
      (if (= i n-param)
          (reverse! results)
          (loop (+ i 1)
                (cons (g-type->symbol
                       (bytevector-u64-native-ref bv i))
                      results))))))


;;;
;;; Signals Bindings
;;;

(define g_signal_query
  (pointer->procedure void
                      (dynamic-func "g_signal_query"
				    %libgobject)
                      (list unsigned-int	;; id
                            '*)))		;; query

(define g_signal_lookup
  (pointer->procedure unsigned-int
                      (dynamic-func "g_signal_lookup"
				    %libgobject)
                      (list '*			;; name
                            unsigned-long)))	;; g-type


;;;
;;; Types and Values
;;;

(define %g-signal-flags
  (make <gi-flag>
    #:gi-name "GsignalFlags"
    #:enum-set '(run-first
                 run-last
                 run-cleanup
                 no-recurse
                 detailed
                 action
                 no-hooks
                 must-collect
                 deprecated)))

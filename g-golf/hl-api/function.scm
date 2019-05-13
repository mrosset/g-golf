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
  
  #:export (gi-import-function
            <function>
            <argument>))


(g-export describe	;; function and argument
          !name
          !type-desc

          !flags	;; functoon
          !n-arg
          !caller-owns
          !return-type
          !may-return-null?
          !arguments
          !n-gi-arg-in
          !args-in
          !gi-args-in
          !n-gi-arg-out
          !args-out
          !gi-args-out

          !closure	;; argument
          !destroy
          !direction
          !transfert
          !scope
          !type-tag
          !forced-type
          !is-pointer?
          !may-be-null?
          !is-caller-allocate?
          !is-optional?
          !is-return-value?
          !is-skip?
          !gi-argument
          !gi-argument-field

          is-interface?)


;;;
;;; 
;;;

(define (gi-import-function info)
  (let* ((cm (current-module))
         (function (make <function> #:info info))
         (name (!name function)))
    (g-base-info-unref info)
    (gi-cache-set! 'function name function)
    (module-define! cm
                    name
                    (lambda ( . args)
                      (let ((function function)
                            (name name))
                        (check-n-arg (!n-gi-arg-in function) args)
                        (prepare-gi-arguments function args)
                        #;(with-gerror g-error
                                     (g-function-info-invoke info
                                                             gi-arg-in
                                                             n-gi-arg-in
			                                     gi-arg-out
                                                             n-gi-arg-out
			                                     gi-arg-res
                                                             g-error))
                        function)))
    (module-g-export! cm `(,name))))

(define-class <function> ()
  (name #:accessor !name)
  (flags #:accessor !flags)
  (n-arg #:accessor !n-arg)
  (caller-owns #:accessor !caller-owns)
  (return-type #:accessor !return-type)
  (type-desc #:accessor !type-desc)
  (may-return-null? #:accessor !may-return-null?)
  (arguments #:accessor !arguments)
  (n-gi-arg-in #:accessor !n-gi-arg-in)
  (args-in #:accessor !args-in)
  (gi-args-in #:accessor !gi-args-in)
  (n-gi-arg-out #:accessor !n-gi-arg-out)
  (args-out #:accessor !args-out)
  (gi-args-out #:accessor !gi-args-out))

(define-method (initialize (self <function>) initargs)
  (let ((info (or (get-keyword #:info initargs #f)
                  (error "Missing #:info initarg: " initargs))))
    (next-method self '())
    (let* ((gi-name (g-function-info-get-symbol info))
           (scm-name (g-name->scm-name gi-name))
           (name (string->symbol scm-name))
           (return-type-info (g-callable-info-get-return-type info))
           (return-type (g-type-info-get-tag return-type-info))
           (type-desc (type-description return-type-info #:type-tag return-type)))
      (g-base-info-unref return-type-info)
      (slot-set! self 'name name)
      (slot-set! self 'flags (g-function-info-get-flags info))
      (slot-set! self 'caller-owns (g-callable-info-get-caller-owns info))
      (slot-set! self 'return-type return-type)
      (slot-set! self 'type-desc type-desc)
      (slot-set! self 'may-return-null? (g-callable-info-may-return-null info))
      (receive (n-arg args
                n-gi-arg-in args-in gi-args-in
                n-gi-arg-out args-out gi-args-out)
          (function-arguments-and-gi-arguments info)
        (slot-set! self 'n-arg n-arg)
        (slot-set! self 'arguments args)
        (slot-set! self 'n-gi-arg-in n-gi-arg-in)
        (slot-set! self 'args-in args-in)
        (slot-set! self 'gi-args-in gi-args-in)
        (slot-set! self 'n-gi-arg-out n-gi-arg-out)
        (slot-set! self 'args-out args-out)
        (slot-set! self 'gi-args-out gi-args-out)))))

(define-method* (describe (self <function>) #:key (port #t))
  (next-method self #:port port)
  (if (boolean? port)
      (newline)
      (newline port))
  (for-each (lambda (argument)
              (describe argument #:port port)
              (if (boolean? port)
                  (newline)
                  (newline port)))
      (!arguments self)))

(define-class <argument> ()
  (name #:accessor !name)
  (closure #:accessor !closure)
  (destroy #:accessor !destroy)
  (direction #:accessor !direction)
  (transfert #:accessor !transfert)
  (scope #:accessor !scope)
  (type-tag #:accessor !type-tag)
  (type-desc #:accessor !type-desc)
  (forced-type #:accessor !forced-type)
  (is-pointer? #:accessor !is-pointer?)
  (may-be-null? #:accessor !may-be-null?)
  (is-caller-allocate? #:accessor !is-caller-allocate?)
  (is-optional? #:accessor !is-optional?)
  (is-return-value? #:accessor !is-return-value?)
  (is-skip? #:accessor !is-skip?)
  (gi-argument #:accessor !gi-argument)
  (gi-argument-field #:accessor !gi-argument-field))

(define-method (initialize (self <argument>) initargs)
  (let ((info (or (get-keyword #:info initargs #f)
                  (error "Missing #:info initarg: " initargs))))
    (next-method self '())
    (let* ((gi-name (g-base-info-get-name info))
           (scm-name (g-name->scm-name gi-name))
           (name (string->symbol scm-name))
           (direction (g-arg-info-get-direction info))
           (type-info (g-arg-info-get-type info))
           (type-tag (g-type-info-get-tag type-info))
           (type-desc (type-description type-info #:type-tag type-tag))
           (is-pointer? (g-type-info-is-pointer type-info))
           (forced-type (arg-info-forced-type direction type-tag is-pointer?)))
      (g-base-info-unref type-info)
      (slot-set! self 'name name)
      (slot-set! self 'closure (g-arg-info-get-closure info))
      (slot-set! self 'destroy (g-arg-info-get-destroy info))
      (slot-set! self 'direction direction)
      (slot-set! self 'transfert (g-arg-info-get-ownership-transfer info))
      (slot-set! self 'scope (g-arg-info-get-scope info))
      (slot-set! self 'type-tag type-tag)
      (slot-set! self 'type-desc type-desc)
      (slot-set! self 'forced-type forced-type)
      (slot-set! self 'is-pointer? is-pointer?)
      (slot-set! self 'may-be-null? (g-arg-info-may-be-null info))
      (slot-set! self 'is-caller-allocate? (g-arg-info-is-caller-allocates info))
      (slot-set! self 'is-optional? (g-arg-info-is-optional info))
      (slot-set! self 'is-return-value? (g-arg-info-is-return-value info))
      (slot-set! self 'is-skip? (g-arg-info-is-skip info))
      (slot-set! self 'gi-argument (make-gi-argument))
      (slot-set! self 'gi-argument-field
                 (gi-type-tag->field forced-type)))))

(define-method (is-interface? (self <argument>))
  (and (eq? (!type-tag self 'interface))
       (!type-desc self)))

(define-method* (describe (self <argument>) #:key (port #t))
  (next-method self #:port port))

(define (check-n-arg n-arg-in args)
  (if (= n-arg-in (length args))
      #t
      (error "Wrong number of arguments: " args)))

(define (arg-info-forced-type direction type-tag is-pointer?)
  (if (or is-pointer?
          (eq? direction 'inout)
          (eq? direction 'out))
      'pointer
      type-tag))

(define* (type-description info #:key (type-tag #f))
  (let ((type-tag (or type-tag
                      (g-type-info-get-tag info))))
    (case type-tag
      ((interface)
       (interface->g-type info))
      ((array)
       (array-description info))
      (else
       type-tag))))

(define (interface->g-type info)
  (let* ((info (g-type-info-get-interface info))
         (type (g-base-info-get-type info)))
    (if (is-registered? type)
        (receive (gi-type name id)
            (registered-type->gi-type info type)
          (g-base-info-unref info)
          (list type name id gi-type))
        (begin
          (g-base-info-unref info)
          type))))

(define (registered-type->gi-type info type)
  (let* ((id (g-registered-type-info-get-g-type info))
         (name (g-studly-caps-expand (g-type-name id)))
         (key (string->symbol name)))
    (case type
      ((enum)
       (values id
               key
               (or (gi-cache-ref 'enum key)
                   (let ((gi-enum (gi-enum-import info)))
                     (gi-cache-set! 'enum key gi-enum)
                     gi-enum))))
      ((struct)
       (values id
               key
               (or (gi-cache-ref 'boxed key)
                   (let ((gi-struct (gi-struct-import info)))
                     (gi-cache-set! 'boxed key gi-struct)
                     gi-struct))))
      (else
       (values #f key id)))))

(define (is-registered? type-tag)
  (member type-tag
          '(enum
            interface
            object
            struct
            union)))

(define (array-description info)
  (let* ((type (g-type-info-get-array-type info))
         (fixed-size (g-type-info-get-array-fixed-size info))
         (is-zero-terminated (g-type-info-is-zero-terminated info))
         (n (g-type-info-get-array-length info))
         (param-type (and (>= n 0)
                          (g-type-info-get-param-type info n)))
         (param-tag (and param-type
                         (g-type-info-get-tag param-type))))
    (when param-type
      (g-base-info-unref param-type))
    (list (cons 'array type)
          (cons 'fixed-size fixed-size)
          (cons 'is-zero-terminated is-zero-terminated)
          (cons 'param-n n)
          (cons 'param-tag param-tag))))

(define (function-arguments-and-gi-arguments info)
  (let ((n-arg (g-callable-info-get-n-args info)))
    (let loop ((i 0)
               (arguments '())
               (n-gi-arg-in 0)
               (args-in '())
               (n-gi-arg-out 0)
               (args-out '()))
      (if (= i n-arg)
          (let* ((gi-args-in-bv (if (> n-gi-arg-in 0)
                                    (make-bytevector (* %gi-argument-size
                                                        n-gi-arg-in)
                                                     0)
                                    #f))
                 (gi-args-in (if gi-args-in-bv
                                 (bytevector->pointer gi-args-in-bv)
                                 %null-pointer))
                 (gi-args-out-bv (if (> n-gi-arg-out 0)
                                     (make-bytevector (* %gi-argument-size
                                                         n-gi-arg-out)
                                                      0)
                                     #f))
                 (gi-args-out (if gi-args-out-bv
                                  (bytevector->pointer gi-args-out-bv)
                                  %null-pointer)))
            (values n-arg
                    (reverse! arguments)
                    n-gi-arg-in
                    (reverse! args-in)
                    gi-args-in
                    n-gi-arg-out
                    (reverse! args-out)
                    gi-args-out))
          (let* ((arg-info (g-callable-info-get-arg info i))
                 (argument (make <argument> #:info arg-info)))
            (g-base-info-unref arg-info)
            (case (!direction argument)
              ((in)
               (loop (+ i 1)
                     (cons argument arguments)
                     (+ n-gi-arg-in 1)
                     (cons argument args-in)
                     n-gi-arg-out
                     args-out))
              ((inout)
               (loop (+ i 1)
                     (cons argument arguments)
                     (+ n-gi-arg-in 1)
                     (cons argument args-in)
                     (+ n-gi-arg-out 1)
                     (cons argument args-out)))
              ((out)
               (loop (+ i 1)
                     (cons argument arguments)
                     n-gi-arg-in
                     args-in
                     (+ n-gi-arg-out 1)
                     (cons argument args-out)))))))))

(define (prepare-gi-arguments function args)
  (let ((arguments (!arguments function))
        (n-gi-arg-in (!n-gi-arg-in function))
        (args-in (!args-in function))
        (n-gi-arg-out (!n-gi-arg-out function))
        (args-out (!args-out function)))
    (prepare-gi-args-in function n-gi-arg-in args-in args)
    #;(prepare-gi-args-in function n-gi-arg-out args-out)))

(define (prepare-gi-args-in function n-gi-arg-in args-in args)
  (let loop ((i 0))
    (if (= i n-gi-arg-in)
        #t
        (let* ((arg-in (list-ref args-in i))
               (gi-argument (!gi-argument arg-in))
               (field (!gi-argument-field arg-in))
               (val (list-ref args i)))
          (gi-argument-set! gi-argument field (scm->gi val))))))

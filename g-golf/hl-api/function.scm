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

          !closure	;; argument
          !destroy
          !direction
          !transfert
          !scope
          !type-tag
          !is-pointer?
          !may-be-null?
          !is-caller-allocate?
          !is-optional?
          !is-return-value?
          !is-skip?
          !gi-argument
          !gi-argument-field
          !value

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
                            (n-arg (!n-arg function))
                            (arguments (!arguments function)))
                        (check-n-arg n-arg args)
                        #;(with-gerror g-error
                                   (g-function-info-invoke info
                                                           gi-args-in
                                                           n-gi-args-in
			                                   gi-args-out
                                                           n-gi-args-out
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
  (arguments #:accessor !arguments))

(define-method (initialize (self <function>) initargs)
  (let ((info (or (get-keyword #:info initargs #f)
                  (error "Missing #:info initarg: " initargs))))
    (next-method self '())
    (let* ((gi-name (g-function-info-get-symbol info))
           (scm-name (g-name->scm-name gi-name))
           (name (string->symbol scm-name))
           (n-arg (g-callable-info-get-n-args info))
           (return-type-info (g-callable-info-get-return-type info))
           (return-type (g-type-info-get-tag return-type-info))
           (type-desc (gi-type-description return-type-info return-type)))
      (g-base-info-unref return-type-info)
      (slot-set! self 'name name)
      (slot-set! self 'flags (g-function-info-get-flags info))
      (slot-set! self 'n-arg n-arg)
      (slot-set! self 'caller-owns (g-callable-info-get-caller-owns info))
      (slot-set! self 'return-type return-type)
      (slot-set! self 'type-desc type-desc)
      (slot-set! self 'may-return-null? (g-callable-info-may-return-null info))
      (slot-set! self 'arguments (make-arguments info n-arg)))))

(define-method* (describe (self <function>) #:key (port #t))
  (next-method self #:port port)
  (for-each (lambda (argument)
              (describe argument #:port port))
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
  (is-pointer? #:accessor !is-pointer?)
  (may-be-null? #:accessor !may-be-null?)
  (is-caller-allocate? #:accessor !is-caller-allocate?)
  (is-optional? #:accessor !is-optional?)
  (is-return-value? #:accessor !is-return-value?)
  (is-skip? #:accessor !is-skip?)
  (gi-argument #:accessor !gi-argument)
  (gi-argument-field #:accessor !gi-argument-field)
  (value #:accessor !value))

(define-method (initialize (self <argument>) initargs)
  (let ((info (or (get-keyword #:info initargs #f)
                  (error "Missing #:info initarg: " initargs))))
    (next-method self '())
    (let* ((gi-name (g-base-info-get-name info))
           (scm-name (g-name->scm-name gi-name))
           (name (string->symbol scm-name))
           (type-info (g-arg-info-get-type info))
           (type-tag (g-type-info-get-tag type-info))
           (type-desc (gi-type-description type-info type-tag))
           (is-pointer? (g-type-info-is-pointer type-info)))
      (g-base-info-unref type-info)
      (slot-set! self 'name name)
      (slot-set! self 'closure (g-arg-info-get-closure info))
      (slot-set! self 'destroy (g-arg-info-get-destroy info))
      (slot-set! self 'direction (g-arg-info-get-direction info))
      (slot-set! self 'transfert (g-arg-info-get-ownership-transfer info))
      (slot-set! self 'scope (g-arg-info-get-scope info))
      (slot-set! self 'type-tag type-tag)
      (slot-set! self 'type-desc type-desc)
      (slot-set! self 'is-pointer? is-pointer?)
      (slot-set! self 'may-be-null? (g-arg-info-may-be-null info))
      (slot-set! self 'is-caller-allocate? (g-arg-info-is-caller-allocates info))
      (slot-set! self 'is-optional? (g-arg-info-is-optional info))
      (slot-set! self 'is-return-value? (g-arg-info-is-return-value info))
      (slot-set! self 'is-skip? (g-arg-info-is-skip info))
      (slot-set! self 'gi-argument (make-gi-argument))
      (slot-set! self 'gi-argument-field
                 (if is-pointer?
                     'v-pointer
                     (gi-type-tag->field type-tag))))))

(define-method (is-interface? (self <argument>))
  (and (eq? (!type-tag self 'interface))
       (!type-desc self)))

(define (make-arguments info n-arg)
  (let loop ((i 0)
             (arguments '()))
    (if (= i n-arg)
        (reverse! arguments)
        (let* ((info (g-callable-info-get-arg info i))
               (argument (make <argument> #:info info)))
          (g-base-info-unref info)
          (loop (+ i 1)
                (cons argument arguments))))))

(define (check-n-arg n-arg args)
  (if (= n-arg (length args))
      #t
      (error "Wrong number of arguments: " args)))

(define* (gi-type-description type-info #:optional (type-tag #f))
  (let ((type-tag (or type-tag
                      (g-type-info-get-tag type-info))))
    (case type-tag
      ((interface)
       (interface->g-type type-info))
      ((array)
       (cons 'array
             (g-type-info-get-array-type type-info)))
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

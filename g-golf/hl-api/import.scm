;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2018 - 2019
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


(define-module (g-golf hl-api import)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-golf support)
  #:use-module (g-golf gi)
  #:use-module (g-golf glib)
  #:use-module (g-golf gobject)
  #:use-module (g-golf hl-api gtype)
  #:use-module (g-golf hl-api gobject)
  #:use-module (g-golf hl-api function)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)
  
  #:export (%gi-base-info-types
            %gi-imported-base-info-types
            gi-import))


#;(g-export )


;;;
;;;
;;;

(define %gi-base-info-types '())
(define %gi-imported-base-info-types '())

(define (gi-import namespace)
  (g-irepository-require namespace)
  (let ((n-info (g-irepository-get-n-infos namespace))
        (base-info-types (reverse %gi-base-info-types))
        (imported-base-info-types (reverse %gi-imported-base-info-types)))
    (do ((i 0
            (+ i 1)))
        ((= i n-info))
      (let* ((info (g-irepository-get-info namespace i))
             (i-type (g-base-info-get-type info)))
        #;(dimfi (g-base-info-get-name info) " " i-type)
        (unless (memq i-type
                      base-info-types)
          (push! i-type base-info-types))
        (case i-type
          ((enum flags)
           (unless (memq i-type
                         imported-base-info-types)
             (push! i-type imported-base-info-types))
           (gi-import-enum info))
          ((struct)
           (unless (memq i-type
                         imported-base-info-types)
             (push! i-type imported-base-info-types))
           (gi-import-struct info))
          ((function)
           (unless (memq i-type
                         imported-base-info-types)
             (push! i-type imported-base-info-types))
           (gi-import-function info))
          ((object)
           (unless (memq i-type
                         imported-base-info-types)
             (push! i-type imported-base-info-types))
           (gi-import-object info))
          (else
           ;; I won't do nothing, not even displaying a message, till
           ;; G-Golf is complete, because it would fill the repl ...
           ;; Ultimately, it will raise an exception
           'nothing))))
    (set! %gi-base-info-types (reverse! base-info-types))
    (set! %gi-imported-base-info-types (reverse! imported-base-info-types))
    (values)))

(define (gi-import-object info)
  (let* ((cm (current-module))
         (r-type (g-registered-type-info-get-g-type info))
         (gi-name (g-type-name r-type))
         (c-name (g-name->class-name gi-name))
         (c-inst (make-class (list <gobject>)
                             '()
                             #:name c-name
                             #:info info)))
    (module-define! cm c-name c-inst)
    (module-g-export! cm `(,c-name))))

(define (gi-import-enum info)
  (let* ((id (g-registered-type-info-get-g-type info))
         (name (g-studly-caps-expand (g-type-name id)))
         (key (string->symbol name)))
    (or (gi-cache-ref 'enum key)
        (let ((gi-enum (gi-enum-import info)))
          (gi-cache-set! 'enum key gi-enum)
          (gi-enum-import-methods info)
          gi-enum))))

(define (gi-import-struct info)
  (let* ((id (g-registered-type-info-get-g-type info))
         (name (g-studly-caps-expand (g-type-name id)))
         (key (string->symbol name)))
    (or (gi-cache-ref 'boxed key)
        (let ((gi-struct (gi-struct-import info)))
          (gi-cache-set! 'boxed key gi-struct)
          (gi-struct-import-methods info)
          gi-struct))))

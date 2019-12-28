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

;; This code is largely inspired by the Guile-Gnome modules
;; (gnome gobject generics) and (gnome gobject gsignal), see:

;;   https://www.gnu.org/software/guile-gnome

;;   http://git.savannah.gnu.org/cgit/guile-gnome.git
;;     tree/glib/gnome/gobject/generic.scm
;;     tree/glib/gnome/gobject/gsignal.scm

;;; Code:


(define-module (g-golf hl-api signal)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-golf support)
  #:use-module (g-golf gi)
  #:use-module (g-golf glib)
  #:use-module (g-golf gobject)
  #:use-module (g-golf hl-api gtype)
  #:use-module (g-golf hl-api closure)

  #:replace (connect)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)
  
  #:export (<signal>

            %gi-signal-cache

            gi-signal-cache-ref
            gi-signal-cache-set!

            gi-signal-cache-show
            gi-signal-cache-find))


(g-export !id
          !name
          !flags
          !interface-type
          !return-type
          !param-types
          #;!class-generic

          connect-after)


;;;
;;; Signal connection
;;;

(define %connect
  (module-ref the-root-module 'connect))

(define-method (connect . args)
  "The core Guile implementation of the connect(2) POSIX call"
  (apply %connect args))

(define-method* (connect (inst <gtype-instance>) name function
                         #:optional (after? #f) (detail #f))
  (signal-connect inst name function after? detail))

(define-method* (connect-after (inst <gtype-instance>) name function
                               #:optional (detail #f))
  (signal-connect inst name function #t detail))

(define (signal-connect inst s-name function after? detail)
  ;; Below, i- stands for interface-, s- for signal-
  (let* ((i-class (class-of inst))
         (i-class-name (class-name i-class)))
    (or (gi-signal-cache-ref i-class-name s-name)
        (let* ((i-type (!gtype-id i-class))
               (s-id (or (g-signal-lookup (symbol->string s-name) i-type)
                         (error "No such signal: " i-class-name s-name)))
               (g-signal (g-signal-query s-id)))
          (match g-signal
            ((id name i-type flags return-type n-param param-types)
             (let ((signal (make <signal>
                             #:id id
                             #:name name
                             #:interface-type i-type
                             #:flags flags
                             #:return-type return-type
                             #:n-param n-param
                             #:param-types param-types))
                   (closure (make <closure>
                              #:function function
                              #:return-type return-type
                              #:param-types (cons 'object param-types))))
               (gi-signal-cache-set! i-class-name s-name signal)
               (g-signal-connect-closure-by-id (!g-inst inst)
                                               id
                                               detail
                                               (!g-closure closure)
                                               after?)
               (values))))))))


;;;
;;; The <signal> class, accesors and methods
;;;

(define-class <signal> ()
  (id #:accessor !id #:init-keyword #:id #:init-value #f)
  (name #:accessor !name #:init-keyword #:name)
  (interface-type #:accessor !interface-type #:init-keyword #:interface-type)
  (flags #:accessor !flags #:init-keyword #:flags)
  (return-type #:accessor !return-type #:init-keyword #:return-type)
  (n-param #:accessor !n-param #:init-keyword #:n-param)
  (param-types #:accessor !param-types #:init-keyword #:param-types)
  #;(class-generic #:accessor !class-generic
                 #:init-keyword #:class-generic #:init-value #f))

(define-method (initialize (self <signal>) initargs)
  (next-method)
  'wip)


;;;
;;; The gi-signal-cache
;;;

(define %gi-signal-cache
  '())

(define (gi-signal-cache-ref m-key s-key)
  ;; m-key, s-key stand for main and secondary keys
  (let ((subcache (assq-ref %gi-signal-cache m-key)))
    (and subcache
         (assq-ref subcache s-key))))

(define (gi-signal-cache-set! m-key s-key val)
  (let ((subcache (assq-ref %gi-signal-cache m-key)))
    (set! %gi-signal-cache
          (assq-set! %gi-signal-cache m-key
                     (assq-set! (or subcache '()) s-key
                                val)))))

(define* (gi-signal-cache-show #:optional (m-key #f))
  (format #t "%gi-signal-cahe~%")
  (if m-key
      (begin
        (format #t "  ~A~%" m-key)
        (for-each (lambda (s-entry)
                    (match s-entry
                      ((s-key . s-vals)
                       (format #t "    ~A~%" s-key))))
            (assq-ref %gi-signal-cache m-key)))
      (for-each (lambda (m-entry)
                  (match m-entry
                    ((m-key . m-vals)
                     (format #t "  ~A~%" m-key))))
          %gi-signal-cache)))

(define (gi-signal-cache-find name)
  'wip)

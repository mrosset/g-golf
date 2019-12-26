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
            ))


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

(define (signal-connect inst name func after? detail)
  'wip)


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

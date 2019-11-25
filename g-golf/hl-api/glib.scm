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

;; This code is largely inspired by the Guile-Gnome module (gnome glib)

;;   https://www.gnu.org/software/guile-gnome

;;   http://git.savannah.gnu.org/cgit/guile-gnome.git
;;     tree/glib/gnome/glib.scm

;;; Code:


(define-module (g-golf hl-api glib)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-golf support)
  #:use-module (g-golf glib)
  #:use-module (g-golf gobject)
  #:use-module (g-golf gi)
  #:use-module (g-golf hl-api closure)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-idle-add
            g-timeout-add
            g-timeout-add-seconds))


#;(g-export )


(define (g-idle-add proc)
  (let* ((closure (make <closure>
                    #:function proc
                    #:return-type 'boolean
                    #:param-types '()))
         (g-closure (!g-closure closure))
         (source (g-idle-source-new))
         (dummy (g-source-set-closure source g-closure))
         (id (g-source-attach source #f)))
    (g-source-unref source)
    (g-closure-unref g-closure)
    id))

(define (g-timeout-add interval proc)
  (let* ((closure (make <closure>
                    #:function proc
                    #:return-type 'boolean
                    #:param-types '()))
         (g-closure (!g-closure closure))
         (source (g-timeout-source-new interval))
         (dummy (g-source-set-closure source g-closure))
         (id (g-source-attach source #f)))
    (g-source-unref source)
    (g-closure-unref g-closure)
    id))

(define (g-timeout-add-seconds interval proc)
  (let* ((closure (make <closure>
                    #:function proc
                    #:return-type 'boolean
                    #:param-types '()))
         (g-closure (!g-closure closure))
         (source (g-timeout-source-new-seconds interval))
         (dummy (g-source-set-closure source g-closure))
         (id (g-source-attach source #f)))
    (g-source-unref source)
    (g-closure-unref g-closure)
    id))

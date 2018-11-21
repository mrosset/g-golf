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


(define-module (g-golf gobject generic-values)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  #:use-module (g-golf gi utils)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-value-init))


;;;
;;; GObject Low level API
;;;

(define (g-value-init g-type)
  ;; guile-gnome gobject.c does this:
  ;;   GValue value = { 0, };
  ;; Below g-value is a pointer to a (newly created) GValue
  (let ((g-value (make-c-struct (list size_t int64 int64)
				(list 0 0 0))))
    (g_value_init g-value g-type)
    g-value))


;;;
;;; GObject Bindings
;;;

(define g_value_init
  (pointer->procedure '*
                      (dynamic-func "g_value_init"
				    %libgobject)
                      (list '* int)))

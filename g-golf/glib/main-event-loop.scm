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


(define-module (g-golf glib main-event-loop)
  #:use-module (system foreign)
  #:use-module (g-golf init)

  #:export (g-main-loop-new
            g-idle-source-new))


;;;
;;; Glib Low level API
;;;

(define (g-main-loop-new context is-running?)
  (g_main_loop_new (if context context %null-pointer)
                   (if is-running? 1 0)))

(define (g-idle-source-new)
  (g_idle_source_new))


;;;
;;; Glib Bindings
;;;

(define g_main_loop_new
  (pointer->procedure '*
                      (dynamic-func "g_main_loop_new"
				    %libglib)
                      (list '*		;; context
                            int)))	;; is-running?

(define g_idle_source_new
  (pointer->procedure '*
                      (dynamic-func "g_idle_source_new"
				    %libglib)
                      (list )))	;; void

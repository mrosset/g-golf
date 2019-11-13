;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2019
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
  #:use-module (g-golf gi utils)
  
  #:export (g-main-loop-new
            g-main-loop-ref
            g-main-loop-unref
            g-main-loop-run
            g-main-loop-quit
            g-main-context-new
            g-main-context-default
            g-timeout-source-new
            g-timeout-source-new-seconds
            g-idle-source-new
            g-source-ref
            g-source-unref
            g-source-attach
            g-source-destroy
            g-source-is-destroyed?
            g-source-set-priority
            g-source-get-priority
            g-source-remove))


;;;
;;; Glib Low level API
;;;

(define* (g-main-loop-new #:optional (context #f) (is-running? #f))
  (g_main_loop_new (if context context %null-pointer)
                   (if is-running? 1 0)))

(define (g-main-loop-ref loop)
  (gi->scm (g_main_loop_ref loop) 'pointer))

(define (g-main-loop-unref loop)
  (g_main_loop_unref loop))

(define (g-main-loop-run loop)
  (g_main_loop_run loop))

(define (g-main-loop-quit loop)
  (g_main_loop_quit loop))

(define (g-main-context-new)
  (gi->scm (g_main_context_new) 'pointer))

(define (g-main-context-default)
  (gi->scm (g_main_context_default) 'pointer))

(define (g-timeout-source-new interval)
  (g_timeout_source_new interval))

(define (g-timeout-source-new-seconds interval)
  (g_timeout_source_new_seconds interval))

(define (g-idle-source-new)
  (g_idle_source_new))

(define (g-source-ref source)
  (g_source_ref source))

(define (g-source-unref source)
  (g_source_unref source))

(define (g-source-attach source context)
  (g_source_attach source
                   (scm->gi context 'pointer)))

(define (g-source-destroy source)
  (g_source_destroy source))

(define (g-source-is-destroyed? source)
  (gi->scm (g_source_is_destroyed source) 'boolean))

(define (g-source-set-priority source priority)
  (g_source_set_priority source priority))

(define (g-source-get-priority source)
  (g_source_get_priority source))

(define (g-source-remove id)
  (gi->scm (g_source_remove id) 'boolean))


;;;
;;; Glib Bindings
;;;

(define g_main_loop_new
  (pointer->procedure '*
                      (dynamic-func "g_main_loop_new"
				    %libglib)
                      (list '*		;; context
                            int)))	;; is-running?

(define g_main_loop_ref
  (pointer->procedure '*
                      (dynamic-func "g_main_loop_ref"
				    %libglib)
                      (list '*)))	;; loop

(define g_main_loop_unref
  (pointer->procedure void
                      (dynamic-func "g_main_loop_unref"
				    %libglib)
                      (list '*)))	;; loop

(define g_main_loop_run
  (pointer->procedure void
                      (dynamic-func "g_main_loop_run"
				    %libglib)
                      (list '*)))	;; loop

(define g_main_loop_quit
  (pointer->procedure void
                      (dynamic-func "g_main_loop_quit"
				    %libglib)
                      (list '*)))	;; loop

(define g_main_context_new
  (pointer->procedure '*
                      (dynamic-func "g_main_context_new"
				    %libglib)
                      (list )))	;; void

(define g_main_context_default
  (pointer->procedure '*
                      (dynamic-func "g_main_context_default"
				    %libglib)
                      (list )))	;; void

(define g_timeout_source_new
  (pointer->procedure '*
                      (dynamic-func "g_timeout_source_new"
				    %libglib)
                      (list unsigned-int)))	;; interval

(define g_timeout_source_new_seconds
  (pointer->procedure '*
                      (dynamic-func "g_timeout_source_new_seconds"
				    %libglib)
                      (list unsigned-int)))	;; interval

(define g_idle_source_new
  (pointer->procedure '*
                      (dynamic-func "g_idle_source_new"
				    %libglib)
                      (list )))	;; void

(define g_source_ref
  (pointer->procedure '*
                      (dynamic-func "g_source_ref"
				    %libglib)
                      (list '*)))	;; source

(define g_source_unref
  (pointer->procedure void
                      (dynamic-func "g_source_unref"
				    %libglib)
                      (list '*)))	;; source

(define g_source_attach
  (pointer->procedure unsigned-int
                      (dynamic-func "g_source_attach"
				    %libglib)
                      (list '*		;; source
                            '*)))	;; context

(define g_source_destroy
  (pointer->procedure void
                      (dynamic-func "g_source_destroy"
				    %libglib)
                      (list '*)))	;; source

(define g_source_is_destroyed
  (pointer->procedure int
                      (dynamic-func "g_source_is_destroyed"
				    %libglib)
                      (list '*)))	;; source

(define g_source_set_priority
  (pointer->procedure void
                      (dynamic-func "g_source_set_priority"
				    %libglib)
                      (list '*		;; source
                            int)))	;; priority

(define g_source_get_priority
  (pointer->procedure int
                      (dynamic-func "g_source_get_priority"
				    %libglib)
                      (list '*)))	;; source

(define g_source_remove
  (pointer->procedure int
                      (dynamic-func "g_source_remove"
				    %libglib)
                      (list unsigned-int)))	;; tag (the source ID)

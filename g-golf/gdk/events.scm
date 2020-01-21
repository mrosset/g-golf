;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2020
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


(define-module (g-golf gdk events)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-4)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  #:use-module (g-golf support libg-golf)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support enum)
  #:use-module (g-golf support struct)
  #:use-module (g-golf support union)
  #:use-module (g-golf support flag)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi cache)
  
  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (<gdk-event>

            gdk-event-get-keycode
            gdk-event-get-keyval
            gdk-event-get-state
            gdk-event-get-time
            gdk-event-get-window
            gdk-event-get-event-type

            %gdk-event-type))


(g-export !event
          ;; !event-items

          gdk-event:type)


(define-class <gdk-event> ()
  (event #:accessor !event #:init-keyword #:event)
  #;(event-items #:accessor !event-items #:init-keyword #:event-items))

(define-method (gdk-event:type (self <gdk-event>))
  (gdk-event-get-event-type (!event self)))


;;;
;;; Gdk Low level API
;;;

(define (gdk-event-get-keycode event)
  (let ((bv (make-bytevector (sizeof uint16) 0)))
    (and (gi->scm (gdk_event_get_keycode event
                                         (bytevector->pointer bv))
                  'boolean)
         (u16vector-ref bv 0))))

(define (gdk-event-get-keyval event)
  (let ((bv (make-bytevector (sizeof unsigned-int) 0)))
    (and (gi->scm (gdk_event_get_keyval event
                                        (bytevector->pointer bv))
                  'boolean)
         (u32vector-ref bv 0))))

(define (gdk-event-get-state event)
  (let ((modifier-flags (gi-cache-ref 'flag 'gdk-modifier-type))
        (bv (make-bytevector (sizeof int) 0)))
    (and (gi->scm (gdk_event_get_state event
                                       (bytevector->pointer bv))
                  'boolean)
         (gi-integer->gflags modifier-flags
                             (s32vector-ref bv 0)))))

(define (gdk-event-get-time event)
  (gdk_event_get_time event))

(define (gdk-event-get-window event)
  (gi->scm (gdk_event_get_window event) 'pointer))

(define (gdk-event-get-event-type event)
  (enum->symbol %gdk-event-type
                (gdk_event_get_event_type event)))


;;;
;;; Gdk Bindings
;;;

(define gdk_event_get_keycode
  (pointer->procedure int
                      (dynamic-func "gdk_event_get_keycode"
				    %libgdk)
                      (list '*		;; event
                            '*)))	;; *keycode

(define gdk_event_get_keyval
  (pointer->procedure int
                      (dynamic-func "gdk_event_get_keyval"
				    %libgdk)
                      (list '*		;; event
                            '*)))	;; *keyval

(define gdk_event_get_state
  (pointer->procedure int
                      (dynamic-func "gdk_event_get_state"
				    %libgdk)
                      (list '*		;; event
                            '*)))	;; *state

(define gdk_event_get_time
  (pointer->procedure unsigned-int
                      (dynamic-func "gdk_event_get_time"
				    %libgdk)
                      (list '*)))	;; event

(define gdk_event_get_window
  (pointer->procedure '*
                      (dynamic-func "gdk_event_get_window"
				    %libgdk)
                      (list '*)))	;; event

(define gdk_event_get_event_type
  (pointer->procedure int
                      (dynamic-func "gdk_event_get_event_type"
				    %libgdk)
                      (list '*)))	;; event


;;;
;;; Types and Values
;;;

(define %gdk-event-type #f)

(eval-when (expand load eval)
  (let ((gdk-event-type
         (make <gi-enum>
           #:gi-name  "GdkEventType"
           #:enum-set '((nothing . -1)
                        (delete . 0)
                        (destroy . 1)
                        (expose . 2)
                        (motion-notify . 3)
                        (button-press . 4)
                        (2button-press . 5)
                        (double-button-press . 5)
                        (3button-press . 6)
                        (triple-button-press . 6)
                        (button-release . 7)
                        (key-press . 8)
                        (key-release . 9)
                        (enter-notify . 10)
                        (leave-notify . 11)
                        (focus-change . 12)
                        (configure . 13)
                        (map . 14)
                        (unmap . 15)
                        (property-notify . 16)
                        (selection-clear . 17)
                        (selection-request . 18)
                        (selection-notify . 19)
                        (proximity-in . 20)
                        (proximity-out . 21)
                        (drag-enter . 22)
                        (drag-leave . 23)
                        (drag-motion . 24)
                        (drag-status . 25)
                        (drop-start . 26)
                        (drop-finished . 27)
                        (client-event . 28)
                        (visibility-notify . 29)
                        (scroll . 31)
                        (window-state . 32)
                        (setting . 33)
                        (owner-change . 34)
                        (grab-broken . 35)
                        (damage . 36)
                        (touch-begin . 37)
                        (touch-update . 38)
                        (touch-end . 39)
                        (touch-cancel . 40)
                        (touchpad-swipe . 41)
                        (touchpad-pinch . 42)
                        (pad-button-press . 43)
                        (pad-button-release . 44)
                        (pad-ring . 45)
                        (pad-strip . 46)
                        (pad-group-mode . 47)
                        (event-last . 48)))))
    (set! %gdk-event-type gdk-event-type)
    (gi-cache-set! 'enum 'gdk-event-type gdk-event-type)))

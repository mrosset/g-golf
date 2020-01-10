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


(define-module (g-golf gdk event-structures)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  #:use-module (g-golf support libg-golf)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support enum)
  #:use-module (g-golf support struct)
  #:use-module (g-golf support union)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi cache)
  #:use-module (g-golf gdk events)
  #:use-module (g-golf gdk key-values)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (<gdk-event-key>
            gdk-event-key:window
            gdk-event-key:send-event
            gdk-event-key:time
            gdk-event-key:state
            gdk-event-key:keyval
            gdk-event-key:keyname
            gdk-event-key:length
            gdk-event-key:string
            gdk-event-key:hardware-keycode
            gdk-event-key:group
            gdk-event-key:is-modifier

            gdk-event-key-parse

            %gdk-event-key))


#;(g-export !items)


;;;
;;;
;;;

(define-class <gdk-event-key> (<gdk-event>))

(define-method (initialize (self <gdk-event-key>) initargs)
  (let ((event (or (get-keyword #:event initargs #f)
                  (error "Missing #:event initarg: " initargs))))
    (next-method)
    (slot-set! self 'event-items
               (gdk-event-key-parse event))))

(define-method (gdk-event-key:window (self <gdk-event-key>))
  (match (!event-items self)
    ((_ window _ _ _ _ _ _ _ _ _)
     window)))

(define-method (gdk-event-key:send-event (self <gdk-event-key>))
  (match (!event-items self)
    ((_ _ send-event _ _ _ _ _ _ _ _)
     (gi->scm send-event 'boolean))))

(define-method (gdk-event-key:time (self <gdk-event-key>))
  (match (!event-items self)
    ((_ _ _ time _ _ _ _ _ _ _)
     time)))

(define-method (gdk-event-key:state (self <gdk-event-key>))
  (match (!event-items self)
    ((_ _ _ _ state _ _ _ _ _ _)
     state)))

(define-method (gdk-event-key:keyval (self <gdk-event-key>))
  (match (!event-items self)
    ((_ _ _ _ _ keyval _ _ _ _ _)
     keyval)))

(define-method (gdk-event-key:keyname (self <gdk-event-key>))
  (gdk-keyval-name (gdk-event-key:keyval self)))

(define-method (gdk-event-key:length (self <gdk-event-key>))
  (match (!event-items self)
    ((_ _ _ _ _ _ length _ _ _ _)
     length)))

(define-method (gdk-event-key:string (self <gdk-event-key>))
  (match (!event-items self)
    ((_ _ _ _ _ _ _ str _ _ _)
     (gi->scm str 'string))))

(define-method (gdk-event-key:hardware-keycode (self <gdk-event-key>))
  (match (!event-items self)
    ((_ _ _ _ _ _ _ _ hardware-keycode _ _)
     hardware-keycode)))

(define-method (gdk-event-key:group (self <gdk-event-key>))
  (match (!event-items self)
    ((_ _ _ _ _ _ _ _ _ group _)
     group)))

(define-method (gdk-event-key:is-modifier (self <gdk-event-key>))
  (match (!event-items self)
    ((_ _ _ _ _ _ _ _ _ _ is-modifier)
     (gi->scm is-modifier 'boolean))))


;;;
;;; Low level API
;;;

(define (gdk-event-key-parse event)
  (parse-c-struct event
                  %gdk-event-key-struct))




;;;
;;; Types and Values
;;;

(define %gdk-event-key-struct
  (list int		;; type
        '*		;; window
        int8		;; send event
        uint32		;; time
        unsigned-int	;; state
        unsigned-int	;; keyval
        int		;; length
        '*		;; string
        uint16		;; hardware keycode;
        uint8		;; group
        unsigned-int))	;; is-modifier

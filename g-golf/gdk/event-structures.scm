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
  #:use-module (g-golf support enum)
  #:use-module (g-golf support struct)
  #:use-module (g-golf support union)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi cache)
  
  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (gdk-event-key-parse

            %gdk-event-key))


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

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

;;; Code:


(define-module (g-golf glib glist)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module (g-golf init)

  #:export (g-list-data
            g-list-next
            g-list-prev

            g-list-free
            g-list-length
            g-list-nth-data))


;;;
;;; Glib Low level API
;;;

(define %g-list-struct
  (list '* '* '*))

(define (g-list-parse g-list)
  (parse-c-struct g-list %g-list-struct))

(define (g-list-data g-list)
  (match (g-list-parse g-list)
    ((data _ _) data)))

(define (g-list-next g-list)
  (match (g-list-parse g-list)
    ((_ next _) next)))

(define (g-list-prev g-list)
  (match (g-list-parse g-list)
    ((_ _ prev) prev)))

(define (g-list-free g-list)
  (g_list_free g-list))

(define (g-list-length g-list)
  (g_list_length g-list))

(define (g-list-nth-data g-list n)
  (let ((foreign (g_list_nth_data g-list n)))
    (if (null-pointer? foreign)
        #f
        foreign)))


;;;
;;; Glib Bindings
;;;

(define g_list_free
  (pointer->procedure void
                      (dynamic-func "g_list_free"
				    %libglib)
                      (list '*)))

(define g_list_length
  (pointer->procedure unsigned-int
                      (dynamic-func "g_list_length"
				    %libglib)
                      (list '*)))

(define g_list_nth_data
  (pointer->procedure '*
                      (dynamic-func "g_list_nth_data"
				    %libglib)
                      (list '*
                            unsigned-int)))

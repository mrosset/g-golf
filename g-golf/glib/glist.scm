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
  #:use-module (system foreign)
  #:use-module (g-golf init)
  #:use-module (g-golf support libg-golf)

  #:export (g-list-data
            g-list-next
            
            g-list-length
            g-list-nth-data))


;;;
;;; Glib Low level API
;;;


;; from libg-golf
(define (g-list-data glist)
  (g_list_data glist))

;; from libg-golf
(define (g-list-next glist)
  (g_list_next glist))

(define (g-list-length glist)
  (g_list_length glist))

(define (g-list-nth-data glist n)
  (let ((foreign (g_list_nth_data glist n)))
    (if (null-pointer? foreign)
        #f
        foreign)))


;;;
;;; Glib Bindings
;;;

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

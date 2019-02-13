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


(define-module (g-golf support struct)
  #:use-module (oop goops)
  #:use-module (g-golf support goops)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support utils)

  #:export (<gi-struct>))


(g-export !gi-name
          !scm-name
          !field-types
          !scm-types)


(define-class <gi-struct> ()
  (gi-name #:accessor !gi-name
           #:init-keyword #:gi-name)
  (scm-name #:accessor !scm-name)
  (field-types #:accessor !field-types
               #:init-keyword #:field-types)
  (scm-types #:accessor !scm-types))


(define-method (initialize (self <gi-struct>) initargs)
  (next-method)
  (set! (!scm-name self)
        (g-name->scm-name (!gi-name self)))
  (set! (!scm-types self)
        (map g-type-tag->scm (!field-types self))))

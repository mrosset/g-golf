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


(define-module (g-golf support union)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (g-golf support goops)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support c-union)

  #:export (<gi-union>))


(g-export !gi-name
          !scm-name
          !field-names
          !field-types
          !scm-types)


(define-class <gi-union> ()
  (foreign #:accessor !foreign
           #:init-keyword #:foreign #:init-value #f)
  (size #:accessor !size
        #:init-keyword #:size #:init-value #f)
  (gi-name #:accessor !gi-name
           #:init-keyword #:gi-name)
  (scm-name #:accessor !scm-name)
  (field-names #:accessor !field-names
               #:init-keyword #:field-names)
  (field-types #:accessor !field-types
               #:init-keyword #:field-types)
  (scm-types #:accessor !scm-types))

(define-method (initialize (self <gi-union>) initargs)
  (next-method)
  (let ((gi-name (get-keyword #:gi-name initargs))
        (field-names (get-keyword #:field-names initargs))
        (field-types (get-keyword #:field-types initargs)))
    (and gi-name
         (set! (!gi-name self) gi-name)
         (set! (!scm-name self)
               (g-name->scm-name gi-name)))
    (and field-names
         (set! (!field-names self) field-names))
    (and field-types
         (set! (!scm-types self)
               (map gi-type-tag->scm field-types)))))

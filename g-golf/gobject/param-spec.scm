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


(define-module (g-golf gobject param-spec)
  #:use-module (oop goops)  
  #:use-module (g-golf support enum)
  #:use-module (g-golf support flag)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (%g-param-flags))


;;;
;;; GParamSpec
;;;

(define %g-param-flags
  (make <gi-flag>
    #:gi-name "GParamFlags"
    #:enum-set '(readable
                 writable
                 #;readwrite
                 construct
                 construct-only
                 lax-validation
                 static-name
                 #;private
                 static-nick
                 static-blurb
                 explicit-notify
                 deprecated)))

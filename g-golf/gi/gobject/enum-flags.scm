;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016
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


(define-module (g-golf gi gobject enum-flags)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (g-golf support goops)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support enum)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (<genum>))


(g-export !type-name
	  !scm-name)


(define-class <genum> (<enum>)
  (type-name #:getter !type-name #:init-keyword #:type-name)
  (scm-name #:getter !scm-name #:init-keyword #:scm-name))

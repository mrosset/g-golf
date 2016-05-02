;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016
;;;; Free Software Foundation, Inc.

;;;; This file is part of GNU Gbank

;;;; GNU Gbank is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published
;;;; by the Free Software Foundation, either version 3 of the License,
;;;; or (at your option) any later version.

;;;; GNU Gbank is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with GNU Gbank.  If not, see <http://www.gnu.org/licenses/>.
;;;;

;;; Commentary:

;;; Code:


(define-module (gbank gi enum-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (gbank support enum)
  #:use-module (gbank gi init)
  #:use-module (gbank gi utils)
  #:use-module (gbank gi types)

  #:export (gbank-ei-get-n-values))


;;;
;;; Low level API
;;;

(define (gbank-ei-get-n-values enum-info)
  (g-enum-info-get-n-values enum-info))


;;;
;;; GI Bindings
;;;

(define g-enum-info-get-n-values
  (pointer->procedure int
                      (dynamic-func "g_enum_info_get_n_values"
				    %libgirepository)
                      (list '*)))

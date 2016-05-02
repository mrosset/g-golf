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

;; I don't like the rnrs enums interface very much and besides it misses
;; an essential procedure for FFI users, which to get the symbol from an
;; integer, since all C functions return enum as int.

;; Then gbank will, in the end, extensively use goops, so I decided to
;; start here.

;;; Code:


(define-module (gbank support enum)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (gbank support goops)
  #:use-module (gbank support g-export)
  #:use-module (gbank support utils)
  
  #:export (<enum>))


(g-export !set
	  e-value
	  e-sym
	  slot-set!)


(define-class <enum> ()
  (set #:getter !set
       #:init-keyword #:set))

(define-method (initialize (self <enum>) initargs)
  (let-keywords initargs #t
		((set #f))
    (if set
	(let ((real-set (map (lambda (i)
			       (cons i (list-ref set i)))
			  (iota (length set)))))
	  (next-method self (list #:set real-set)))
	(warning "initialize <enum>"
		 "<enum> instance set can't be empty."
		 (current-output-port)))))

(define-method (e-value (self <enum>) (item <symbol>))
  (list-index (lambda (x) (eq? (cdr x) item))
	      (!set self)))

(define-method (e-sym (self <enum>) (item <integer>))
  (and (> item 0)
       (assq-ref (!set self) item)))

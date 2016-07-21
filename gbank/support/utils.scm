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

;; this file is a copy of (grip utils)
;; http://www.nongnu.org/grip/

;;; Code:


(define-module (gbank support utils)
  #:export (storage-get
	    storage-set
	    displayln
	    dimfi
	    warning
	    abort
	    and-l
	    identities))


(define storage-get #f)
(define storage-set #f)

(let ((storage (list)))
  (set! storage-get
	(lambda (key)
	  (case key
	    ((all) storage)
	    (else
	     (assq-ref storage key)))))
  (set! storage-set
	(lambda (key value)
	  (set! storage
		(assq-set! storage key value)))))


(define* (displayln msg #:optional (port #f))
  (if port
      (begin
	(display msg port)
	(newline port))
      (begin
	(display msg)
	(newline))))

(define (dimfi . items)
  ;; if the first item is a port, we use it.
  (if (port? (car items))
      (let ((p (car items)))
	(display ";; " p)
	(for-each (lambda (item)
		    (display item p) (display " " p))
	    (cdr items))
	(display "\n" p))
      (begin
	(display ";; ")
	(for-each (lambda (item)
		    (display item) (display " " ))
	    items)
	(display "\n")))
  (car (last-pair items)))

(define* (warning what msg port #:key (msg-2 #f))
  (display (string-append "Warning: " what ": " msg) port)
  (newline port)
  (when msg-2
    (display msg-2 port)
    (newline port)))

(define* (abort what msg port #:key (msg-2 #f) (code -1))
  (display (string-append "ERROR: " what ": " msg) port)
  (newline port)
  (when msg-2
    (display msg-2 port)
    (newline port))
  (exit code))

(define (and-l ll)
  (if (null? ll)
      #t
      (if (car ll)
	  (and-l (cdr ll))
	  #f)))

(define (identities . args)
  args)

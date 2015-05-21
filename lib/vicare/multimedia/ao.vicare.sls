;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/AO
;;;Contents: Libao binding backend
;;;Date: Thu May 21, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (vicare multimedia ao)
  (foreign-library "vicare-ao")
  (export

    ;; version numbers and strings
    vicare-ao-version-interface-current
    vicare-ao-version-interface-revision
    vicare-ao-version-interface-age
    vicare-ao-version

    ;; ao alpha struct
    ao-alpha-initialise
    ao-alpha-finalise
    ao-alpha?
    ao-alpha?/alive		$ao-alpha-alive?
    ao-alpha-custom-destructor	set-ao-alpha-custom-destructor!
    ao-alpha-putprop		ao-alpha-getprop
    ao-alpha-remprop		ao-alpha-property-list
    ao-alpha-hash

    ao-alpha.vicare-arguments-validation
    ao-alpha/alive.vicare-arguments-validation
    false-or-ao-alpha.vicare-arguments-validation
    false-or-ao-alpha/alive.vicare-arguments-validation

;;; --------------------------------------------------------------------
;;; still to be implemented

    )
  (import (vicare)
    (vicare multimedia ao constants)
    (prefix (vicare multimedia ao unsafe-capi) capi.)
    #;(prefix (vicare ffi) ffi.)
    (prefix (vicare ffi foreign-pointer-wrapper) ffi.)
    (vicare arguments validation)
    #;(vicare arguments general-c-buffers)
    #;(vicare language-extensions syntaxes)
    #;(prefix (vicare platform words) words.))


;;;; arguments validation

#;(define-argument-validation (fixnum who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum as argument" obj))


;;;; version functions

(define (vicare-ao-version-interface-current)
  (capi.vicare-ao-version-interface-current))

(define (vicare-ao-version-interface-revision)
  (capi.vicare-ao-version-interface-revision))

(define (vicare-ao-version-interface-age)
  (capi.vicare-ao-version-interface-age))

(define (vicare-ao-version)
  (ascii->string (capi.vicare-ao-version)))


;;;; data structures: alpha

(ffi.define-foreign-pointer-wrapper ao-alpha
  (ffi.foreign-destructor capi.ao-alpha-finalise)
  #;(ffi.foreign-destructor #f)
  (ffi.collector-struct-type #f)
  (ffi.collected-struct-type ao-beta))

(module ()
  (set-rtd-printer! (type-descriptor ao-alpha)
    (lambda (S port sub-printer)
      (define-inline (%display thing)
	(display thing port))
      (define-inline (%write thing)
	(write thing port))
      (%display "#[ao-alpha")
      (%display " pointer=")	(%display ($ao-alpha-pointer  S))
      (%display "]"))))

;;; --------------------------------------------------------------------

(define (ao-alpha-initialise)
  (define who 'ao-alpha-initialise)
  (cond ((capi.ao-alpha-initialise)
	 => (lambda (rv)
	      (make-ao-alpha/owner rv)))
	(else
	 (error who "unable to create alpha object"))))

(define (ao-alpha-finalise alpha)
  (define who 'ao-alpha-finalise)
  (with-arguments-validation (who)
      ((ao-alpha		alpha))
    ($ao-alpha-finalise alpha)))


;;;; data structures: beta

(ffi.define-foreign-pointer-wrapper ao-beta
  (ffi.foreign-destructor #f)
  (ffi.collector-struct-type ao-alpha))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put 'ffi.define-foreign-pointer-wrapper 'scheme-indent-function 1)
;; End:

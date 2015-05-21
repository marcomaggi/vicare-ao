;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/AO
;;;Contents: unsafe interface to the C language API
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
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare multimedia ao unsafe-capi)
  (export

    ;; version functions
    vicare-ao-version-interface-current
    vicare-ao-version-interface-revision
    vicare-ao-version-interface-age
    vicare-ao-version

    ;; ao alpha
    ao-alpha-initialise
    ao-alpha-finalise

;;; --------------------------------------------------------------------
;;; still to be implemented

    )
  (import (vicare))


;;;; version functions

(define-syntax-rule (vicare-ao-version-interface-current)
  (foreign-call "ikrt_ao_version_interface_current"))

(define-syntax-rule (vicare-ao-version-interface-revision)
  (foreign-call "ikrt_ao_version_interface_revision"))

(define-syntax-rule (vicare-ao-version-interface-age)
  (foreign-call "ikrt_ao_version_interface_age"))

(define-syntax-rule (vicare-ao-version)
  (foreign-call "ikrt_ao_version"))


;;;; ao alpha struct

(define-syntax-rule (ao-alpha-initialise)
  (foreign-call "ikrt_ao_alpha_initialise"))

(define-syntax-rule (ao-alpha-finalise alpha)
  (foreign-call "ikrt_ao_alpha_finalise" alpha))


;;;; still to be implemented

#;(define-syntax-rule (vicare-ao)
  (foreign-call "ikrt_ao"))


;;;; done

#| end of library |# )

;;; end of file

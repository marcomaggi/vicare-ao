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

    ;; initialisation and shutdown unsafe C API
    ao-initialize
    ao-shutdown

    ;; device options
    ao-append-global-option
    ao-append-option
    ao-free-options
    ao-option->alist

    ;; device setup/playback/teardown unsafe C API
    ao-open-live
    ao-open-file
    ao-play
    ao-close

    ;; driver information unsafe C API
    ao-driver-id
    ao-default-driver-id
    ao-driver-info
    ao-driver-info-list
    ao-file-extension

    ;; miscellaneous unsafe C API
    ao-is-big-endian)
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


;;;; initialisation and shutdown unsafe C API

(define-syntax-rule (ao-initialize)
  (foreign-call "ikrt_ao_initialize"))

(define-syntax-rule (ao-shutdown)
  (foreign-call "ikrt_ao_shutdown"))


;;;; device options unsafe C API

(define-syntax-rule (ao-append-global-option key val)
  (foreign-call "ikrt_ao_append_global_option" key val))

;;; --------------------------------------------------------------------

(define-syntax-rule (ao-append-option false/option key val)
  (foreign-call "ikrt_ao_append_option" false/option key val))

(define-syntax-rule (ao-free-options option)
  (foreign-call "ikrt_ao_free_options" option))

(define-syntax-rule (ao-option->alist option)
  (foreign-call "ikrt_ao_option_keys_and_vals" option))


;;;; device playback and teardown  unsafe C API

(define-syntax-rule (ao-open-live driver-id sample-format options)
  (foreign-call "ikrt_ao_open_live" driver-id sample-format options))

(define-syntax-rule (ao-open-file)
  (foreign-call "ikrt_ao_open_file"))

(define-syntax-rule (ao-play device output-samples)
  (foreign-call "ikrt_ao_play" device output-samples))

(define-syntax-rule (ao-close device)
  (foreign-call "ikrt_ao_close" device))


;;;; driver information unsafe C API

(define-syntax-rule (ao-driver-id short-name)
  (foreign-call "ikrt_ao_driver_id" short-name))

(define-syntax-rule (ao-default-driver-id)
  (foreign-call "ikrt_ao_default_driver_id"))

(define-syntax-rule (ao-driver-info id ao-info-std)
  (foreign-call "ikrt_ao_driver_info" id ao-info-std))

(define-syntax-rule (ao-driver-info-list ao-info-std)
  (foreign-call "ikrt_ao_driver_info_list" ao-info-std))

(define-syntax-rule (ao-file-extension id)
  (foreign-call "ikrt_ao_file_extension" id))


;;;; miscellaneous unsafe C API

(define-syntax-rule (ao-is-big-endian)
  (foreign-call "ikrt_ao_is_big_endian"))


;;;; still to be implemented

#;(define-syntax-rule (vicare-ao)
  (foreign-call "ikrt_ao"))


;;;; done

#| end of library |# )

;;; end of file

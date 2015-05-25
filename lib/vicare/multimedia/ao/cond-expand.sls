;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/AO
;;;Contents: feature-based conditional expansion
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
(library (vicare multimedia ao cond-expand)
  (export vicare-ao-features)
  (import (only (vicare language-extensions cond-expand helpers)
		define-cond-expand-identifiers-helper)
    (vicare multimedia ao features)
    (vicare multimedia ao))


(define-cond-expand-identifiers-helper vicare-ao-features

  ;; cond-expand clauses initialisation and shutdown
  (ao-initialize			HAVE_AO_INITIALIZE)
  (ao-initialise			HAVE_AO_INITIALIZE)
  (ao-shutdown				HAVE_AO_SHUTDOWN)

  ;; cond-expand clauses device setup/playback/teardown
  (ao-append-global-option		HAVE_AO_APPEND_GLOBAL_OPTION)
  (ao-append-option			HAVE_AO_APPEND_OPTION)
  (ao-free-options			HAVE_AO_FREE_OPTIONS)
  (ao-open-live				HAVE_AO_OPEN_LIVE)
  (ao-open-file				HAVE_AO_OPEN_FILE)
  (ao-play				HAVE_AO_PLAY)
  (ao-close				HAVE_AO_CLOSE)

  ;; cond-expand clauses driver information
  (ao-driver-id				HAVE_AO_DRIVER_ID)
  (ao-default-driver-id			HAVE_AO_DEFAULT_DRIVER_ID)
  (ao-driver-info			HAVE_AO_DRIVER_INFO)
  (ao-driver-info-list			HAVE_AO_DRIVER_INFO_LIST)
  (ao-file-extension			HAVE_AO_FILE_EXTENSION)

  ;; cond-expand clauses miscellaneous
  (ao-is-big-endian			HAVE_AO_IS_BIG_ENDIAN))


;;;; done

#| end of library |# )

;;; end of file

;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/AO
;;;Contents: tests for Ao bindings
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


#!r6rs
(import (vicare)
  (vicare multimedia ao)
  (vicare multimedia ao constants)
  (vicare arguments validation)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare Libao bindings\n")


;;;; helpers



(parametrise ((check-test-name	'version))

  (check
      (fixnum? (vicare-ao-version-interface-current))
    => #t)

  (check
      (fixnum? (vicare-ao-version-interface-revision))
    => #t)

  (check
      (fixnum? (vicare-ao-version-interface-age))
    => #t)

  (check
      (string? (vicare-ao-version))
    => #t)

  #t)


(parametrise ((check-test-name		'struct-alpha)
	      (struct-guardian-logger	#t))

  (define who 'test)

  (check	;this will be garbage collected
      (let ((voice (ao-alpha-initialise)))
;;;(debug-print voice)
	(ao-alpha? voice))
    => #t)

  (check
      (ao-alpha?/alive (ao-alpha-initialise))
    => #t)

  (check	;single finalisation
      (let ((voice (ao-alpha-initialise)))
  	(ao-alpha-finalise voice))
    => #f)

  (check	;double finalisation
      (let ((voice (ao-alpha-initialise)))
  	(ao-alpha-finalise voice)
  	(ao-alpha-finalise voice))
    => #f)

  (check	;alive predicate after finalisation
      (let ((voice (ao-alpha-initialise)))
  	(ao-alpha-finalise voice)
  	(ao-alpha?/alive voice))
    => #f)

;;; --------------------------------------------------------------------
;;; destructor

  (check
      (with-result
       (let ((voice (ao-alpha-initialise)))
	 (set-ao-alpha-custom-destructor! voice (lambda (voice)
							(add-result 123)))
	 (ao-alpha-finalise voice)))
    => '(#f (123)))

;;; --------------------------------------------------------------------
;;; hash

  (check-for-true
   (integer? (ao-alpha-hash (ao-alpha-initialise))))

  (check
      (let ((A (ao-alpha-initialise))
	    (B (ao-alpha-initialise))
	    (T (make-hashtable ao-alpha-hash eq?)))
	(hashtable-set! T A 1)
	(hashtable-set! T B 2)
	(list (hashtable-ref T A #f)
	      (hashtable-ref T B #f)))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; properties

  (check
      (let ((S (ao-alpha-initialise)))
	(ao-alpha-property-list S))
    => '())

  (check
      (let ((S (ao-alpha-initialise)))
	(ao-alpha-putprop S 'ciao 'salut)
	(ao-alpha-getprop S 'ciao))
    => 'salut)

  (check
      (let ((S (ao-alpha-initialise)))
	(ao-alpha-getprop S 'ciao))
    => #f)

  (check
      (let ((S (ao-alpha-initialise)))
	(ao-alpha-putprop S 'ciao 'salut)
	(ao-alpha-remprop S 'ciao)
	(ao-alpha-getprop S 'ciao))
    => #f)

  (check
      (let ((S (ao-alpha-initialise)))
	(ao-alpha-putprop S 'ciao 'salut)
	(ao-alpha-putprop S 'hello 'ohayo)
	(list (ao-alpha-getprop S 'ciao)
	      (ao-alpha-getprop S 'hello)))
    => '(salut ohayo))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check-for-true
   (let ((S (ao-alpha-initialise)))
     (with-arguments-validation (who)
	 ((ao-alpha	S))
       #t)))

  (check-for-true
   (let ((S (ao-alpha-initialise)))
     (ao-alpha-finalise S)
     (with-arguments-validation (who)
	 ((ao-alpha	S))
       #t)))

  (check-for-true
   (let ((S (ao-alpha-initialise)))
     (with-arguments-validation (who)
	 ((ao-alpha/alive	S))
       #t)))

;;;

  (check-for-procedure-argument-violation
      (let ((S 123))
	(with-arguments-validation (who)
	    ((ao-alpha	S))
	  #t))
    => (list who '(123)))

  (check-for-procedure-argument-violation
      (let ((S 123))
	(with-arguments-validation (who)
	    ((ao-alpha/alive	S))
	  #t))
    => (list who '(123)))

  (let ((S (ao-alpha-initialise)))
    (check-for-procedure-argument-violation
	(begin
	  (ao-alpha-finalise S)
	  (with-arguments-validation (who)
	      ((ao-alpha/alive	S))
	    #t))
      => (list who (list S))))

  (collect))


;;;; done

(check-report)

;;; end of file

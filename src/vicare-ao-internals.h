/*
  Part of: Vicare/AO
  Contents: internal header file
  Date: Thu May 21, 2015

  Abstract

	Internal header file.

  Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either version  3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See the  GNU
  General Public License for more details.

  You should  have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef VICARE_AO_INTERNALS_H
#define VICARE_AO_INTERNALS_H 1


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <vicare.h>
#include <ao/ao.h>


/** --------------------------------------------------------------------
 ** Handling of Scheme objects.
 ** ----------------------------------------------------------------- */

/* Accessors for the fields of the Scheme structure "ao-option". */
#define IK_AO_OPTION_POINTER(OPTION)	IK_FIELD((OPTION),0)
#define IK_AO_OPTION_OWNER(OPTION)	IK_FIELD((OPTION),1)
#define IK_AO_OPTION(OPTION)	\
  IK_POINTER_DATA_VOIDP(IK_AO_OPTION_POINTER(OPTION))

/* Accessors for the fields of the Scheme structure "ao-device". */
#define IK_AO_DEVICE_POINTER(DEVICE)	IK_FIELD((DEVICE),0)
#define IK_AO_DEVICE_OWNER(DEVICE)	IK_FIELD((DEVICE),1)
#define IK_AO_DEVICE(DEVICE)	\
  IK_POINTER_DATA_VOIDP(IK_AO_DEVICE_POINTER(DEVICE))

/* Accessors for the fields of the Scheme record "ao-sample-format". */
#define IK_AO_SAMPLE_FORMAT_BITS(SAMPLE_FORMAT)		IK_FIELD((SAMPLE_FORMAT),0)
#define IK_AO_SAMPLE_FORMAT_RATE(SAMPLE_FORMAT)		IK_FIELD((SAMPLE_FORMAT),1)
#define IK_AO_SAMPLE_FORMAT_CHANNELS(SAMPLE_FORMAT)	IK_FIELD((SAMPLE_FORMAT),2)
#define IK_AO_SAMPLE_FORMAT_BYTE_FORMAT(SAMPLE_FORMAT)	IK_FIELD((SAMPLE_FORMAT),3)
#define IK_AO_SAMPLE_FORMAT_MATRIX(SAMPLE_FORMAT)	IK_FIELD((SAMPLE_FORMAT),4)


/** --------------------------------------------------------------------
 ** Support for missing functions.
 ** ----------------------------------------------------------------- */

static IK_UNUSED void
feature_failure_ (const char * funcname)
{
  ik_abort("called unavailable Ao specific function, %s\n", funcname);
}

#define feature_failure(FN)     { feature_failure_(FN); return IK_VOID; }


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#endif /* VICARE_AO_INTERNALS_H */

/* end of file */

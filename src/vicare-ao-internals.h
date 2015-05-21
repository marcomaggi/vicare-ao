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

/* Accessors for the fields of the Scheme structure "ao_alpha". */
#define IK_AO_ALPHA_POINTER(ALPHA)	IK_FIELD((ALPHA),0)
#define IK_AO_ALPHA_OWNER(ALPHA)		IK_FIELD((ALPHA),1)
#define IK_AO_ALPHA(ALPHA)	\
  IK_POINTER_DATA_VOIDP(IK_AO_ALPHA_POINTER(ALPHA))


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

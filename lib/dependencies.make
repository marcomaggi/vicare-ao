## dependencies.make --
#
# Automatically built.

EXTRA_DIST +=  \
	lib/vicare/multimedia/ao/constants.vicare.sls.in

lib/vicare/multimedia/ao.fasl: \
		lib/vicare/multimedia/ao.vicare.sls \
		lib/vicare/multimedia/ao/constants.fasl \
		lib/vicare/multimedia/ao/unsafe-capi.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_multimedia_ao_fasldir = $(bundledlibsdir)/vicare/multimedia
lib_vicare_multimedia_ao_vicare_slsdir  = $(bundledlibsdir)/vicare/multimedia
nodist_lib_vicare_multimedia_ao_fasl_DATA = lib/vicare/multimedia/ao.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_multimedia_ao_vicare_sls_DATA = lib/vicare/multimedia/ao.vicare.sls
endif
EXTRA_DIST += lib/vicare/multimedia/ao.vicare.sls
CLEANFILES += lib/vicare/multimedia/ao.fasl

lib/vicare/multimedia/ao/constants.fasl: \
		lib/vicare/multimedia/ao/constants.vicare.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_multimedia_ao_constants_fasldir = $(bundledlibsdir)/vicare/multimedia/ao
lib_vicare_multimedia_ao_constants_vicare_slsdir  = $(bundledlibsdir)/vicare/multimedia/ao
nodist_lib_vicare_multimedia_ao_constants_fasl_DATA = lib/vicare/multimedia/ao/constants.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_multimedia_ao_constants_vicare_sls_DATA = lib/vicare/multimedia/ao/constants.vicare.sls
endif
CLEANFILES += lib/vicare/multimedia/ao/constants.fasl

lib/vicare/multimedia/ao/unsafe-capi.fasl: \
		lib/vicare/multimedia/ao/unsafe-capi.vicare.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_multimedia_ao_unsafe_capi_fasldir = $(bundledlibsdir)/vicare/multimedia/ao
lib_vicare_multimedia_ao_unsafe_capi_vicare_slsdir  = $(bundledlibsdir)/vicare/multimedia/ao
nodist_lib_vicare_multimedia_ao_unsafe_capi_fasl_DATA = lib/vicare/multimedia/ao/unsafe-capi.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_multimedia_ao_unsafe_capi_vicare_sls_DATA = lib/vicare/multimedia/ao/unsafe-capi.vicare.sls
endif
EXTRA_DIST += lib/vicare/multimedia/ao/unsafe-capi.vicare.sls
CLEANFILES += lib/vicare/multimedia/ao/unsafe-capi.fasl

lib/vicare/multimedia/ao/features.fasl: \
		lib/vicare/multimedia/ao/features.vicare.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_multimedia_ao_features_fasldir = $(bundledlibsdir)/vicare/multimedia/ao
lib_vicare_multimedia_ao_features_vicare_slsdir  = $(bundledlibsdir)/vicare/multimedia/ao
nodist_lib_vicare_multimedia_ao_features_fasl_DATA = lib/vicare/multimedia/ao/features.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_multimedia_ao_features_vicare_sls_DATA = lib/vicare/multimedia/ao/features.vicare.sls
endif
CLEANFILES += lib/vicare/multimedia/ao/features.fasl

lib/vicare/multimedia/ao/cond-expand.fasl: \
		lib/vicare/multimedia/ao/cond-expand.sls \
		lib/vicare/multimedia/ao/features.fasl \
		lib/vicare/multimedia/ao.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_multimedia_ao_cond_expand_fasldir = $(bundledlibsdir)/vicare/multimedia/ao
lib_vicare_multimedia_ao_cond_expand_slsdir  = $(bundledlibsdir)/vicare/multimedia/ao
nodist_lib_vicare_multimedia_ao_cond_expand_fasl_DATA = lib/vicare/multimedia/ao/cond-expand.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_multimedia_ao_cond_expand_sls_DATA = lib/vicare/multimedia/ao/cond-expand.sls
endif
EXTRA_DIST += lib/vicare/multimedia/ao/cond-expand.sls
CLEANFILES += lib/vicare/multimedia/ao/cond-expand.fasl


### end of file
# Local Variables:
# mode: makefile-automake
# End:

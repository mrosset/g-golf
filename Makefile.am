
####
#### Copyright (C) 2016
#### Free Software Foundation, Inc.

#### This file is part of GNU Gbank.

#### GNU Gbank is free software: you can redistribute it and/or
#### modify it under the terms of the GNU General Public License as
#### published by the Free Software Foundation, either version 3 of
#### the License, or (at your option) any later version.

#### GNU Gbank is distributed in the hope that it will be useful, but
#### WITHOUT ANY WARRANTY; without even the implied warranty of
#### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#### General Public License for more details.

#### You should have received a copy of the GNU General Public License
#### along with GNU Gbank.  If not, see
#### <http://www.gnu.org/licenses/>.
####


include $(top_srcdir)/am/guile.mk

moddir=$(prefix)/share
godir=$(gccache)

pkgconfigdir=$(libdir)/pkgconfig
pkgconfig_DATA=meta/gbank-1.0.pc

SOURCES = \
	gbank/support/modules.scm	\
	gbank/support/goops.scm		\
	gbank/support/g-export.scm	\
	gbank/support/utils.scm		\
	gbank/support/enum.scm		\
	gbank/support.scm		\
	gbank/gi/init.scm		\
	gbank/gi/utils.scm		\
	gbank/gi/types.scm		\
	gbank/gi/repository.scm		\
	gbank/gi/base-info.scm		\
	gbank/gi/callable-info.scm	\
	gbank/gi/enum-info.scm		\
	gbank/gi/arg-info.scm		\
	gbank/gi/type-info.scm		\
	gbank/gi.scm

# NOCOMP_SOURCES= \
# 	$(wildcard gbank/glade/*)	\
# 	$(wildcard gbank/icons/*)	\
# 	$(wildcard gbank/latex/*)	\
# 	$(wildcard gbank/gtkrc/*)	\
# 	$(shell find $(top_srcdir)/gbank/pof -type f)

EXTRA_DIST = \
	AUTHORS				\
	ChangeLog			\
	COPYING				\
	INSTALL				\
	LICENSE				\
	NEWS				\
	README				\
	env.in				\
	meta/guild.in			\
	meta/gbank-1.0.pc.in		\
	$(wildcard org/*)		\
	$(wildcard am/*)		\
	$(wildcard m4/*)		\
	$(SOURCES)
# 	$(NOCOMP_SOURCES)

# bin_SCRIPTS = gbank/gbank

CLEANFILES = \
	meta/gbank-1.0.pc		\
	$(GOBJECTS)
# 	$(bin_SCRIPTS)

ETAGS_ARGS = \
	gbank				\
	$(SOURCES)

SUBDIRS = \
	doc

distclean-local:
	rm -f Makefile.in
	rm -f aclocal.m4
	rm -rf autom4te.cache
	rm -rf build-aux
	rm -f configure
	rm -f *~
	rm -f gbank-*.tar.gz
	rm -f gbank-*.tar.gz.sig
	rm -rf doc/gbank
	rm -f doc/*.info
	rm -f doc/*.pdf
	rm -f doc/*~
	rm -f doc/*.aux doc/*.cp doc/*.cps doc/*.fn doc/*.fns doc/*.ky
	rm -f doc/*.log doc/*.pg doc/*.toc doc/*.tp doc/*.vr doc/*.vrs
	rm -f doc/Makefile.in
	rm -f doc/stamp-vti
	rm -f gbank/*~
	rm -f org/*~

# * .PHONY: gen-pof:

# Makefile.am ends here

#
#  Makefile --
#
#     Makefile for copying kernels 
#
#  Copyright (c) 2006 Dan Gudmundsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.5 2006/01/27 15:17:56 dgud Exp $
#

DIR = ../../../plugins/pbr/kernels

FILES = qbvh_kernel.cl

TARGETFILES = $(FILES:%=$(DIR)/%)

all: $(TARGETFILES) 

$(DIR)/%: ./%
	install -d $(DIR)
	install $< $@

clean:
	rm -f $(TARGETFILES)
	rm -f core


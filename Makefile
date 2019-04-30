# -*- Mode: makefile; -*-

CCL_PATH := ccl64
SBCL = sbcl --noinform
LW = "C:/Program Files (x86)/LispWorks/lispworks-7-1-0-x86-win32.exe"

.PHONY: all
.DEFAULT_GOAL := all

ifeq ($(OS),Windows_NT)
	target = lw
else
	target = sbcl
endif

all: $(target)
	@echo "Done"

ccl: 
	$(CCL_PATH) --load build.lisp --batch

lw:
	$(LW) -build src/build-ui.lisp

sbcl:
	$(SBCL) --disable-debugger --load src/build-cmdline.lisp

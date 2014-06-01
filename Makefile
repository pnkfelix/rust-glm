RUSTC     ?= rustc
RUSTFLAGS ?= -O
OUTDIR    ?= ./build

BINDIR := $(OUTDIR)/bin
LIBDIR := $(OUTDIR)/lib
SRCDIR := .

LIBFILE:=$(shell $(RUSTC) --crate-file-name $(SRCDIR)/lib.rs)

ALL_SRC_RS=$(wildcard $(SRCDIR)/src/*.rs)
LIB_RS=$(SRCDIR)/lib.rs

default: $(LIBDIR)/$(LIBFILE) run-tests bench-tests
.PHONY: default run-tests


$(LIBDIR)/$(LIBFILE): $(LIB_RS) $(ALL_SRC_RS) Makefile | $(LIBDIR)
	$(RUSTC) --out-dir '$(LIBDIR)' $(RUSTFLAGS) $<

$(BINDIR):
	mkdir -p $@

$(LIBDIR):
	mkdir -p $@

run-tests: $(BINDIR)/tests
	./$<

bench-tests: $(BINDIR)/tests
	./$< --bench

$(BINDIR)/tests: $(LIB_RS) $(ALL_SRC_RS) | $(BINDIR)
	$(RUSTC) -o $@ --test $(RUSTFLAGS) $<

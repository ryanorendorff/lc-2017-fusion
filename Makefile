# This is to make the stupid expansion in clean work.
SHELL=/bin/bash

DOC=fusion
HANDOUT=$(DOC)_handout

HCOMP=stack ghc -- -O3

#######################################################################
#                               Targets                               #
#######################################################################
.PHONY: pdf fast handout clean

pdf: $(DOC).pdf

allpdf: handout pdf

fast:
	pdflatex $(DOC).tex

handout:
	pdflatex -jobname=fusion_handout "\PassOptionsToClass{handout}{beamer}\input{$(DOC)}"
	bibtex $(HANDOUT)
	pdflatex -jobname=$(HANDOUT) "\PassOptionsToClass{handout}{beamer}\input{$(DOC)}" 1>/dev/null 2>/dev/null
	pdflatex -jobname=$(HANDOUT) "\PassOptionsToClass{handout}{beamer}\input{$(DOC)}" 1>/dev/null 2>/dev/null

$(DOC).pdf: $(DOC).tex
	pdflatex $(DOC).tex
	bibtex $(DOC)
	pdflatex $(DOC).tex 1>/dev/null 2>/dev/null
	pdflatex $(DOC).tex 1>/dev/null 2>/dev/null

$(DOC).tex: $(DOC).lhs
	lhs2TeX --poly -o $(DOC).tex $(DOC).lhs

bench:
	$(HCOMP) bench.hs

weighTest:
	$(HCOMP) weighTest.hs

clean:
	-rm -r $(DOC).{tex,aux,log,nav,out,ptb,snm,toc}

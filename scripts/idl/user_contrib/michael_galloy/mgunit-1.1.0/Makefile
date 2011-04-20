VERSION=1.1
REVISION=r`svn info | sed -n 's/Revision: \(.*\)/\1/p'`
RELEASE="$(VERSION)-$(REVISION)"
IDL=idl

.PHONY: all doc clean dist srcdist
 
all:
	cd src; make

doc:
	idl -e mgunit_build_docs

clean:
	rm -rf api-docs
	cd src; make clean

# need to: blow away old dist, blow away api-docs, generate api-docs
dist:
	rm -rf mgunit-$(RELEASE)
	rm -rf api-docs/

	$(IDL) -e mgunit_build_docs
	
	$(IDL) -IDL_STARTUP "" mgunit_build

	mkdir mgunit-$(RELEASE)/

	mv mgunit.sav mgunit-$(RELEASE)/
	svn export docs mgunit-$(RELEASE)/docs/
	cp -r api-docs mgunit-$(RELEASE)/
	cp src/error_is_fail.pro mgunit-$(RELEASE)/
	cp src/error_is_pass.pro mgunit-$(RELEASE)/
	cp src/style.css mgunit-$(RELEASE)/
	cp src/mgunit-templates.xml mgunit-$(RELEASE)/
	cp COPYING mgunit-$(RELEASE)/
	cp RELEASE mgunit-$(RELEASE)/	
	
	zip -r mgunit-$(RELEASE).zip mgunit-$(RELEASE)/*
	rm -rf mgunit-$(RELEASE)

srcdist:
	rm -rf mgunit-src-$(RELEASE)
	rm -rf api-docs/

	$(IDL) -e mgunit_build_docs

	mkdir mgunit-src-$(RELEASE)/

	svn export docs mgunit-src-$(RELEASE)/docs/
	cp -r api-docs mgunit-src-$(RELEASE)/
	cp src/*.pro mgunit-src-$(RELEASE)/
	cp src/style.css mgunit-src-$(RELEASE)/
	cp src/dist_tools/mg_src_root.pro mgunit-src-$(RELEASE)/
	cp src/dist_tools/mg_options__define.pro mgunit-src-$(RELEASE)/
	cp src/dist_tools/collection/mgcohashtable__define.pro mgunit-src-$(RELEASE)/
	cp src/dist_tools/collection/mgcoarraylist__define.pro mgunit-src-$(RELEASE)/
	cp src/dist_tools/collection/mgcoabstractlist__define.pro mgunit-src-$(RELEASE)/
	cp src/cmdline_tools/mg_ansicode.pro mgunit-src-$(RELEASE)/
	cp src/mgunit-templates.xml mgunit-src-$(RELEASE)/
	cp COPYING mgunit-src-$(RELEASE)/
	cp RELEASE mgunit-src-$(RELEASE)/	
	
	zip -r mgunit-src-$(RELEASE).zip mgunit-src-$(RELEASE)/*
	rm -rf mgunit-src-$(RELEASE)
	
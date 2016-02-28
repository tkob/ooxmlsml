TESTS = t/rels.t t/Content_Types.t t/contenttype.t t/iri.t t/partname.t

main: shared-commonSimpleTypes.sml sml.sml

check: opc.sml spreadsheet.sml $(TESTS)
	prove -f --exec ./t/do-test

shared-commonSimpleTypes.sml: xsd2sml.xsl xsd
	xsltproc -o $@ xsd2sml.xsl xsd/shared-commonSimpleTypes.xsd

sml.sml: xsd2sml.xsl xsd
	xsltproc -o $@ xsd2sml.xsl xsd/sml.xsd

%.t: %.t.in
	autom4te -l m4sugar -o $@ $<

clean:
	rm -f shared-commonSimpleTypes.sml sml.sml

.PHONY: check clean

TESTS = t/rels.t t/Content_Types.t t/contenttype.t t/iri.t t/partname.t

main: shared-commonSimpleTypes.sml sml.sml

check: opc.sml spreadsheet.sml $(TESTS)
	prove -f --exec ./t/do-test

shared-commonSimpleTypes.sml: xsd2sml.xsl xsd
	xsltproc -o $@ xsd2sml.xsl xsd/shared-commonSimpleTypes.xsd

sml.sml: xsd2sml.xsl xsd
	xsltproc -o $@ xsd2sml.xsl xsd/sml.xsd

part4.zip:
	wget -O part4.zip 'http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-376,%20Fourth%20Edition,%20Part%204%20-%20Transitional%20Migration%20Features.zip'

OfficeOpenXML-XMLSchema-Transitional.zip: part4.zip
	unzip part4.zip OfficeOpenXML-XMLSchema-Transitional.zip
	touch OfficeOpenXML-XMLSchema-Transitional.zip

xsd: OfficeOpenXML-XMLSchema-Transitional.zip
	unzip -u -d xsd OfficeOpenXML-XMLSchema-Transitional.zip
	touch xsd

%.t: %.t.in
	autom4te -l m4sugar -o $@ $<

clean:
	rm -f shared-commonSimpleTypes.sml sml.sml

.PHONY: check clean

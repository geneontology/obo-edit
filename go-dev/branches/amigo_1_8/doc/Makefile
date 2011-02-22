# HTML, PDF etc generated for asciidoc file using asciidoc.py
#
# See: http://www.methods.co.nz/asciidoc/

# there are two options for html; plain, and with CSS embedded
#
####HTML_BACKEND = html
####HTML_BACKEND = css-embedded
HTML_BACKEND = xhtml11

SPEC = obo_format_spec
MAPPING = mapping-obo-to-owl
OBJECTS = $(SPEC) $(MAPPING)

all: all_html all_pdf

all_html: $(patsubst %,%.html,$(OBJECTS))
all_pdf: $(patsubst %,%.pdf,$(OBJECTS))


extra: all $(SPEC).pdf

%.html: %.txt
	asciidoc.py -b $(HTML_BACKEND) $<

# docbook xml format can be converted to pdf, rtf, ps, ...
%.xml: %.txt
	asciidoc.py -b docbook $<
.PRECIOUS: %.xml

# for this part you need xsltproc
ASCIIDOC_HOME = $(HOME)/asciidoc

# generate xsl formatting objects
%.fo: %.xml
	xsltproc $(ASCIIDOC_HOME)/doc/fo.xsl $< > $@
.PRECIOUS: %.fo

# you need apache fop for this
%.pdf: %.fo
	$(FOP_HOME)/fop.sh $< $@

scp: all
	scp mapping-obo-to-owl.* $(SERVER_BASE)/htdocs/dev/doc

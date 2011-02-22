SRCDB=fb
MYSQLDB=go_latest_lite

load_gaf.txt:
	gzip -dc ~/cvs/go/gene-associations/gene_association.$(SRCDB).gz | grep -v '^\!' > $@

import: load_gaf.txt:
	mysqlimport -L $(MYSQLDB) $<


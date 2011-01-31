WWW_LIVE = $(HOME)/cvs/go/www

godoc.html: go-pg.sql
	dropdb godoc && createdb godoc && psql godoc < $< && postgresql_autodoc -d godoc -l autodoc

godocv.html: go-pg-with-views.sql
	dropdb godocv && createdb godocv && psql godocv < $< && postgresql_autodoc -d godocv -l autodoc

go-pg.sql: go-tables-FULL.sql
	./create_postgres_schema.pl $< > $@.tmp && mv $@.tmp $@

go-pg-with-views.sql: go-views-FULL.sql
	./create_postgres_schema.pl go-tables-FULL.sql $< > $@.tmp && mv $@.tmp $@

cp:
	cp godoc.html $(WWW_LIVE)/GO.database.schema.shtml
	cp godocv.html $(WWW_LIVE)/GO.database.schema-with-views.shtml

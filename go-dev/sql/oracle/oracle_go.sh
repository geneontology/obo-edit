#!/bin/sh
# Script for loading GO into an oracle instance
# Author: Frank Schacherer <bioinformatics@schacherer.de>
# $Logfile: $
# $Date: 2002/08/08 19:05:34 $
# $Revision: 1.1 $
# $History: $

function usage() {
    echo "Usage: `basename $0` sql-dump-data-file"
    echo "Can not read file >>$1<<. Exiting"
    exit 1
}

[ -r "$1" ] || usage $1
if [ -z "$GO_ROOT" ]
then
    echo "You should set the environment variable GO_ROOT"
    echo "to the sql directory of your go-dev distribution"
    exit 1
fi

echo "Loading GO..."

datafile="$1" #the SQL dump from mysql
gen_table_file="$GO_ROOT/go-tables-FULL.sql"
clean_db_file="go-drop-all.sql"
tempdir="loaded-`date +\"%Y-%m-%d\"`"

dbuser="${GO_DATABASE_USER:-go}"
dbauth="${GO_DATABASE_AUTH:-go}"
dbhost="${GO_DATABASE_HOST:-go}"
dbconnect="$dbuser/$dbauth@$dbhost"

tablelist="term species dbxref gene_product association term_synonym \
term_definition term_dbxref term2term seq seq_property seq_dbxref \
graph_path graph_path2term gene_product_synonym gene_product_seq \
gene_product_property gene_product_count evidence"

if [ ! -d "$tempdir" ]
then
    mkdir "$tempdir" || exit 1
fi

#TODO: FTP Download of datafile
# unzip $datafile.gz

sqlplus $dbconnect <$clean_db_file
./compiledb -t oracle $gen_table_file | sqlplus $dbconnect

for table in $tablelist 
  do
  echo "$table.."
  grep -w "^INSERT INTO \\b$table\\b" $datafile | \
      perl -pe "s/''/' '/g;    #Oracle treats empty string as NULL" |\
      perl -pe "s/'/\"/g;      #field delimiters" |\
      perl -pe "s/\\\\\"/'/g;  #internal apostrophes" |\
      perl -pe "s/NULL/\"\"/gi #NULL would be read as text" |\
      perl -pe "s/^.*?\\(//;   #strip select part" |\
      perl -pe "s/\\);$//;     #strip closing stuff" \
      > "$tempdir"/$table.data
   head "$tempdir/$table.data"

  cat >"$tempdir"/$table.ctrl <<EOF
load data
infile '$tempdir/$table.data'
badfile '$tempdir/$table.bad'
into table $table
fields terminated by ',' optionally enclosed by '"'
EOF
  ./get_table_cols.pl $table tables_oracle >> "$tempdir/$table.ctrl"

  sqlldr userid=$dbconnect errors=666 control="$tempdir/$table.ctrl" log="$tempdir/$table.log"

  cat "$tempdir/$table.log"
  echo "Lines in load file: `wc -l \"$tempdir\"/$table.data`"
  if [ ! -e "$tempdir/$table.bad" ] 
      then 
      echo "$table: ok" >> "$tempdir/final.log"
      else
      echo "$table: `wc -l $tempdir/$table.bad` errors" >> "$tempdir/final.log"
      fi
done
cat "$tempdir/final.log"




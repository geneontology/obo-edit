* Getting a GOlr instance using Solr 3.6 on Ubuntu.
** Install jetty
   sudo apt-get -u install jetty
** Beginning prep and download
   cd ~/local/src/java
   wget http://apache.cs.utah.edu/lucene/solr/3.6.0/apache-solr-3.6.0.tgz
   tar -zxvf apache-solr-3.6.0.tgz
** Solr schema
   schema.xml is generated something like:
   ~/local/src/svn/owltools/OWLTools-Runner/bin/owltools --solr-config /home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/ont-config.yaml /home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/bio-config.yaml /home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/ann-config.yaml /home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/ann_ev_agg-config.yaml --solr-schema-dump
** Experimental:
   sudo ./tools/golr.el

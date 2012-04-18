* Getting a GOlr instance using Solr 3.6 on Ubuntu.
** Install jetty
   sudo apt-get -u install jetty
** Beginning prep and download
   cd ~/local/src/java
   wget http://apache.cs.utah.edu/lucene/solr/3.6.0/apache-solr-3.6.0.tgz
   tar -zxvf apache-solr-3.6.0.tgz
** Experimental:
   sudo ./tools/golr.el

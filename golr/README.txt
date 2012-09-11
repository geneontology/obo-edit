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
* Updating the instance on stove
** update all of the amigo2 stuff and roll it out in the usual way
** run: "sudo ./tools/golr.el" to get the new schema and server ready
** to get the new fields out into the store
   cd /home/bbop/local/src/svn/owltools/; svn update; cd /home/bbop/local/src/svn/owltools/OWLTools-Parent/; mvn clean package -DskipTests; owltools http://purl.obolibrary.org/obo/ncbitaxon/subsets/taxslim.owl http://purl.obolibrary.org/obo/cl.owl http://purl.obolibrary.org/obo/go.owl --solr-url http://localhost:8080/solr/ --solr-purge --solr-config /home/bbop/local/src/svn/owltools/OWLTools-Solr/src/main/resources/ont-config.yaml --solr-load-ontology --solr-load-gafs http://www.geneontology.org/gene-associations/gene_association.GeneDB_Spombe.gz http://www.geneontology.org/gene-associations/gene_association.dictyBase.gz http://www.geneontology.org/gene-associations/gene_association.mgi.gz http://www.geneontology.org/gene-associations/gene_association.zfin.gz

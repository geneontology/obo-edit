package org.geneontology.solrj.test;

import java.io.IOException;

import junit.framework.TestCase;

import org.apache.solr.client.solrj.SolrServerException;
import org.geneontology.solrj.SolrLoader;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;


public class SolrLoaderTest extends TestCase {

	public static void testLoad() throws OWLOntologyCreationException, SolrServerException, IOException{
		SolrLoader  loader = getLoader();
		
		loader.load("test_resources/test.owl");
	}
	
	private static SolrLoader getLoader(){
		return new SolrLoader("http://localhost:8983/solr");
		
	}
	
}

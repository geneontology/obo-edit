package org.geneontology.gold.io.test;

import java.io.File;
import java.io.IOException;

import org.geneontology.gold.io.OntologyBulkLoader;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;

import owltools.graph.OWLGraphWrapper;

import junit.framework.TestCase;

public class OntologyBulkLoaderTest2 extends TestCase {

	public void testBulkLoad() throws IOException, OWLOntologyCreationException{
		
		OWLGraphWrapper g = new OWLGraphWrapper("test_resources/regulation_of_anti_apoptosis_xp.obo");
		OntologyBulkLoader loader = new OntologyBulkLoader(g);
		
		loader.dumpBulkLoadTables();
	}
	
	
	

}

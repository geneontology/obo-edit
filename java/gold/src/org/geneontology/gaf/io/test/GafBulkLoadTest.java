package org.geneontology.gaf.io.test;

import java.io.File;
import java.io.IOException;

import org.geneontology.gaf.hibernate.GafDocument;
import org.geneontology.gaf.hibernate.GafObjectsBuilder;
import org.geneontology.gaf.io.GafBulkLoader;

import junit.framework.TestCase;

public class GafBulkLoadTest extends TestCase {

	public void testBulkLoad() throws IOException{

		GafObjectsBuilder builder = new GafObjectsBuilder();
		
		GafDocument doc = builder.buildDocument( new File("test_resources/test_gene_association_mgi.gaf"));
		
		GafBulkLoader loader = new GafBulkLoader(doc, "data", "");
		
		loader.loadAll();
	}
	
}

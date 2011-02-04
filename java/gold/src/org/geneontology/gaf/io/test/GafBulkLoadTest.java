package org.geneontology.gaf.io.test;

import java.io.File;
import java.io.IOException;

import org.geneontology.gaf.hibernate.GAFParserHandlerForHibernate;
import org.geneontology.gaf.hibernate.GafDocument;
import org.geneontology.gaf.io.GafBulkLoader;
import org.geneontology.gaf.parser.GAFParser;

import junit.framework.TestCase;

public class GafBulkLoadTest extends TestCase {

	public void testBulkLoad() throws IOException{
		GAFParser parser = new GAFParser();
		GAFParserHandlerForHibernate handler = new GAFParserHandlerForHibernate();
		parser.parse(new File("test_resources/test_gene_association_mgi.gaf"), handler);
		
		
		GafDocument doc = handler.getGafDocument();
		
		GafBulkLoader loader = new GafBulkLoader(doc, "data", "");
		
		loader.loadAll();
	}
	
}

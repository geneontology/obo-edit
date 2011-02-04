package org.geneontology.gaf.parser.test;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.io.ParserWrapper;
import junit.framework.TestCase;

import org.geneontology.gaf.hibernate.GAFParserHandlerForHibernate;
import org.geneontology.gaf.hibernate.GafDocument;
import org.geneontology.gaf.parser.GAFParser;
import org.geneontology.gaf.parser.GAFParserHandler;
import org.geneontology.gold.rules.AnnotationTaxonCheck;
import org.obolibrary.oboformat.model.FrameMergeException;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;

public class BasicGAFParserTest extends TestCase {
	
	GAFParser parser;
	GAFParserHandlerForHibernate handler;
	AnnotationTaxonCheck ac;
	File gafFile;
	
	@Override
	public void setUp() throws Exception {
		parser = new GAFParser();
		gafFile = new File("test_resources/test_gene_association_mgi.gaf");
		handler = new GAFParserHandlerForHibernate();
	}


	public void testCheck() throws OWLOntologyCreationException, IOException, FrameMergeException {
		parser.parse(gafFile, handler);
		
		GafDocument doc = handler.getGafDocument();
	
		assertFalse(doc.getBioentities().isEmpty());
		
		assertFalse(doc.getGeneAnnotations().isEmpty());
		
		
		
		// DO SOME STUFF HERE
	}

}

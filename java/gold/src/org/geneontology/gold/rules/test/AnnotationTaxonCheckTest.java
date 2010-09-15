package org.geneontology.gold.rules.test;

import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.io.ParserWrapper;
import junit.framework.TestCase;
import org.obolibrary.oboformat.model.FrameMergeException;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;

public class AnnotationTaxonCheckTest extends TestCase {
	
	public void testAll() throws OWLOntologyCreationException, IOException, FrameMergeException {
		List<String> files = new Vector<String>();
		files.add("test_resources/cell_prolif_placenta.obo");
		files.add("test_resources/ncbi_taxon_slim.obo");
		files.add("test_resources/taxon_union_terms.obo");
		files.add("test_resources/taxon_go_triggers.obo");
		ParserWrapper pw = new ParserWrapper();

		OWLOntology ont = pw.parseOBOFiles(files);
		OWLGraphWrapper gw = new OWLGraphWrapper(ont);
		OWLObject cls = gw.getOWLObjectByIdentifier("GO:0001701");
		Set<OWLGraphEdge> edges = gw.getOutgoingEdgesClosure(cls);
		for (OWLGraphEdge e : edges) {
			System.out.println(e);
		}
	}
}

package owltools.mireot.test;

import java.io.IOException;
import java.util.Collection;
import java.util.Set;

import org.obolibrary.obo2owl.Obo2Owl;
import org.obolibrary.oboformat.model.Frame;
import org.obolibrary.oboformat.model.OBODoc;
import org.obolibrary.oboformat.model.Xref;
import org.obolibrary.oboformat.parser.OBOFormatParser;
import org.semanticweb.owlapi.io.OWLXMLOntologyFormat;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyFormat;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.io.ParserWrapper;
import owltools.mireot.Mireot;

import junit.framework.TestCase;

public class MireotTest2 extends TestCase {

	public static void testMireot() throws IOException, OWLOntologyCreationException, OWLOntologyStorageException {
		ParserWrapper pw = new ParserWrapper();
		
		// this test ontology has a class defined using a caro class, and imports caro_local
		OWLGraphWrapper g =
			pw.parseToOWLGraph("file:test_resources/caro_mireot_test.owl");
		OWLOntology ont = g.getOntology();
		OWLGraphWrapper rg =
			pw.parseToOWLGraph("file:test_resources/caro_local.owl");
		OWLOntology refOnt = rg.getOntology();
		
		Mireot m = new Mireot(ont);
		
		m.addReferencedOntology(refOnt);
		for (OWLEntity e : m.getExternalReferencedEntities()) {
			System.out.println("e="+e);
		}
		for (OWLObject e : m.getClosure()) {
			System.out.println("c="+e);
		}
		for (OWLAxiom ax : m.getClosureAxioms()) {
			System.out.println("M_AX:"+ax);
		}
	}
	
}

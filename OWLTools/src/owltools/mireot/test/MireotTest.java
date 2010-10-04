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

public class MireotTest extends TestCase {

	public static void testMireot() throws IOException, OWLOntologyCreationException, OWLOntologyStorageException {
		ParserWrapper pw = new ParserWrapper();
		OWLGraphWrapper g =
			pw.parseToOWLGraph("file:test_resources/caro_mireot_test.owl");
		OWLOntology ont = g.getOntology();
		OWLGraphWrapper rg =
			pw.parseToOWLGraph("file:test_resources/caro_local.owl");
		OWLOntology refOnt = rg.getOntology();
		
		Mireot m = new Mireot(ont);
		m.addReferencedOntology(refOnt);
		OWLOntology mOnt = m.mireot(ont);
		for (OWLClass cls : mOnt.getClassesInSignature()) {
			System.out.println("M:"+cls);
		}
		for (OWLAxiom ax : mOnt.getAxioms()) {
			System.out.println("M_AX:"+ax);
		}
	}
	
}

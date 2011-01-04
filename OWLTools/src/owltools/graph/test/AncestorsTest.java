package owltools.graph.test;

import java.io.File;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLRestriction;

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.graph.OWLGraphWrapper.Config;

import junit.framework.TestCase;

public class AncestorsTest extends TestCase {

	public static void testIntersectionsReturnedInClosure() throws Exception {
		OWLGraphWrapper  g =  getOntologyWrapper();
		OWLObject obj = g.getOWLObject("http://example.org#o1");
		boolean ok = false;
		for (OWLGraphEdge e : g.getOutgoingEdgesClosureReflexive(obj)) {
			System.out.println(e);
			if (e.getTarget() instanceof OWLObjectIntersectionOf)
				ok = true;
		}
		assertTrue(ok);
	}
	public static void testRestrictionsReturnedInClosure() throws Exception {
		OWLGraphWrapper  g =  getOntologyWrapper();
		OWLObject obj = g.getOWLObject("http://example.org#deformed_hippocampus");
		boolean ok = false;
		for (OWLGraphEdge e : g.getOutgoingEdgesClosureReflexive(obj)) {
			System.out.println(e);
			if (e.getTarget() instanceof OWLRestriction)
				ok = true;
		}
		assertTrue(ok);
	}
	public static void testExclusion() throws Exception {
		OWLGraphWrapper  g =  getOntologyWrapper();
		Config cfg = g.getConfig();
		cfg.excludeProperty(g.getDataFactory().getOWLObjectProperty(IRI.create("http://example.org#has")));
		OWLObject obj = g.getOWLObject("http://example.org#o1");
		OWLObject eye = g.getOWLObject("http://example.org#eye");
		boolean ok = true;
		for (OWLGraphEdge e : g.getOutgoingEdgesClosureReflexive(obj)) {
			if (e.getTarget().equals(eye))
				ok = false;
		}
		assertTrue(ok);
	}



	private static OWLGraphWrapper getOntologyWrapper() throws OWLOntologyCreationException{
		OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
		return new OWLGraphWrapper( 
				manager.loadOntologyFromOntologyDocument(
						new File("test_resources/lcstest2.owl")));
	}
		
	
	
}

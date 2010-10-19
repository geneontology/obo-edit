package owltools.graph.test;

import java.io.File;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import owltools.graph.OWLGraphWrapper;

import junit.framework.TestCase;

public class OWLGraphWrapperTest extends TestCase {

	public static void testSynonyms() throws Exception{
		OWLGraphWrapper  wrapper =  getOntologyWrapper();
		
		OWLObject cls = wrapper.getOWLClass(OWLGraphWrapper.DEFAULT_IRI_PREFIX + "CHEBI_15355");
		
		String s[] = wrapper.getSynonym(cls);
		assertTrue(s.length>0);
	}
	
	public static void testDef() throws Exception{
		OWLGraphWrapper  wrapper =  getOntologyWrapper();
		
		OWLObject cls = wrapper.getOWLClass(OWLGraphWrapper.DEFAULT_IRI_PREFIX + "CHEBI_15355");
		
		String s = wrapper.getDef(cls);
		assertTrue(s != null);
	}

	private static OWLGraphWrapper getOntologyWrapper() throws OWLOntologyCreationException{
		OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
		return new OWLGraphWrapper( 
				manager.loadOntologyFromOntologyDocument(
						new File("test_resources/test.owl")));
	}
	
	
	
}

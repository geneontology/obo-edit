package org.geneontology.gold.io.test;

import java.io.IOException;

import org.geneontology.gold.io.OntologyBulkLoader;
import org.obolibrary.obo2owl.Obo2Owl;
import org.obolibrary.oboformat.model.OBODoc;
import org.obolibrary.oboformat.parser.OBOFormatParser;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;

import owltools.graph.OWLGraphWrapper;
import junit.framework.TestCase;

public class OntologyBulkLoaderTest extends TestCase {

	public static void testLoad() throws Exception{
		OWLGraphWrapper wrapper = getGraphWrapper();
		
		OntologyBulkLoader loader = new OntologyBulkLoader(wrapper, "test_resources", "");
		
		loader.dumpBulkLoadTables();
	}

	private static OWLGraphWrapper getGraphWrapper() throws IOException,
			OWLOntologyCreationException {

		OBOFormatParser p = new OBOFormatParser();
		OBODoc obodoc = p.parse("test_resources/caro.obo");

		Obo2Owl bridge = new Obo2Owl();
		OWLOntology ontology = bridge.convert(obodoc);

		return new OWLGraphWrapper(ontology);
	}

}

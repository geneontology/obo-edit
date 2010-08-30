package owltools.io;

import java.io.IOException;

import org.obolibrary.obo2owl.Obo2Owl;
import org.obolibrary.oboformat.model.OBODoc;
import org.obolibrary.oboformat.parser.OBOFormatParser;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import owltools.graph.OWLGraphWrapper;

public class ParserWrapper {

	public OWLGraphWrapper parseToOWLGraph(String iriString) throws OWLOntologyCreationException, IOException {
		return new OWLGraphWrapper(parse(iriString));		
	}

	public OWLOntology parse(String iriString) throws OWLOntologyCreationException, IOException {
		if (iriString.endsWith(".obo"))
			return parseOBO(iriString);
		return parseOWL(iriString);		
	}
	
	private OWLOntology parseOBO(String iri) throws IOException, OWLOntologyCreationException {
		OBOFormatParser p = new OBOFormatParser();
		OBODoc obodoc = p.parse(iri);

		Obo2Owl bridge = new Obo2Owl();
		OWLOntologyManager manager = bridge.getManager();
		OWLOntology ontology = bridge.convert(obodoc);
		return ontology;
	}

	public OWLOntology parseOWL(String iriString) throws OWLOntologyCreationException {
		IRI iri = IRI.create(iriString);
		return parseOWL(iri);

	}
	
	public OWLOntology parseOWL(IRI iri) throws OWLOntologyCreationException {
		OWLOntologyManager manager = OWLManager.createOWLOntologyManager(); // persist?
		OWLDataFactory df = manager.getOWLDataFactory();
		OWLOntology ont = manager.loadOntologyFromOntologyDocument(iri);
		return ont;
	}


}
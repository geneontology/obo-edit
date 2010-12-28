package owltools.io;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;

import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLAnnotationSubject;
import org.semanticweb.owlapi.model.OWLAnnotationValue;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLProperty;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;

import owltools.graph.OWLGraphWrapper;

/**
 * reads in a table (e.g. tab-delimited table) converting each row to an OWL Axiom.
 * 
 * These could be ClassAssertion axioms, SubClassOf axioms, etc
 * 
 * @author cjm
 *
 */
public class TableToAxiomConverter {
	
	public class Config {
		public boolean isOboIdentifiers = true;
		public boolean isSwitchSubjectObject = false;
		public AxiomType axiomType;
		public IRI property;
		
		public void setPropertyToLabel() {
			property = OWLRDFVocabulary.RDFS_LABEL.getIRI();
		}
	}
	
	public Config config = new Config();
	public OWLGraphWrapper graph;
	
	
	
	public TableToAxiomConverter(OWLGraphWrapper graph) {
		super();
		this.graph = graph;
	}

	public void parse(String fn) throws IOException {
		File myFile = new File(fn);
		FileReader fileReader = new FileReader(myFile);
		BufferedReader reader = new BufferedReader(fileReader);
		String line;
		while ((line = reader.readLine()) != null) {
			String[] row = line.split("\t");
			addRow(row);
		}
	}

	public OWLAxiom rowToAxiom(String[] row) {
		OWLDataFactory df = graph.getDataFactory();
		 String sub = row[0];
		 String obj = row[1];
		if (config.isSwitchSubjectObject) {
			sub = row[1];
			obj = row[0];
		}
		OWLAxiom ax = null;
		if (config.axiomType.equals(AxiomType.CLASS_ASSERTION)) {
			ax = df.getOWLClassAssertionAxiom(resolveClass(sub), (OWLIndividual) resolveIndividual(obj));
		}
		else if (config.axiomType.equals(AxiomType.ANNOTATION_ASSERTION)) {
			ax = df.getOWLAnnotationAssertionAxiom(df.getOWLAnnotationProperty(config.property), 
					(OWLAnnotationSubject) resolveIndividual(sub), literal(obj));
		}
		else if (config.axiomType.equals(AxiomType.OBJECT_PROPERTY_ASSERTION)) {
			ax = df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(config.property), 
					resolveIndividual(sub), resolveIndividual(obj));
		}
		else {
			// TODO
		}
		return ax;
	}
	
	public void addRow(String[] row) {
		addRow(graph.getSourceOntology(), row);
		
	}
	public void addRow(OWLOntology ont, String[] row) {
		 graph.getManager().applyChange(new AddAxiom(ont, rowToAxiom(row)));
	}

	private OWLAnnotationValue literal(String obj) {
		return graph.getDataFactory().getOWLTypedLiteral(obj);
	}

	private OWLClass resolveClass(String id) {
		if (config.isOboIdentifiers) {
			return (OWLClass) graph.getOWLObjectByIdentifier(id);
		}
		return graph.getOWLClass(id);
	}

	private OWLIndividual resolveIndividual(String id) {
		if (config.isOboIdentifiers) {
			return graph.getOWLIndividualByIdentifier(id);
		}
		return graph.getOWLIndividual(IRI.create(id));
	}
}

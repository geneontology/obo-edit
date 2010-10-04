package owltools.mireot;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.Stack;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.AddImport;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLImportsDeclaration;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import owltools.graph.OWLGraphWrapper;

public class Mireot {

	OWLOntologyManager manager;
	OWLDataFactory dataFactory;
	Set<OWLAxiom> mAxioms = new HashSet<OWLAxiom>();
	OWLOntology ontology;
	Set<OWLOntology> referencedOntologies = new HashSet<OWLOntology>();
	Set<OWLOntology> allOntologies = null;



	public Mireot(OWLOntologyManager manager, OWLDataFactory dataFactory,
			OWLOntology ontology) {
		super();
		this.manager = manager;
		this.dataFactory = dataFactory;
		this.ontology = ontology;
	}



	public Mireot(OWLOntology ontology) {
		super();
		this.ontology = ontology;
		manager = OWLManager.createOWLOntologyManager();
		dataFactory = manager.getOWLDataFactory();
	}



	public Set<OWLOntology> getReferencedOntologies() {
		return referencedOntologies;
	}

	public Set<OWLOntology> getAllOntologies() {
		if (allOntologies == null) {
			allOntologies = new HashSet<OWLOntology>();
			allOntologies.addAll(getReferencedOntologies());
			allOntologies.add(ontology);
		}
		return allOntologies;
	}


	public void setReferencedOntologies(Set<OWLOntology> referencedOntologies) {
		allOntologies = null;
		this.referencedOntologies = referencedOntologies;
	}
	
	public void addReferencedOntology(OWLOntology refOnt) {
		allOntologies = null;
		this.referencedOntologies.add(refOnt);
	}

	public OWLOntologyManager getManager() {
		return manager;
	}

	public void setManager(OWLOntologyManager manager) {
		this.manager = manager;
	}

	public OWLOntology getOntology() {
		return ontology;
	}

	public void setOntology(OWLOntology ontology) {
		this.ontology = ontology;
	}

	public void addImport(String importedIRIString) {
		OWLImportsDeclaration iax = dataFactory.getOWLImportsDeclaration(IRI.create(importedIRIString));
		AddImport addAx = new AddImport(ontology, iax);
		manager.applyChange(addAx);
	}

	public OWLOntology mireot() throws OWLOntologyCreationException {
		Stack<OWLObject> objs = new Stack<OWLObject>();
		return mireot(objs);
	}

	public OWLOntology mireot(OWLOntology ont) throws OWLOntologyCreationException {
		Set<OWLClass> classes = ont.getClassesInSignature(false);
		Stack<OWLObject> objs = new Stack<OWLObject>();
		objs.addAll(classes);
		return mireot(objs);
	}


	public OWLOntology mireot(Stack<OWLObject> objs) throws OWLOntologyCreationException {
		OWLOntology ont = getOntology();


		Set<OWLObject> visitedObjs = new HashSet<OWLObject>();

		while (objs.size() > 0) {
			OWLObject nextObj = objs.pop();
			for (OWLOntology refOnt : getAllOntologies()) {
				Set nextAxioms;
				//System.out.println(nextObj);
				if (nextObj instanceof OWLClass)
					nextAxioms = refOnt.getAxioms((OWLClass)nextObj);
				else if (nextObj instanceof OWLObjectProperty) {
					nextAxioms = refOnt.getAxioms((OWLObjectProperty)nextObj);
				}
				else if (nextObj instanceof OWLDataProperty) {
					nextAxioms = refOnt.getAxioms((OWLDataProperty)nextObj);
				}
				else if (nextObj instanceof OWLClassExpression) {
					//((OWLClassExpression)nextObj).getClassesInSignature();

					continue;
				}
				else {
					continue;
				}
				for (Object x : nextAxioms) {
					OWLAxiom axiom = (OWLAxiom)x;
					mAxioms.add(axiom);

					Set<OWLObject> newObjs = new HashSet<OWLObject>();
					newObjs.addAll(axiom.getClassesInSignature());
					newObjs.addAll(axiom.getObjectPropertiesInSignature());
					newObjs.addAll(axiom.getDataPropertiesInSignature());
					newObjs.removeAll(visitedObjs);
					objs.addAll(newObjs);
					visitedObjs.addAll(newObjs);
				}
			}
		}

		OWLOntology mOnt = manager.createOntology();
		for (OWLAxiom axiom : mAxioms) {
			AddAxiom addAx = new AddAxiom(mOnt, axiom);
			manager.applyChange(addAx);
		}
		return mOnt;
	}
}

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
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLImportsDeclaration;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.UnknownOWLOntologyException;

import owltools.graph.OWLGraphWrapper;

/**
 * Given one source ontology importing one or more referenced ontologies
 * (e.g. CL referencing PRO, GO, CHEBI, UBERON), merge/copy selected axiom
 * from the referenced ontologies into the source ontology
 * 
 * @author cjm
 *
 */
public class Mireot {

	OWLOntologyManager manager;
	OWLDataFactory dataFactory;
	Set<OWLAxiom> mAxioms = new HashSet<OWLAxiom>();
	OWLOntology ontology;
	Set<OWLOntology> referencedOntologies = new HashSet<OWLOntology>();
	Set<OWLOntology> allOntologies = null;
	OWLGraphWrapper graph;

	public Mireot(OWLOntologyManager manager, OWLDataFactory dataFactory,
			OWLOntology ontology) {
		super();
		this.manager = manager;
		this.dataFactory = dataFactory;
		this.ontology = ontology;
	}



	public Mireot(OWLOntology ontology) throws UnknownOWLOntologyException, OWLOntologyCreationException {
		super();
		this.ontology = ontology;
		manager = OWLManager.createOWLOntologyManager();
		dataFactory = manager.getOWLDataFactory();
		// ensure the graph object follows the import closure
		graph = new OWLGraphWrapper(ontology, true);
	}



	public Set<OWLOntology> getReferencedOntologies() {
		return referencedOntologies;
	}

	/**
	 * @return union of referenced ontologies and source ontology
	 */
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

	/**
	 * 
	 * @param refOnt
	 */
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



	public OWLGraphWrapper getGraph() {
		return graph;
	}



	public void setGraph(OWLGraphWrapper graph) {
		this.graph = graph;
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

	/**
	 * create a new ontology that includes everything from the source ontology, plus
	 * selected axioms from referenced ontologies.
	 * 
	 * @param ont -- source ontology
	 * @return
	 * @throws OWLOntologyCreationException
	 */
	public OWLOntology mireot(OWLOntology ont) throws OWLOntologyCreationException {
		Set<OWLClass> classes = ont.getClassesInSignature(false);
		Stack<OWLObject> objs = new Stack<OWLObject>();
		objs.addAll(classes);
		return mireot(objs);
	}


	/**
	 * note: only use this in client code if fine-grained control of merging is required.
	 * 
	 * @param objs -- source objects (typically all objects declared in source ontology)
	 * @return
	 * @throws OWLOntologyCreationException
	 */
	public OWLOntology mireot(Stack<OWLObject> objs) throws OWLOntologyCreationException {
		OWLOntology ont = getOntology();


		Set<OWLObject> visitedObjs = new HashSet<OWLObject>();

		// keep track of all axioms to be included
		while (objs.size() > 0) {
			OWLObject nextObj = objs.pop();
			for (OWLOntology refOnt : getAllOntologies()) {

				// Step 1: find all axioms connected to the source object
				//  that satisfy the traversal constraints
				Set nextAxioms;
				//System.out.println(nextObj);
				if (nextObj instanceof OWLClass) {
					// both subclasses and superclasses
					nextAxioms = refOnt.getAxioms((OWLClass)nextObj);
				}
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

				// Step 2: for all axioms to be included, ensure all objects
				//  referenced in axiom are included
				//  (this is a little backwards right now - should find objects first then axioms)
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

	public Set<OWLEntity> getExternalReferencedEntities() {
		OWLOntology ont = getOntology();
		Set<OWLEntity> objs = ont.getSignature(false);
		/*
		Set<OWLEntity> objs = new HashSet<OWLEntity>();
		objs.addAll(ont.getClassesInSignature(false));
		objs.addAll(ont.getIndividualsInSignature(false));
		objs.addAll(ont.getObjectPropertiesInSignature(false));
		objs.addAll(ont.getDataPropertiesInSignature(false));
		 */
		Set<OWLEntity> refObjs = new HashSet<OWLEntity>();
		for (OWLEntity obj :objs) {
			for (OWLOntology refOnt : getReferencedOntologies()) {
				if (refOnt.getDeclarationAxioms(obj).size() > 0) {
					refObjs.add(obj);
					continue;
				}
			}
		}
		return refObjs;

	}
	public Set<OWLObject> getClosure() {
		Set<OWLObject> objs = new HashSet<OWLObject>();
		Set<OWLEntity> refs = getExternalReferencedEntities();
		for (OWLEntity ref : refs) {
			// todo - allow per-relation control
			Set<OWLObject> ancs = graph.getAncestorsReflexive(ref);
			objs.addAll(ancs);
		}
		return objs;
	}

	/**
	 * find all axioms in closure
	 * @return
	 */
	public Set<OWLAxiom> getClosureAxioms() {
		Set<OWLAxiom> axioms = new HashSet<OWLAxiom>();
		Set<OWLObject> objs = getClosure();
		for (OWLOntology refOnt : getReferencedOntologies()) {
			for (OWLObject obj : objs) {
				if (obj instanceof OWLClass) {
					axioms.addAll(refOnt.getAxioms((OWLClass) obj));
				}
				else if (obj instanceof OWLObjectProperty) {
					axioms.addAll(refOnt.getAxioms((OWLObjectProperty) obj));
				}
				else if (obj instanceof OWLNamedIndividual) {
					axioms.addAll(refOnt.getAxioms((OWLNamedIndividual) obj));
				}
				else if (obj instanceof OWLDataProperty) {
					axioms.addAll(refOnt.getAxioms((OWLDataProperty) obj));
				}
				else {
					// TODO
				}
				axioms.addAll(((OWLEntity) obj).getAnnotationAssertionAxioms(refOnt));
			}
		}
		Set<OWLAxiom> filteredAxioms = new HashSet<OWLAxiom>();
		for (OWLAxiom a : axioms) {
			boolean includeThis = true;
			
			// make this configurable
			if (a instanceof OWLAnnotationAssertionAxiom) {
				//
			}
			else {
				for (OWLEntity e : a.getSignature()) {
					if (!objs.contains(e)) {
						System.out.println("removing:"+a+" -- E:"+e);
						includeThis = false;
						break;
					}
				}
			}
			if (includeThis)
				filteredAxioms.add(a);
		}
		return filteredAxioms;
	}

}

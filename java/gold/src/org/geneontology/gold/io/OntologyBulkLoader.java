package org.geneontology.gold.io;

import java.util.Set;

import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owlapi.model.OWLNamedObject;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;

import owltools.graph.OWLGraphWrapper;

/**
 * Loads ontology into Gold database.
 * 
 * TO BE IMPLEMENTED
 * 
 * Default implementation is to use bulkloader framework.
 * - dumps files
 * - bulk loads them into bulk_* tables
 * - uses sql insert/update/delete commands to sync main tables from bulk_*  
 *
 */
public class OntologyBulkLoader {
	
	/**
	 * Ontologies are accessed via GraphWrappers
	 */
	private OWLGraphWrapper graphWrapper;
	
	public OWLOntology getOwlOntology() {
		return graphWrapper.getOntology();
	}
	
	/**
	 * 
	 */
	public void dumpBulkLoadTables() {
		dumpDeclarationsAndMetadata();
		dumpLogicalAxioms();
	}

	public void dumpDeclarationsAndMetadata() {
		TableDumper clsDumper = new TableDumper("cls");
		for (OWLClass cls : getOwlOntology().getClassesInSignature()) {
			String label = graphWrapper.getLabel(cls);
			String def = graphWrapper.getDef(cls);
			// textdef TODO
			//clsDumper.dumpRow(id, label, ...); TODO
		}
		TableDumper relDumper = new TableDumper("relation");
		for (OWLObjectProperty op : getOwlOntology().getObjectPropertiesInSignature()) {
			String label = graphWrapper.getLabel(op);
			// textdef TODO
			//relDumper.dumpRow(id, label, ...); TODO
		}
	}
	
	public void dumpLogicalAxioms() {
		TableDumper subClassOfDumper = new TableDumper("subclass_of");
		TableDumper allSomeRelationshipDumper = new TableDumper("all_some_relationship");
		
		Set<OWLSubClassOfAxiom> axioms = getOwlOntology().getAxioms(AxiomType.SUBCLASS_OF);
		for (OWLSubClassOfAxiom sca : axioms) {
			//OWLSubClassOfAxiom sca = 
			//	(OWLSubClassOfAxiom) ax;
			OWLClassExpression subcls = sca.getSubClass();
			OWLClassExpression supercls = sca.getSuperClass();
			if (subcls instanceof OWLClass && supercls instanceof OWLClass) {
				subClassOfDumper.dumpRow(oboId(subcls),oboId(supercls));
			}		
			else if (subcls instanceof OWLClass && supercls instanceof OWLObjectSomeValuesFrom) {
				OWLObjectSomeValuesFrom restr = (OWLObjectSomeValuesFrom) supercls;
				OWLObjectPropertyExpression p = restr.getProperty();
				OWLClassExpression filler = restr.getFiller();
				allSomeRelationshipDumper.dumpRow(
						oboId(subcls),
						oboId(p),
						oboId(filler));
			}
			else {
				// TODO
			}
		}
		for (OWLEquivalentClassesAxiom sca : getOwlOntology().getAxioms(AxiomType.EQUIVALENT_CLASSES)) {
			//
		}
		// TODO - disjoint_with
		
	}
	
	private String oboId(OWLObject ob) {
		return graphWrapper.getIdentifier(ob);
	}
	

}

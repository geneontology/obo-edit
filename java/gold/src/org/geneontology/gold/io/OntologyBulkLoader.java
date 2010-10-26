package org.geneontology.gold.io;

import java.io.IOException;
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

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.graph.OWLQuantifiedProperty;

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
public class OntologyBulkLoader extends AbstractBulkLoader{
	
	public OntologyBulkLoader(OWLGraphWrapper wrapper, String path) {
		super(wrapper, path);
	}

	public OntologyBulkLoader(OWLGraphWrapper wrapper) {
		super(wrapper);
	}

	/**
	 * 
	 */
	public void dumpBulkLoadTables() throws IOException{
		dumpDeclarationsAndMetadata();
		dumpLogicalAxioms();
	}

	public void dumpDeclarationsAndMetadata() throws IOException {
		TableDumper clsDumper = new TableDumper("cls", this.path);
		
		
		TableDumper obj_alternate_labelDumper = new TableDumper("obj_alternate_label", this.path);
	//	TableDumper subclass_ofDumper = new TableDumper("subclass_of");
		//TableDumper allSomeRelationship = new TableDumper("all_some_relationship");
		
		
		for (OWLClass cls : getOwlOntology().getClassesInSignature()) {
			String label = graphWrapper.getLabel(cls);
			String def = graphWrapper.getDef(cls);
			String id = graphWrapper.getIdentifier(cls);
			// textdef TODO
			clsDumper.dumpRow(id, label, getOwlOntology().getOntologyID().toString(), null, null, def, null);
			
			
			for(String l: graphWrapper.getSynonymStrings(cls)){
				obj_alternate_labelDumper.dumpRow(id, l, null, null, null);
			}
			
			
/*			for(String sc: graphWrapper.getSubClassesNames(cls)){
				subclass_ofDumper.dumpRow(sc, label, null);
			}
			
			
			for(OWLGraphEdge ed: graphWrapper.getOutgoingEdges(cls)){
				String prop = null;
				for(OWLQuantifiedProperty qp: ed.getQuantifiedPropertyList()){
					if(qp.isSomeValuesFrom())
						prop  = qp.getPropertyId();
				}
				
				if(prop != null){
					allSomeRelationship.dumpRow(ed.getSourceId(), prop, ed.getTargetId(), null);
				}
			}*/
		}
		TableDumper relDumper = new TableDumper("relation", path);
		for (OWLObjectProperty op : getOwlOntology().getObjectPropertiesInSignature()) {
			String label = graphWrapper.getLabel(op);
			String def = graphWrapper.getDef(op);
			String id = graphWrapper.getIdentifier(op);
			// textdef TODO
			relDumper.dumpRow(id, label, null, null, null, def, null, null, null, null);
			
			for(String l: graphWrapper.getSynonymStrings(op)){
				obj_alternate_labelDumper.dumpRow(id, l, null, null, null);
			}
			
			
		}
		
		
		clsDumper.close();
		relDumper.close();
		obj_alternate_labelDumper.close();
		
	}
	
	public void dumpLogicalAxioms() throws IOException {
		TableDumper subClassOfDumper = new TableDumper("subclass_of", path);
		TableDumper allSomeRelationshipDumper = new TableDumper("all_some_relationship", path);
		
		Set<OWLSubClassOfAxiom> axioms = getOwlOntology().getAxioms(AxiomType.SUBCLASS_OF);
		for (OWLSubClassOfAxiom sca : axioms) {
			//OWLSubClassOfAxiom sca = 
			//	(OWLSubClassOfAxiom) ax;
			OWLClassExpression subcls = sca.getSubClass();
			OWLClassExpression supercls = sca.getSuperClass();
			if (subcls instanceof OWLClass && supercls instanceof OWLClass) {
				subClassOfDumper.dumpRow(oboId(subcls),oboId(supercls), null);
			}		
			else if (subcls instanceof OWLClass && supercls instanceof OWLObjectSomeValuesFrom) {
				OWLObjectSomeValuesFrom restr = (OWLObjectSomeValuesFrom) supercls;
				OWLObjectPropertyExpression p = restr.getProperty();
				OWLClassExpression filler = restr.getFiller();
				allSomeRelationshipDumper.dumpRow(
						oboId(subcls),
						oboId(p),
						oboId(filler), null);
			}
			else {
				// TODO
			}
		}
		for (OWLEquivalentClassesAxiom sca : getOwlOntology().getAxioms(AxiomType.EQUIVALENT_CLASSES)) {
			//
		}
		// TODO - disjoint_with
		
		
		allSomeRelationshipDumper.close();
		subClassOfDumper.close();
	}
	
	private String oboId(OWLObject ob) {
		return graphWrapper.getIdentifier(ob);
	}
	

}

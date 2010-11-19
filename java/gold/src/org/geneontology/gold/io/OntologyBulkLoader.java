package org.geneontology.gold.io;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.Vector;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDisjointClassesAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owlapi.model.OWLNamedObject;
import org.semanticweb.owlapi.model.OWLNaryBooleanClassExpression;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectUnionOf;
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
public class OntologyBulkLoader extends AbstractBulkLoader{
	
	
	public OntologyBulkLoader(OWLGraphWrapper wrapper, String path, String dumpFilePrefix) {
		super(wrapper, path, dumpFilePrefix);
		
		
	}

	public OntologyBulkLoader(OWLGraphWrapper wrapper) {
		super(wrapper);
	}

	/**
	 * 
	 */
	public List<String> dumpBulkLoadTables() throws IOException{
		List<String> list = new Vector<String>();
		
		list.addAll(dumpDeclarationsAndMetadata());
		list.addAll(dumpLogicalAxioms());
		
		return list;
	}
	
	public String dumpOntologyTable() throws IOException{
		TableDumper ontologyDumper = new TableDumper(this.dumpFilePrefix + "cls", this.path);
	
		ontologyDumper.dumpRow(graphWrapper.getOntologyId(), null, null, null);
		ontologyDumper.close();
		
		
		return ontologyDumper.getTable();
	}

	public List<String> dumpDeclarationsAndMetadata() throws IOException {
		TableDumper clsDumper = new TableDumper(this.dumpFilePrefix + "cls", this.path);
		
		
		TableDumper obj_alternate_labelDumper = new TableDumper(this.dumpFilePrefix + "obj_alternate_label", this.path);

		
		TableDumper obj_definition_xref = new TableDumper(this.dumpFilePrefix + "obj_definition_xref", this.path);

		TableDumper obj_xref = new TableDumper(this.dumpFilePrefix + "obj_xref", this.path);

		
		TableDumper cls_intersection_ofDumper = new TableDumper(this.dumpFilePrefix + "cls_intersection_of", this.path);

		TableDumper cls_union_ofDumper = new TableDumper(this.dumpFilePrefix + "cls_union_of", this.path);
		
		TableDumper equivalent_toDumper = new TableDumper(this.dumpFilePrefix + "equivalent_to", this.path);
		
		TableDumper disjoint_withDumper = new TableDumper(this.dumpFilePrefix + "disjoint_with", this.path);
		
		
		//	TableDumper subclass_ofDumper = new TableDumper("subclass_of");
		//TableDumper allSomeRelationship = new TableDumper("all_some_relationship");
		
		String ontologyId = graphWrapper.getOntologyId();
		for (OWLClass cls : getOwlOntology().getClassesInSignature()) {
			String label = graphWrapper.getLabel(cls);
			String def = graphWrapper.getDef(cls);
			String id = graphWrapper.getIdentifier(cls);
			// textdef TODO
			clsDumper.dumpRow(id, label, ontologyId, null, null, def, null);
			
			//dump synonms
			for(String l: graphWrapper.getSynonymStrings(cls)){
				obj_alternate_labelDumper.dumpRow(id, l, null, null, null);
			}

			
			for(String xref: graphWrapper.getDefXref(cls)){
				obj_definition_xref.dumpRow(id, xref);
			}
			
			for(String xref: graphWrapper.getXref(cls)){
				obj_xref.dumpRow(id, xref, null);
			}
			
			//dump intersection of and union_of	
			for(OWLObject ec: cls.getEquivalentClasses(graphWrapper.getOntology())){
				if(ec instanceof OWLObjectIntersectionOf){
					dumpNaryBooleanExpression(cls_intersection_ofDumper, id, ontologyId, (OWLNaryBooleanClassExpression)ec);
				}else if(ec instanceof OWLObjectUnionOf){
					dumpNaryBooleanExpression(cls_union_ofDumper, id, ontologyId,(OWLNaryBooleanClassExpression) ec);
				}else if(ec instanceof OWLNamedObject){
					String id2 = graphWrapper.getIdentifier(ec);
					if(id != null)
						equivalent_toDumper.dumpRow(id, id2 , ontologyId);
				}
			}

			
			for(OWLObject ec: cls.getDisjointClasses(graphWrapper.getOntology())){
				String id2 = graphWrapper.getIdentifier(ec);
				if(id2 != null){
					disjoint_withDumper.dumpRow(id, id2, ontologyId);
				}
			}
			
		}
		TableDumper relDumper = new TableDumper(this.dumpFilePrefix + "relation", path);
		for (OWLObjectProperty op : getOwlOntology().getObjectPropertiesInSignature()) {
			String label = graphWrapper.getLabel(op);
			String def = graphWrapper.getDef(op);
			String id = graphWrapper.getIdentifier(op);
			// textdef TODO
			relDumper.dumpRow(id, label, ontologyId , null, null, def, null, null, null, null);
			
			for(String l: graphWrapper.getSynonymStrings(op)){
				obj_alternate_labelDumper.dumpRow(id, l, null, null, null);
			}
			
			
			for(String xref: graphWrapper.getDefXref(op)){
				obj_definition_xref.dumpRow(id, xref);
			}
			
			for(String xref: graphWrapper.getXref(op)){
				obj_xref.dumpRow(id, xref, null);
			}
			
		}
		
		clsDumper.close();
		relDumper.close();
		obj_alternate_labelDumper.close();
		obj_definition_xref.close();
		obj_xref.close();
		
		equivalent_toDumper.close();
		cls_intersection_ofDumper.close();
		cls_union_ofDumper.close();
		disjoint_withDumper.close();
	
		
		List<String> list = new ArrayList<String>();
		list.add(clsDumper.getTable());
		list.add(relDumper.getTable());
		list.add(obj_alternate_labelDumper.getTable());
		list.add(obj_alternate_labelDumper.getTable());
		list.add(obj_xref.getTable());
		list.add(obj_definition_xref.getTable());

		list.add(cls_intersection_ofDumper.getTable());
		list.add(cls_union_ofDumper.getTable());
		list.add(equivalent_toDumper.getTable());
		list.add(disjoint_withDumper.getTable());
		
		return list;
		
	}
	
	
	private void dumpNaryBooleanExpression(TableDumper dumper, String id, 
			String ontologyId,  OWLNaryBooleanClassExpression ec) 
					throws IOException{
		
		for(OWLClassExpression expression: ec.getOperands()){
			if(expression instanceof OWLClass){
				String targetId = graphWrapper.getIdentifier(expression);
				if(targetId != null){
					if(ec instanceof OWLObjectIntersectionOf)
						dumper.dumpRow(id, null, targetId,ontologyId);
					else
						dumper.dumpRow(id, targetId,ontologyId);
				}
			}else if(expression instanceof OWLObjectSomeValuesFrom){
				OWLObjectSomeValuesFrom restr = (OWLObjectSomeValuesFrom) expression;
				OWLObjectPropertyExpression p = restr.getProperty();
				OWLClassExpression filler = restr.getFiller();
				dumper.dumpRow(id,
						graphWrapper.getIdentifier(p),
						graphWrapper.getIdentifier(filler),
						ontologyId);
				
			}
		}		
		
	}
	
	public List<String> dumpLogicalAxioms() throws IOException {
		TableDumper subClassOfDumper = new TableDumper(this.dumpFilePrefix + "subclass_of", path);
		TableDumper allSomeRelationshipDumper = new TableDumper(this.dumpFilePrefix + "all_some_relationship", path);
	
		
		
		Set<OWLSubClassOfAxiom> axioms = getOwlOntology().getAxioms(AxiomType.SUBCLASS_OF);
		String ontologyId = graphWrapper.getOntologyId();
		for (OWLSubClassOfAxiom sca : axioms) {
			//OWLSubClassOfAxiom sca = 
			//	(OWLSubClassOfAxiom) ax;
			OWLClassExpression subcls = sca.getSubClass();
			OWLClassExpression supercls = sca.getSuperClass();
			if (subcls instanceof OWLClass && supercls instanceof OWLClass) {
				subClassOfDumper.dumpRow(oboId(subcls),oboId(supercls), ontologyId);
			}		
			else if (subcls instanceof OWLClass && supercls instanceof OWLObjectSomeValuesFrom) {
				OWLObjectSomeValuesFrom restr = (OWLObjectSomeValuesFrom) supercls;
				OWLObjectPropertyExpression p = restr.getProperty();
				OWLClassExpression filler = restr.getFiller();
				allSomeRelationshipDumper.dumpRow(
						oboId(subcls),
						oboId(p),
						oboId(filler), ontologyId);
			}
			else {
				// TODO
			}
		}
		
		
		/*for (OWLEquivalentClassesAxiom sca : getOwlOntology().getAxioms(AxiomType.EQUIVALENT_CLASSES)) {
		
			System.out.println("----");
			OWLObject obj = null;
			for(OWLClassExpression ce: sca.getClassExpressionsAsList()){
				if(obj == null)
					obj = ce;
				else if(ce instanceof OWLNaryBooleanClassExpression){
					
					String id = graphWrapper.getIdentifier(obj);
					if(id != null){
						if(ce instanceof OWLObjectIntersectionOf){
							dumpNaryBooleanExpression(cls_intersection_ofDumper, id, ontologyId, (OWLNaryBooleanClassExpression)ce);
						}else if(ce instanceof OWLObjectUnionOf){
							dumpNaryBooleanExpression(cls_union_ofDumper, id, ontologyId,(OWLNaryBooleanClassExpression) ce);
						}
					}
				
					
					break;
				}else{
					String id = graphWrapper.getIdentifier(obj);
					String id2 = graphWrapper.getIdentifier(ce);

					if(id != null && id2 != null){
						equivalent_toDumper.dumpRow(id, id2, ontologyId);
					}
					break;
				}
			}
		}*/
		
		/*Set<OWLDisjointClassesAxiom> disjoint_axioms = getOwlOntology().getAxioms(AxiomType.DISJOINT_CLASSES);
		
		for(OWLDisjointClassesAxiom disjoint: disjoint_axioms){
			String id1 = null;
			String id2 = null;
			for(OWLClassExpression ce: disjoint.getClassExpressionsAsList()){
				if(id1 == null)
					id1 = graphWrapper.getIdentifier(ce);
				else if(id1 != null && id2 == null){
					id2 = graphWrapper.getIdentifier(ce);
					if(id2 != null){
						disjoint_withDumper.dumpRow(id1, id2, ontologyId);
					}
				}
			}
		}*/
		
		// TODO - disjoint_with
		
		
		allSomeRelationshipDumper.close();
		subClassOfDumper.close();
		
		List<String> list = new ArrayList<String>();
		
		
		list.add(allSomeRelationshipDumper.getTable());
		list.add(subClassOfDumper.getTable());
		
		return list;
	}
	
	private String oboId(OWLObject ob) {
		return graphWrapper.getIdentifier(ob);
	}
	

}

package org.geneontology.gold.io;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.Vector;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDisjointObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedObject;
import org.semanticweb.owlapi.model.OWLNaryBooleanClassExpression;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectUnionOf;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;
import org.semanticweb.owlapi.model.OWLSubObjectPropertyOfAxiom;
import org.semanticweb.owlapi.model.OWLSubPropertyChainOfAxiom;
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

	
	private void initTables() throws IOException{
		tables.put("ontology",  new TableDumper(this.dumpFilePrefix + "ontology", this.path));
		tables.put("ontology_annotation",  new TableDumper(this.dumpFilePrefix + "ontology_annotation", this.path));
		
		tables.put("obj_alternate_label",  new TableDumper(this.dumpFilePrefix + "obj_alternate_label", this.path));

		
		tables.put("obj_definition_xref",   new TableDumper(this.dumpFilePrefix + "obj_definition_xref", this.path));

		tables.put("obj_xref",   new TableDumper(this.dumpFilePrefix + "obj_xref", this.path));

		
		tables.put("cls_intersection_of",   new TableDumper(this.dumpFilePrefix + "cls_intersection_of", this.path));

		tables.put("cls_union_of",   new TableDumper(this.dumpFilePrefix + "cls_union_of", this.path));
		
		tables.put("equivalent_to",   new TableDumper(this.dumpFilePrefix + "equivalent_to", this.path));
		
		tables.put("disjoint_with",   new TableDumper(this.dumpFilePrefix + "disjoint_with", this.path));
		

		//tables.put("inferred_relationship",   new TableDumper(this.dumpFilePrefix + "inferred_relationship", this.path));
		

		tables.put("annotation_assertion",   new TableDumper(this.dumpFilePrefix + "annotation_assertion", this.path));
		
		tables.put("obj_alternate_id",   new TableDumper(this.dumpFilePrefix + "obj_alternate_id", this.path));
		
		tables.put("obj_subset",   new TableDumper(this.dumpFilePrefix + "obj_subset", this.path));
		
		tables.put("subrelation_of",   new TableDumper(this.dumpFilePrefix + "subrelation_of", this.path));
		
		tables.put("relation_disjoint_with",   new TableDumper(this.dumpFilePrefix + "relation_disjoint_with", this.path));
		
		tables.put("relation_equivalent_to",   new TableDumper(this.dumpFilePrefix + "relation_equivalent_to", this.path));
		
		tables.put("cls",   new TableDumper(this.dumpFilePrefix + "cls", this.path));
		
		tables.put("relation",   new TableDumper(this.dumpFilePrefix + "relation", this.path));
	
		tables.put("annotation_property",   new TableDumper(this.dumpFilePrefix + "annotation_property", this.path));
		
		
	}
	
	
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

		initTables();
		
		List<String> list = new Vector<String>();
		list.addAll(tables.keySet());
		
		
		dumpOntologyTable();
		dumpTermFrameAndMetaData();
		dumpTypeDefFrameAndMetaData();
		dumpLogicalAxioms();
		flushTables();
		return list;
	}
	
	public void dumpOntologyTable() throws IOException{
		TableDumper ontologyDumper = tables.get("ontology");
		TableDumper ontology_annotationDumper = tables.get("ontology_annotation");
	
		String id = graphWrapper.getOntologyId();
		Set<OWLAnnotationAssertionAxiom> anns = graphWrapper.getOntology().getAnnotationAssertionAxioms(graphWrapper.getOntology().getOntologyID().getOntologyIRI());
		
		String dt = null;
		for(OWLAnnotationAssertionAxiom ann: anns){
			
			if(ann.getValue() instanceof OWLLiteral){
				String value = ((OWLLiteral) ann.getValue()).getLiteral();
				String prop = graphWrapper.getIdentifier(ann.getProperty());
				if(prop.endsWith(":date")){
					dt = value;
				}else{

					ontology_annotationDumper.dumpRow(id, prop, 
							value);
				}
				
			}
			
		}

		IRI version = graphWrapper.getOntology().getOntologyID().getVersionIRI();
		String versionString = null;
		if(version != null)
			versionString = version.toString();
		
		
		ontologyDumper.dumpRow(id, id,  versionString,
				dt);
		
	}

	private void dumpAnnotations(OWLObject obj,String objId, String ontology)
	throws IOException{

		
		
		boolean builtin = graphWrapper.getBuiltin(obj);
		if(builtin){
			dumpAnnotation("builtin",  objId, builtin + "", ontology);
		}

		String createdBy = graphWrapper.getCreatedBy(obj);
		if(createdBy != null)
			dumpAnnotation("created_by",  objId, createdBy, ontology);
		
		
		boolean isObselete = graphWrapper.getIsObsolete(obj);
		if(isObselete)
			dumpAnnotation("is-obsolete",  objId, isObselete + "", ontology);

			
		String replacedBy = graphWrapper.getReplacedBy(obj);
		if(replacedBy != null)
			dumpAnnotation("replaced_by",  objId, replacedBy, ontology);
			

		String consider = graphWrapper.getConsider(obj);
		if(consider != null)
			dumpAnnotation("consider",  objId, consider, ontology);
		

		boolean isAnonymous = graphWrapper.getIsAnonymous(obj);
		if(isAnonymous)
			dumpAnnotation("is_anonymous",  objId, isAnonymous + "", ontology);
		
		if(obj instanceof OWLObjectProperty){
			OWLObjectProperty prop = (OWLObjectProperty)obj;
		
			String domain = graphWrapper.getDomain(prop);
			if(domain != null)
				dumpAnnotation("domain",  objId, domain, ontology);
	
			String range = graphWrapper.getRange(prop);
			if(range != null)
				dumpAnnotation("range",  objId, range, ontology);
			

			boolean isAntiSymmetric = graphWrapper.getIsAntiSymmetric(obj);
			if(isAntiSymmetric)
				dumpAnnotation("is_anti_symmetric",  objId, isAntiSymmetric + "", ontology);
				
			boolean isCyclic = graphWrapper.getIsCyclic(obj);
			if(isCyclic)
				dumpAnnotation("is_cyclic",  objId, isCyclic + "", ontology);

			boolean isFunctional = graphWrapper.getIsFunctional(prop);
			if(isFunctional)
				dumpAnnotation("is_functional",  objId, isFunctional + "", ontology);

			boolean isInverseFunctional = graphWrapper.getIsInverseFunctional(prop);
			if(isInverseFunctional)
				dumpAnnotation("is_inverse_functional",  objId, isInverseFunctional + "", ontology);
			
			
		}
		
	}
	
	private void dumpAlternateId(OWLObject obj, String id) 
			throws IOException{
		
		TableDumper table = tables.get("obj_alternate_id");
		String alt_id = graphWrapper.getAltId(obj);
		if(alt_id != null){
			table.dumpRow(id,alt_id);
		}
		
	}
	
	
	private void dumpAnnotation(String tag,String objId, String target_obj,  String ontology)
		throws IOException{
		String prop = graphWrapper.getIdentifier(  
			graphWrapper.getAnnotationProperty(tag)
			);

		TableDumper table = tables.get("annotation_assertion");
		table.dumpRow(prop, objId, target_obj, ontology );
	}

	private void dumpObjAlternateLabel(OWLObject obj, String id)
			throws IOException{
		
		TableDumper obj_alternate_labelDumper = tables.get("obj_alternate_label");
		
		for(String l: graphWrapper.getSynonymStrings(obj)){
			obj_alternate_labelDumper.dumpRow(id, l, null, null, null);
		}
	}

	
	private void dumpObjDefinitionXref(OWLObject obj, String id)
		throws IOException{
		
		TableDumper obj_definition_xref = tables.get("obj_definition_xref");
		
		for(String xref: graphWrapper.getDefXref(obj)){
			obj_definition_xref.dumpRow(id, xref);
		}
	
		
	}
	
	
	private void dumpObjXref(OWLObject obj, String id)
		throws IOException{
		
		TableDumper obj_xref = tables.get("obj_xref");
		
		for(String xref: graphWrapper.getXref(obj)){
			obj_xref.dumpRow(id, xref, null);
		}
		
		
	}
	
	
	private void dumpObjSubset(OWLObject obj, String id) throws IOException{
		String subset = graphWrapper.getSubset(obj);
		
		if(subset != null){
			TableDumper obj_subsetDumper = tables.get("obj_subset");
			obj_subsetDumper.dumpRow(id, subset);
		}
		
		
	}
	
	private void dumpTypeDefFrameAndMetaData() throws IOException{
		
		TableDumper relDumper = tables.get("relation");
		TableDumper annotation_propertyDumper =  tables.get("annotation_property");
		
		String ontologyId = graphWrapper.getOntologyId();
				
		for (OWLObjectProperty op : getOwlOntology().getObjectPropertiesInSignature()) {
			String label = graphWrapper.getLabel(op);
			String def = graphWrapper.getDef(op);
			String id = graphWrapper.getIdentifier(op);
			String comment = graphWrapper.getComment(op);
			String namespace = graphWrapper.getNamespace(op);

			boolean isTransitive = graphWrapper.getIsTransitive(op);
			boolean isReflexive = graphWrapper.getIsReflexive(op);
			boolean isSymmetric = graphWrapper.getIsSymmetric(op);
			
			boolean isMetaTag = graphWrapper.getIsMetaTag(op);
			
			if(isMetaTag){
				annotation_propertyDumper.dumpRow(id, label, ontologyId , namespace, comment, def, null);
			}else{
				relDumper.dumpRow(id, label, ontologyId , namespace, comment, def, isTransitive + "", 
					isSymmetric + "", isReflexive + "", null);
			}
			
			dumpObjAlternateLabel(op, id);
			
			dumpObjDefinitionXref(op, id);
			
			dumpAlternateId(op, id);
	
			dumpObjXref(op, id);
			
			dumpObjSubset(op, id);
			
			dumpAnnotations(op, id, ontologyId);
			
		}
	
	}
	
	private void dumpTermFrameAndMetaData() throws IOException {
		TableDumper clsDumper = tables.get("cls");
			//new TableDumper(this.dumpFilePrefix + "cls", this.path);
		
		
		TableDumper cls_intersection_ofDumper = tables.get("cls_intersection_of");

		TableDumper cls_union_ofDumper = tables.get("cls_union_of");
		
			//new TableDumper(this.dumpFilePrefix + "cls_union_of", this.path);
		
		TableDumper equivalent_toDumper = tables.get("equivalent_to");
			//new TableDumper(this.dumpFilePrefix + "equivalent_to", this.path);
		
		TableDumper disjoint_withDumper = tables.get("disjoint_with");
			//new TableDumper(this.dumpFilePrefix + "disjoint_with", this.path);
		
	////	TableDumper inferred_relationshipDumper = tables.get("inferred_relationship");
		
		//	TableDumper subclass_ofDumper = new TableDumper("subclass_of");
		//TableDumper allSomeRelationship = new TableDumper("all_some_relationship");
		boolean f = true;
		String ontologyId = graphWrapper.getOntologyId();
		for (OWLClass cls : getOwlOntology().getClassesInSignature()) {
			String label = graphWrapper.getLabel(cls);
			String def = graphWrapper.getDef(cls);
			String id = graphWrapper.getIdentifier(cls);
			String comment = graphWrapper.getComment(cls);
			String namespace = graphWrapper.getNamespace(cls);
			// textdef TODO
			clsDumper.dumpRow(id, label, ontologyId, namespace, comment, def, null);
			
			//dump synonms
			dumpObjAlternateLabel(cls, id);


			//dump alt_id tag
			dumpAlternateId(cls, id);
			

			dumpObjDefinitionXref(cls, id);

			dumpObjXref(cls, id);

			
			dumpObjSubset(cls, id);
			
			
			dumpAnnotations(cls, id, ontologyId);
			
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

			if(f){
				for(OWLObject ec: cls.getDisjointClasses(graphWrapper.getOntology())){
					String id2 = graphWrapper.getIdentifier(ec);
					if(id2 != null){
						disjoint_withDumper.dumpRow(id, id2, ontologyId);
					}
				}
				f = false;
			}
			
			
			/*
			//TODO: Inferred relationship
			Set<OWLGraphEdge> outgoing = graphWrapper.getOutgoingEdges(cls);
			for(OWLGraphEdge edge: outgoing){
				String targetId = graphWrapper.getIdentifier( edge.getTarget() );
				for(OWLQuantifiedProperty prop: edge.getQuantifiedPropertyList()){
					String propId = graphWrapper.getDef(prop.getProperty());
					
					inferred_relationshipDumper.dumpRow(id, targetId, propId, "true" , "false", ontologyId);
				}
			}
			
			
			Set<OWLGraphEdge> reflexive = graphWrapper.getOutgoingEdgesClosureReflexive(cls);
			
			reflexive.removeAll(outgoing);

			for(OWLGraphEdge edge: reflexive){
				String targetId = graphWrapper.getIdentifier( edge.getTarget() );
				for(OWLQuantifiedProperty prop: edge.getQuantifiedPropertyList()){
					String propId = graphWrapper.getDef(prop.getProperty());
					inferred_relationshipDumper.dumpRow(id, targetId, propId, "false" , "true", ontologyId);
				}
			}*/
			
		}
		
		
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
		
		TableDumper subrelation_ofDumper = tables.get("subrelation_of");
		for(OWLSubObjectPropertyOfAxiom ax: getOwlOntology().getAxioms(AxiomType.SUB_OBJECT_PROPERTY)){
			String subid = graphWrapper.getIdentifier(ax.getSubProperty());
			String supid = graphWrapper.getIdentifier(ax.getSuperProperty());
			
			if(subid != null & supid != null){
				subrelation_ofDumper.dumpRow(subid, supid, ontologyId);
			}
		}
		

		TableDumper relation_disjoint_withDumper = tables.get("relation_disjoint_with");
		for(OWLDisjointObjectPropertiesAxiom ax: getOwlOntology().getAxioms(AxiomType.DISJOINT_OBJECT_PROPERTIES)){
			boolean first = false;

			String prop1 = null;
			for(OWLObjectPropertyExpression prop :   ax.getProperties()){
				if(!first)
					prop1 = graphWrapper.getIdentifier(prop);
				else{
					if(prop1 != null){
						String prop2 = graphWrapper.getIdentifier(prop);
						relation_disjoint_withDumper.dumpRow(prop1, prop2, ontologyId);
					}
				}
			}
			
			
		}
		

		TableDumper relation_equivalent_toDumper = tables.get("relation_equivalent_to");
		for(OWLEquivalentObjectPropertiesAxiom ax: getOwlOntology().getAxioms(AxiomType.EQUIVALENT_OBJECT_PROPERTIES)){
			boolean first = false;

			String prop1 = null;
			for(OWLObjectPropertyExpression prop :   ax.getProperties()){
				if(!first)
					prop1 = graphWrapper.getIdentifier(prop);
				else{
					if(prop1 != null){
						String prop2 = graphWrapper.getIdentifier(prop);
						relation_equivalent_toDumper.dumpRow(prop1, prop2, ontologyId);
					}
				}
			}
		}
		
		
		TableDumper relation_chainDumper = tables.get("relation_chain");
		for(OWLSubPropertyChainOfAxiom ax: getOwlOntology().getAxioms(AxiomType.SUB_PROPERTY_CHAIN_OF)){
			boolean first = false;

			String supr = graphWrapper.getIdentifier(ax.getSuperProperty());
			String prop1 = null;
			for(OWLObjectPropertyExpression prop :   ax.getPropertyChain()){
				if(!first)
					prop1 = graphWrapper.getIdentifier(prop);
				else{
					if(prop1 != null){
						String prop2 = graphWrapper.getIdentifier(prop);
						relation_chainDumper.dumpRow(supr, prop1, prop2, null);
					}
				}
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

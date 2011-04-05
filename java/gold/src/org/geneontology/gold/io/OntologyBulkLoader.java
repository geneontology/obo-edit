package org.geneontology.gold.io;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import org.apache.log4j.Logger;
import org.obolibrary.obo2owl.Obo2OWLConstants;
import org.obolibrary.obo2owl.Owl2Obo;
import org.obolibrary.oboformat.model.Clause;
import org.obolibrary.oboformat.model.Frame;
import org.obolibrary.oboformat.parser.OBOFormatConstants;
import org.obolibrary.oboformat.parser.OBOFormatConstants.OboFormatTag;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDisjointClassesAxiom;
import org.semanticweb.owlapi.model.OWLDisjointObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLNamedObject;
import org.semanticweb.owlapi.model.OWLNaryBooleanClassExpression;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectAllValuesFrom;
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

	private static Logger LOG = Logger.getLogger(OntologyBulkLoader.class);
	
	
	private void initTables() throws IOException{
	
		tables.put("cls",   new TableDumper(this.dumpFilePrefix + "cls", this.path));
		tables.put("relation",   new TableDumper(this.dumpFilePrefix + "relation", this.path));
		tables.put("ontology",  new TableDumper(this.dumpFilePrefix + "ontology", this.path));
		tables.put("ontology_annotation",  new TableDumper(this.dumpFilePrefix + "ontology_annotation", this.path));
		tables.put("ontology_subset",  new TableDumper(this.dumpFilePrefix + "ontology_subset", this.path));
		tables.put("ontology_alternate_label_type",  new TableDumper(this.dumpFilePrefix + "ontology_alternate_label_type", this.path));
		
		tables.put("obj_alternate_label",  new TableDumper(this.dumpFilePrefix + "obj_alternate_label", this.path));

		
		tables.put("obj_definition_xref",   new TableDumper(this.dumpFilePrefix + "obj_definition_xref", this.path));

		tables.put("obj_xref",   new TableDumper(this.dumpFilePrefix + "obj_xref", this.path));

		
		tables.put("cls_intersection_of",   new TableDumper(this.dumpFilePrefix + "cls_intersection_of", this.path));

		tables.put("cls_union_of",   new TableDumper(this.dumpFilePrefix + "cls_union_of", this.path));
		
		tables.put("equivalent_to",   new TableDumper(this.dumpFilePrefix + "equivalent_to", this.path));
		
		tables.put("disjoint_with",   new TableDumper(this.dumpFilePrefix + "disjoint_with", this.path));
		

		tables.put("inferred_subclass_of",   new TableDumper(this.dumpFilePrefix + "inferred_subclass_of", this.path));
		tables.put("inferred_all_some_relationship",   new TableDumper(this.dumpFilePrefix + "inferred_all_some_relationship", this.path));
		tables.put("inferred_all_only_relationship",   new TableDumper(this.dumpFilePrefix + "inferred_all_only_relationship", this.path));
		tables.put("inferred_never_some_relationship",   new TableDumper(this.dumpFilePrefix + "inferred_never_some_relationship", this.path));
		

		tables.put("annotation_assertion",   new TableDumper(this.dumpFilePrefix + "annotation_assertion", this.path));
		
		tables.put("obj_alternate_id",   new TableDumper(this.dumpFilePrefix + "obj_alternate_id", this.path));
		
		tables.put("obj_subset",   new TableDumper(this.dumpFilePrefix + "obj_subset", this.path));
		
		tables.put("subrelation_of",   new TableDumper(this.dumpFilePrefix + "subrelation_of", this.path));
		
		tables.put("relation_disjoint_with",   new TableDumper(this.dumpFilePrefix + "relation_disjoint_with", this.path));
		
		tables.put("relation_equivalent_to",   new TableDumper(this.dumpFilePrefix + "relation_equivalent_to", this.path));
		
	
		tables.put("annotation_property",   new TableDumper(this.dumpFilePrefix + "annotation_property", this.path));
		
		tables.put("relation_chain",   new TableDumper(this.dumpFilePrefix + "relation_chain", this.path));

		tables.put("subclass_of",  new TableDumper(this.dumpFilePrefix + "subclass_of", path));
		tables.put("all_some_relationship", new TableDumper(this.dumpFilePrefix + "all_some_relationship", path));
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
		
		for(TableDumper table: tables.values()){
			list.add(table.getTable());
		}
		
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
		TableDumper ontology_subsetDumper = tables.get("ontology_subset");
		TableDumper ontology_alternate_label_typeDumper = tables.get("ontology_alternate_label_type");
		
		String id = graphWrapper.getOntologyId();
		Set<OWLAnnotationAssertionAxiom> anns = graphWrapper.getOntology().getAnnotationAssertionAxioms(graphWrapper.getOntology().getOntologyID().getOntologyIRI());
		
		String dt = null;
		for(OWLAnnotationAssertionAxiom ann: anns){
			
			if(ann.getValue() instanceof OWLLiteral){
				String value = ((OWLLiteral) ann.getValue()).getLiteral();
				//String prop = graphWrapper.getIdentifier(ann.getProperty());
				OboFormatTag tag = 	OBOFormatConstants.getTag(Owl2Obo.owlObjectToTag(ann.getProperty()));

				if( tag == OboFormatTag.TAG_DATE){
					dt = value;
				}else if(tag != null){

					ontology_annotationDumper.dumpRow(id, tag.getTag(), 
							value);
				}
				
			}
			
		}
		
		for(OWLClassAssertionAxiom ax: graphWrapper.getOntology().getAxioms(AxiomType.CLASS_ASSERTION)){

			
			//String clsId = graphWrapper.getIdentifier(ax.getClassExpression());
			
			OboFormatTag tag = OBOFormatConstants.getTag(Owl2Obo.owlObjectToTag(ax.getClassExpression()));
			
			if(tag == OboFormatTag.TAG_SYNONYMTYPEDEF){
	
				OWLNamedIndividual indv =(OWLNamedIndividual) ax.getIndividual();
				String indvId = graphWrapper.getIdentifier(indv);
				
				String nameValue = "";
				String scopeValue = null;
				for(OWLAnnotation ann: indv.getAnnotations(graphWrapper.getOntology())){
				//	String propId =graphWrapper.getIdentifier(ann.getProperty());
					//String propId = Owl2Obo.owlObjectToTag(ann.getProperty());
					
					OboFormatTag propTag = OBOFormatConstants.getTag( Owl2Obo.owlObjectToTag(ann.getProperty()) );
					
					String value = ((OWLLiteral) ann.getValue()).getLiteral();

					if(propTag == OboFormatTag.TAG_NAME){
						nameValue = "\"" +value + "\"";
					}else
						scopeValue = value;
				}
				

				ontology_alternate_label_typeDumper.dumpRow(indvId, nameValue, scopeValue);
					
				
			}else if(tag == OboFormatTag.TAG_SUBSETDEF ){

				OWLNamedIndividual indv =(OWLNamedIndividual) ax.getIndividual();
				String indvId = graphWrapper.getIdentifier(indv);
				
				String nameValue = "";
				for(OWLAnnotation ann: indv.getAnnotations(graphWrapper.getOntology())){
				//	String propId = Owl2Obo.owlObjectToTag(ann.getProperty());
					OboFormatTag propTag = OBOFormatConstants.getTag( Owl2Obo.owlObjectToTag(ann.getProperty()) );
					
					String value = ((OWLLiteral) ann.getValue()).getLiteral();

					if(propTag == OboFormatTag.TAG_NAME){
						nameValue = "\"" +value + "\"";
					}
					
					ontology_subsetDumper.dumpRow(indvId, nameValue);
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
			dumpAnnotation(OboFormatTag.TAG_BUILTIN.getTag(),  objId, builtin + "", ontology);
		}

		String createdBy = graphWrapper.getCreatedBy(obj);
		if(createdBy != null)
			dumpAnnotation(OboFormatTag.TAG_CREATED_BY.getTag(),  objId, createdBy, ontology);
		
		
	//	boolean isObselete = graphWrapper.getIsObsolete(obj);
	//	if(isObselete)
	//		dumpAnnotation(OboFormatTag.TAG_IS_OBSELETE.getTag(),  objId, isObselete + "", ontology);

			
		String replacedBy[] = graphWrapper.getReplacedBy(obj);
		if(replacedBy != null){
			for(String rb: replacedBy)
				dumpAnnotation(OboFormatTag.TAG_REPLACED_BY.getTag(),  objId, rb, ontology);
		}

		String consider = graphWrapper.getConsider(obj);
		if(consider != null)
			dumpAnnotation(OboFormatTag.TAG_CONSIDER.getTag(),  objId, consider, ontology);
		

		boolean isAnonymous = graphWrapper.getIsAnonymous(obj);
		if(isAnonymous)
			dumpAnnotation(OboFormatTag.TAG_IS_ANONYMOUS.getTag(),  objId, isAnonymous + "", ontology);
		
		if(obj instanceof OWLObjectProperty){
			OWLObjectProperty prop = (OWLObjectProperty)obj;
		
			String domain = graphWrapper.getDomain(prop);
			if(domain != null)
				dumpAnnotation(OboFormatTag.TAG_DOMAIN.getTag(),  objId, domain, ontology);
	
			String range = graphWrapper.getRange(prop);
			if(range != null)
				dumpAnnotation(OboFormatTag.TAG_RANGE.getTag(),  objId, range, ontology);
			

			boolean isAntiSymmetric = graphWrapper.getIsAntiSymmetric(obj);
			if(isAntiSymmetric)
				dumpAnnotation(OboFormatTag.TAG_IS_ANTI_SYMMETRIC.getTag(),  objId, isAntiSymmetric + "", ontology);
				
			boolean isCyclic = graphWrapper.getIsCyclic(obj);
			if(isCyclic)
				dumpAnnotation(OboFormatTag.TAG_IS_CYCLIC.getTag(),  objId, isCyclic + "", ontology);

			boolean isFunctional = graphWrapper.getIsFunctional(prop);
			if(isFunctional)
				dumpAnnotation(OboFormatTag.TAG_IS_FUNCTIONAL.getTag(),  objId, isFunctional + "", ontology);

			boolean isInverseFunctional = graphWrapper.getIsInverseFunctional(prop);
			if(isInverseFunctional)
				dumpAnnotation(OboFormatTag.TAG_IS_INVERSE_FUNCTIONAL.getTag(),  objId, isInverseFunctional + "", ontology);
			
			
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
		
		/*String prop = graphWrapper.getIdentifier(  
			graphWrapper.getAnnotationProperty(tag)
			);*/

		TableDumper table = tables.get("annotation_assertion");
		table.dumpRow(tag, objId, target_obj, ontology );
	}

	private void dumpObjAlternateLabel(OWLObject obj, String id)
			throws IOException{
		

		TableDumper obj_alternate_labelDumper = tables.get("obj_alternate_label");
		
		
		OWLEntity entity = (OWLEntity) obj;
		
		for(OWLAnnotationAssertionAxiom ax: entity.getAnnotationAssertionAxioms(this.getOwlOntology())){
			//String prop = graphWrapper.getIdentifier(ax.getProperty().getIRI());
			OboFormatTag tag = OBOFormatConstants.getTag(Owl2Obo.owlObjectToTag(ax.getProperty()));
			if(tag == OboFormatTag.TAG_SYNONYM){
				String synonym = ((OWLLiteral)ax.getValue()).getLiteral();
				String type = null;
				String scope = null;
				String xref = null;
				for(OWLAnnotation ann: ax.getAnnotations()){
					//String annProp = graphWrapper.getIdentifier(ann.getProperty().getIRI());
					String annProp = Owl2Obo.owlObjectToTag(ann.getProperty());
					if("xref".equals(annProp)){
						xref =  ((OWLLiteral)ann.getValue()).getLiteral();
					}else if("scope".equals(annProp)){
						scope =  ((OWLLiteral)ann.getValue()).getLiteral();
					}else if ("type".equals(annProp)){
						type =  ((OWLLiteral)ann.getValue()).getLiteral();
					}
				}
				
				obj_alternate_labelDumper.dumpRow(id, synonym, scope, type, xref);
			}
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
			boolean isObselete = graphWrapper.getIsObsolete(op);

			
			boolean isTransitive = graphWrapper.getIsTransitive(op);
			boolean isReflexive = graphWrapper.getIsReflexive(op);
			boolean isSymmetric = graphWrapper.getIsSymmetric(op);
			
			boolean isMetaTag = graphWrapper.getIsMetaTag(op);
			
			if(isMetaTag){
				annotation_propertyDumper.dumpRow(id, label, ontologyId , namespace, comment, def, isObselete + "");
			}else{
				relDumper.dumpRow(id, label, ontologyId , namespace, comment, def, isTransitive + "", 
					isSymmetric + "", isReflexive + "", isObselete + "");
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

		String ontologyId = graphWrapper.getOntologyId();
		for (OWLClass cls : getOwlOntology().getClassesInSignature()) {
			String label = graphWrapper.getLabel(cls);
			String def = graphWrapper.getDef(cls);
			String id = graphWrapper.getIdentifier(cls);
			String comment = graphWrapper.getComment(cls);
			String namespace = graphWrapper.getNamespace(cls);
			boolean isObselete = graphWrapper.getIsObsolete(cls);
			
			// textdef TODO
			clsDumper.dumpRow(id, label, ontologyId, namespace, comment, def, isObselete + "");
			
			//dump synonms
			dumpObjAlternateLabel(cls, id);


			//dump alt_id tag
			dumpAlternateId(cls, id);
			

			dumpObjDefinitionXref(cls, id);

			dumpObjXref(cls, id);

			
			dumpObjSubset(cls, id);
			
			
			dumpAnnotations(cls, id, ontologyId);
			
			TableDumper inferred_subclass_ofDumper = tables.get("inferred_subclass_of");
			TableDumper inferred_all_some_relationshipDumper= tables.get("inferred_all_some_relationship");
			TableDumper inferred_all_only_relationshipDumper =tables.get("inferred_all_only_relationship");
			TableDumper inferred_never_some_relationshipDumper=tables.get("inferred_never_some_relationship");
			
			
			//TODO: Inferred relationship
			Set<OWLGraphEdge> outgoing = graphWrapper.getOutgoingEdges(cls);
			for(OWLGraphEdge edge: outgoing){
				String targetId = graphWrapper.getIdentifier( edge.getTarget() );

				if(targetId == null || targetId.length()==0)
					continue;
				
				if(edge.getQuantifiedPropertyList().size()>1){
					//TODO: TBD
					LOG.warn("*******************TBD************************");
				}else{
					OWLQuantifiedProperty prop = edge.getSingleQuantifiedProperty();

					String propId = graphWrapper.getIdentifier(prop.getProperty());
					if( prop.getQuantifier() == OWLQuantifiedProperty.Quantifier.SOME){

						inferred_all_some_relationshipDumper.dumpRow(id, propId,  targetId, "true" , "false", "some" ,ontologyId);
					}else if( prop.getQuantifier() == OWLQuantifiedProperty.Quantifier.ONLY){

						inferred_all_only_relationshipDumper.dumpRow(id, propId,  targetId, "true" , "false", "only" ,ontologyId);
					}else if( prop.getQuantifier() == OWLQuantifiedProperty.Quantifier.SUBCLASS_OF)
						//TODO target UnionOf and IntersectionOf cases
						inferred_subclass_ofDumper.dumpRow(id, targetId, "true" , "false", "is_a", null ,ontologyId);
				}
				
				/*for(OWLQuantifiedProperty prop: edge.getQuantifiedPropertyList()){
					String propId = graphWrapper.getIdentifier(prop.getProperty());
					if( prop.getQuantifier() == OWLQuantifiedProperty.Quantifier.SOME){

						inferred_all_some_relationshipDumper.dumpRow(id, propId,  targetId, "true" , "false", "some" ,ontologyId);
					}else if( prop.getQuantifier() == OWLQuantifiedProperty.Quantifier.ONLY){

						inferred_all_only_relationshipDumper.dumpRow(id, propId,  targetId, "true" , "false", "only" ,ontologyId);
					}else if( prop.getQuantifier() == OWLQuantifiedProperty.Quantifier.SUBCLASS_OF)
						//TODO target UnionOf and IntersectionOf cases
						inferred_subclass_ofDumper.dumpRow(id, targetId, "true" , "false", "is_a", null ,ontologyId);
				
				}*/
				
			}
			
			
			Set<OWLGraphEdge> reflexive = graphWrapper.getOutgoingEdgesClosureReflexive(cls);
			
			//reflexive.removeAll(outgoing);

			for(OWLGraphEdge edge: reflexive){
			
				if(outgoing.contains(edge))
						continue;
				
				String targetId = graphWrapper.getIdentifier( edge.getTarget() );
				if(targetId == null || targetId.length()==0)
					continue;

				
				if(edge.getQuantifiedPropertyList().size()>1){
					//TODO: TBD
					LOG.warn("*******************More than one quantifiers properties are found-TBD************************\t"+ edge.getQuantifiedPropertyList());
				}else{
					OWLQuantifiedProperty prop = edge.getSingleQuantifiedProperty();

					String propId = graphWrapper.getIdentifier(prop.getProperty());
					if( prop.getQuantifier() == OWLQuantifiedProperty.Quantifier.SOME){
					
			
						inferred_all_some_relationshipDumper.dumpRow(id, propId,  targetId, "true"  ,"false", "some",ontologyId);
					}
					else if( prop.getQuantifier() == OWLQuantifiedProperty.Quantifier.ONLY){

						inferred_all_only_relationshipDumper.dumpRow(id, propId,  targetId, "true" , "false", "only", ontologyId);
					}else if( prop.getQuantifier() == OWLQuantifiedProperty.Quantifier.SUBCLASS_OF)
						//TODO target UnionOf and IntersectionOf cases
						inferred_subclass_ofDumper.dumpRow(id, targetId, "true" , "false", "is_a", null, ontologyId);
				}
				
				/*for(OWLQuantifiedProperty prop: edge.getQuantifiedPropertyList()){
					String propId = graphWrapper.getIdentifier(prop.getProperty());
					if( prop.getQuantifier() == OWLQuantifiedProperty.Quantifier.SOME){
					
						inferred_all_some_relationshipDumper.dumpRow(id, propId,  targetId, "true"  ,"false", "some",ontologyId);
					}
					else if( prop.getQuantifier() == OWLQuantifiedProperty.Quantifier.ONLY){

						inferred_all_only_relationshipDumper.dumpRow(id, propId,  targetId, "true" , "false", "only", ontologyId);
					}else if( prop.getQuantifier() == OWLQuantifiedProperty.Quantifier.SUBCLASS_OF)
						//TODO target UnionOf and IntersectionOf cases
						inferred_subclass_ofDumper.dumpRow(id, targetId, "true" , "false", "is_a", null, ontologyId);
				}*/
			}
			
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
	
	public void dumpLogicalAxioms() throws IOException {
	
		TableDumper subClassOfDumper = tables.get("subclass_of");
		TableDumper allSomeRelationshipDumper = tables.get("all_some_relationship");	
		
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
				if(!first){
					prop1 = graphWrapper.getIdentifier(prop);
					first = true;
				}else{
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
				if(!first){
					prop1 = graphWrapper.getIdentifier(prop);
					first = true;
				}else{
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
				if(!first){
					prop1 = graphWrapper.getIdentifier(prop);
					first = true;
				}else{
					if(prop1 != null){
						String prop2 = graphWrapper.getIdentifier(prop);
						relation_chainDumper.dumpRow(supr, prop1, prop2, null);
					}
				}
			}
		}
		
		TableDumper disjoint_withDumper = tables.get("disjoint_with");
		for(OWLDisjointClassesAxiom ax: getOwlOntology().getAxioms(AxiomType.DISJOINT_CLASSES)){
			List<OWLClassExpression> list= ax.getClassExpressionsAsList();
			String id1 = graphWrapper.getIdentifier(list.get(0));
			String id2= graphWrapper.getIdentifier(list.get(1));
			
			disjoint_withDumper.dumpRow(id1, id2, ontologyId);
		}
		
	
		TableDumper cls_intersection_ofDumper = tables.get("cls_intersection_of");
		TableDumper cls_union_ofDumper = tables.get("cls_union_of");
		TableDumper equivalent_toDumper = tables.get("equivalent_to");
		
		for(OWLEquivalentClassesAxiom ax: getOwlOntology().getAxioms(AxiomType.EQUIVALENT_CLASSES)){
			List<OWLClassExpression> list= ax.getClassExpressionsAsList();
			String id = graphWrapper.getIdentifier(list.get(0));

			OWLObject ec  = list.get(1);

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
	}
	
	private String oboId(OWLObject ob) {
		return graphWrapper.getIdentifier(ob);
	}
	

}

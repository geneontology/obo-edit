package org.geneontology.gold.hibernate.model;


/**
 * 
 * @author Shahid Manzoor
 *
 */
public class InferredRelationship extends GOModel implements java.io.Serializable {

	private String cls;
	private String targetCls;
	private String relation;
	private Boolean isDirect;
	private Boolean isReflexive;
	private String ontology;
	private String quantifier;
	
	private Cls clsObject;
	private Relation relationObject;
	private Cls targetClsObject;
	private Ontology ontologyObject;
	
	public InferredRelationship() {
		String uniqueKeys[] = {"cls", "targetCls", "relation", "ontology"};
		this.initUniqueConstraintFields(InferredRelationship.class, uniqueKeys);
	}

	public InferredRelationship(String cls, String targetCls,
			String relation, Boolean isDirect, Boolean isReflexive,
			String ontology, String quantifier) {
		this();
		this.cls = cls;
		this.targetCls = targetCls;
		this.relation = relation;
		this.isDirect = isDirect;
		this.isReflexive = isReflexive;
		this.ontology = ontology;
		this.quantifier = quantifier;
	}

	public String getQuantifier() {
		return quantifier;
	}

	public void setQuantifier(String quantifier) {
		this.quantifier = quantifier;
	}

	public String getCls() {
		return this.cls;
	}

	public void setCls(String cls) {
		this.cls = cls;
	}

	public String getTargetCls() {
		return this.targetCls;
	}

	public void setTargetCls(String targetCls) {
		this.targetCls = targetCls;
	}

	public String getRelation() {
		return this.relation;
	}

	public void setRelation(String relation) {
		this.relation = relation;
	}

	public Boolean getIsDirect() {
		return this.isDirect;
	}

	public void setIsDirect(Boolean isDirect) {
		this.isDirect = isDirect;
	}

	public Boolean getIsReflexive() {
		return this.isReflexive;
	}

	public void setIsReflexive(Boolean isReflexive) {
		this.isReflexive = isReflexive;
	}

	public String getOntology() {
		return this.ontology;
	}

	public void setOntology(String ontology) {
		this.ontology = ontology;
	}
	
	public Cls getClsObject() {
		if(clsObject == null && clsObject != null){
			clsObject = (Cls) getHibernateObject(Cls.class, "id", getCls());			
		}
		
		return clsObject;
	}

	public Relation getRelationObject() {
		if(relationObject == null && relationObject != null){
			relationObject =(Relation) getHibernateObject(Relation.class, "id", getRelation());
		}
		
		return relationObject;
	}

	public Cls getTargetClsObject() {
		if(targetClsObject == null && targetCls != null){
			targetClsObject = (Cls)getHibernateObject(Cls.class, "id", getTargetCls());
		}
		
		return targetClsObject;
	}


	public Ontology getOntologyObject() {
		if(ontologyObject == null && ontology != null){
			ontologyObject =(Ontology) getHibernateObject(Ontology.class, "id", this.ontology);
		}
		
		return ontologyObject;
	}
	

}

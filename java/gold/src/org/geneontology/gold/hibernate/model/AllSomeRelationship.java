package org.geneontology.gold.hibernate.model;

/**
 * 
 * @author Shahid Manzoor
 *
 */
public class AllSomeRelationship extends ExistentialRelation implements java.io.Serializable {

	public AllSomeRelationship() {
		super();
	}

	public AllSomeRelationship(String cls, String relation, String targetCls,
			String ontology) {
		super(cls, relation, targetCls, ontology);
	}

	
	
	/*private String cls;
	private String relation;
	private String targetCls;
	private String ontology;

	private Cls clsObject;
	private Relation relationObject;
	private Cls targetClsObject;
	private Ontology ontologyObject;

	public AllSomeRelationship() {
		String[] uniqueConstraintFields = {"cls", "relation", "targetCls", "ontology"};
		this.initUniqueConstraintFields(AllSomeRelationship.class, uniqueConstraintFields);
	}

	public AllSomeRelationship(String cls, String relation, String targetCls,
			String ontology) {
		this();
		this.cls = cls;
		this.relation = relation;
		this.targetCls = targetCls;
		this.ontology = ontology;
		
		
	}

	public String getCls() {
		return this.cls;
	}

	public void setCls(String cls) {
		this.cls = cls;
	}

	public String getRelation() {
		return this.relation;
	}

	public void setRelation(String relation) {
		this.relation = relation;
	}

	public String getTargetCls() {
		return this.targetCls;
	}

	public void setTargetCls(String targetCls) {
		this.targetCls = targetCls;
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
	}*/


}

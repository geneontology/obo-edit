package org.geneontology.gold.hibernate.model;

// Generated Nov 2, 2010 3:53:51 PM by Hibernate Tools 3.4.0.Beta1

/**
 * AnnotationAssertion generated by hbm2java
 */
public class AnnotationAssertion extends GOModel implements java.io.Serializable {

	private String relation;
	private String obj;
	private String targetObj;
	private String ontology;

	private Relation relationObject;
	private Ontology ontologyObject;
	
	
	public AnnotationAssertion() {
		String keys[] = {"relation", "obj", "targetObj", "ontology"};
		this.initUniqueConstraintFields(AnnotationAssertion.class, keys);
	}

	public AnnotationAssertion(String relation, String obj, String targetObj,
			String ontology) {
		this();
		this.relation = relation;
		this.obj = obj;
		this.targetObj = targetObj;
		this.ontology = ontology;
	}

	public String getRelation() {
		return this.relation;
	}

	public void setRelation(String relation) {
		this.relation = relation;
	}

	public String getObj() {
		return this.obj;
	}

	public void setObj(String obj) {
		this.obj = obj;
	}

	public String getTargetObj() {
		return this.targetObj;
	}

	public void setTargetObj(String targetObj) {
		this.targetObj = targetObj;
	}

	public String getOntology() {
		return this.ontology;
	}

	public void setOntology(String ontology) {
		this.ontology = ontology;
	}

	public Relation getRelationObject() {
		if(relationObject == null && relation != null){
			relationObject =(Relation) getHibernateObject(Relation.class, "id", relation);
		}
		
		return relationObject;
	}

	public Ontology getOntologyObject() {
		if(ontologyObject == null && ontology != null){
			ontologyObject = (Ontology) getHibernateObject(Ontology.class,"id", ontology);
		}
		
		
		return ontologyObject;
	}

	
	
}

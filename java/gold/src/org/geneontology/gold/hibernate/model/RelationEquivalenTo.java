package org.geneontology.gold.hibernate.model;

import java.io.Serializable;

public class RelationEquivalenTo extends GOModel implements Serializable {

	private String relation;
	private String equivalentRelation;
	private String ontology;
	
	private Relation relationObject;
	private Relation equivalentRelationObject;
	private Ontology ontologyObject;
	
	public RelationEquivalenTo(){
		String keys[] = {"relation", "equivalentRelation", "ontology"};
		this.initUniqueConstraintFields(RelationEquivalenTo.class, keys);
	}
	
	
	public RelationEquivalenTo(String relation, String equivalentRelation,
			String ontology) {
		this();
		this.relation = relation;
		this.equivalentRelation = equivalentRelation;
		this.ontology = ontology;
	}
	public String getRelation() {
		return relation;
	}
	public void setRelation(String relation) {
		this.relation = relation;
	}

	public String getOntology() {
		return ontology;
	}
	public void setOntology(String ontology) {
		this.ontology = ontology;
	}


	public String getEquivalentRelation() {
		return equivalentRelation;
	}


	public void setEquivalentRelation(String equivalentRelation) {
		this.equivalentRelation = equivalentRelation;
	}
	
	public Relation getRelationObject() {
		if(relationObject == null && relationObject != null){
			relationObject =(Relation) getHibernateObject(Relation.class, "id", getRelation());
		}
		
		return relationObject;
	}

	public Ontology getOntologyObject() {
		if(ontologyObject == null && ontology != null){
			ontologyObject =(Ontology) getHibernateObject(Ontology.class, "id", this.ontology);
		}
		
		return ontologyObject;
	}
	
	public Relation getEquivalentRelationObject() {
		if(equivalentRelationObject == null && equivalentRelation != null){
			equivalentRelationObject = (Relation) getHibernateObject(Relation.class, "id", equivalentRelation);
		}
		
		return equivalentRelationObject;
	}
	
	
}

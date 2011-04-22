package org.geneontology.gold.hibernate.model;

import java.io.Serializable;

/**
 * 
 * @author Shahid Manzoor
 *
 */
public class RelationDisjointWith extends GOModel implements Serializable {

	
	private String relation;
	private String disjointRelation;
	private String ontology;

	private Relation relationObject;
	private Relation disjointRelationObject;
	private Ontology ontologyObject;
	
	public RelationDisjointWith(){
		String keys[] = {"relation", "disjointRelation", "ontology"};
		this.initUniqueConstraintFields(RelationDisjointWith.class, keys);
	}
	
	
	public RelationDisjointWith(String relation, String disjointRelation,
			String ontology) {
		this();
		this.relation = relation;
		this.disjointRelation = disjointRelation;
		this.ontology = ontology;
	}
	public String getRelation() {
		return relation;
	}
	public void setRelation(String relation) {
		this.relation = relation;
	}
	public String getDisjointRelation() {
		return disjointRelation;
	}
	public void setDisjointRelation(String disjointRelation) {
		this.disjointRelation = disjointRelation;
	}
	public String getOntology() {
		return ontology;
	}
	public void setOntology(String ontology) {
		this.ontology = ontology;
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
	
	public Relation getDisjointRelationObject() {
		if(disjointRelationObject == null && disjointRelation != null){
			disjointRelationObject = (Relation) getHibernateObject(Relation.class, "id", disjointRelation);
		}
		
		return disjointRelationObject;
	}
	
	
}

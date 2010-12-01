package org.geneontology.gold.hibernate.model;

import java.io.Serializable;

/**
 * 
 * @author Shahid Manzoor
 *
 */
public class RelationDisjointWith extends GOModel implements Serializable {

	
	private Relation relation;
	private Relation disjointRelation;
	private Ontology ontology;
	
	public RelationDisjointWith(){
		String keys[] = {"relation", "disjointRelation", "ontology"};
		this.initUniqueConstraintFields(RelationDisjointWith.class, keys);
	}
	
	
	public RelationDisjointWith(Relation relation, Relation disjointRelation,
			Ontology ontology) {
		this();
		this.relation = relation;
		this.disjointRelation = disjointRelation;
		this.ontology = ontology;
	}
	public Relation getRelation() {
		return relation;
	}
	public void setRelation(Relation relation) {
		this.relation = relation;
	}
	public Relation getDisjointRelation() {
		return disjointRelation;
	}
	public void setDisjointRelation(Relation disjointRelation) {
		this.disjointRelation = disjointRelation;
	}
	public Ontology getOntology() {
		return ontology;
	}
	public void setOntology(Ontology ontology) {
		this.ontology = ontology;
	}
	
	
}

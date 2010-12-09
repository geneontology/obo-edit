package org.geneontology.gold.hibernate.model;

import java.io.Serializable;

public class RelationEquivalenTo extends GOModel implements Serializable {

	private Relation relation;
	private Relation equivalentRelation;
	private Ontology ontology;
	
	public RelationEquivalenTo(){
		String keys[] = {"relation", "equivalentRelation", "ontology"};
		this.initUniqueConstraintFields(RelationEquivalenTo.class, keys);
	}
	
	
	public RelationEquivalenTo(Relation relation, Relation equivalentRelation,
			Ontology ontology) {
		this();
		this.relation = relation;
		this.equivalentRelation = equivalentRelation;
		this.ontology = ontology;
	}
	public Relation getRelation() {
		return relation;
	}
	public void setRelation(Relation relation) {
		this.relation = relation;
	}

	public Ontology getOntology() {
		return ontology;
	}
	public void setOntology(Ontology ontology) {
		this.ontology = ontology;
	}


	public Relation getEquivalentRelation() {
		return equivalentRelation;
	}


	public void setEquivalentRelation(Relation equivalentRelation) {
		this.equivalentRelation = equivalentRelation;
	}
	
}

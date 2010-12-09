package org.geneontology.gold.hibernate.model;


/**
 * 
 * @author Shahid Manzoor
 *
 */
public class SubrelationOf extends GOModel implements java.io.Serializable {

	private Relation relation;
	private Relation superRelation;
	private Ontology ontology;

	public SubrelationOf() {
		String uniqueKeys[] = {"relation", "superRelation", "ontology"};
		this.initUniqueConstraintFields(SubrelationOf.class, uniqueKeys);
	}

	public SubrelationOf(Relation relation, Relation superRelation,
			Ontology ontology) {
		this();
		this.relation = relation;
		this.superRelation = superRelation;
		this.ontology = ontology;
	}

	public Relation getRelation() {
		return relation;
	}

	public void setRelation(Relation relation) {
		this.relation = relation;
	}

	public Relation getSuperRelation() {
		return superRelation;
	}

	public void setSuperRelation(Relation superRelation) {
		this.superRelation = superRelation;
	}

	public Ontology getOntology() {
		return ontology;
	}

	public void setOntology(Ontology ontology) {
		this.ontology = ontology;
	}

	
	

}

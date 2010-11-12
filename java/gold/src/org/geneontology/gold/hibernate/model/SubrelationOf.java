package org.geneontology.gold.hibernate.model;


/**
 * 
 * @author Shahid Manzoor
 *
 */
public class SubrelationOf extends GOModel implements java.io.Serializable {

	private String relation;
	private String superRelation;
	private String ontology;

	public SubrelationOf() {
		String uniqueKeys[] = {"relation", "superRelation", "ontology"};
		this.initUniqueConstraintFields(SubrelationOf.class, uniqueKeys);
	}

	public SubrelationOf(String relation, String superRelation,
			String ontology) {
		this();
		this.relation = relation;
		this.superRelation = superRelation;
		this.ontology = ontology;
	}

	public String getRelation() {
		return this.relation;
	}

	public void setRelation(String relation) {
		this.relation = relation;
	}

	public String getSuperRelation() {
		return this.superRelation;
	}

	public void setSuperRelation(String superRelation) {
		this.superRelation = superRelation;
	}

	public String getOntology() {
		return this.ontology;
	}

	public void setOntology(String ontology) {
		this.ontology = ontology;
	}

}

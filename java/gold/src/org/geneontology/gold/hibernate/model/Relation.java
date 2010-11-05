package org.geneontology.gold.hibernate.model;

// Generated Nov 4, 2010 4:36:17 PM by Hibernate Tools 3.4.0.Beta1

/**
 * Relation generated by hbm2java
 */
public class Relation extends GOModel implements java.io.Serializable {

	private String id;
	private String label;
	private Ontology ontology;
	private String oboNamespace;
	private String textComment;
	private String textDefinition;
	private Boolean isTransitive;
	private Boolean isSymmetric;
	private Boolean isReflexive;
	private Boolean isObsolete;

	public Relation() {
	}

	public Relation(String id) {
		this.id = id;
	}

	public Relation(String id, String label, Ontology ontology,
			String oboNamespace, String textComment, String textDefinition,
			Boolean isTransitive, Boolean isSymmetric, Boolean isReflexive,
			Boolean isObsolete) {
		this.id = id;
		this.label = label;
		this.ontology = ontology;
		this.oboNamespace = oboNamespace;
		this.textComment = textComment;
		this.textDefinition = textDefinition;
		this.isTransitive = isTransitive;
		this.isSymmetric = isSymmetric;
		this.isReflexive = isReflexive;
		this.isObsolete = isObsolete;
		
		String[] uniqueConstraintFields={"id"};
		this.initUniqueConstraintFields(Relation.class, uniqueConstraintFields);
	}

	public String getId() {
		return this.id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getLabel() {
		return this.label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public Ontology getOntology() {
		return this.ontology;
	}

	public void setOntology(Ontology ontology) {
		this.ontology = ontology;
	}

	public String getOboNamespace() {
		return this.oboNamespace;
	}

	public void setOboNamespace(String oboNamespace) {
		this.oboNamespace = oboNamespace;
	}

	public String getTextComment() {
		return this.textComment;
	}

	public void setTextComment(String textComment) {
		this.textComment = textComment;
	}

	public String getTextDefinition() {
		return this.textDefinition;
	}

	public void setTextDefinition(String textDefinition) {
		this.textDefinition = textDefinition;
	}

	public Boolean getIsTransitive() {
		return this.isTransitive;
	}

	public void setIsTransitive(Boolean isTransitive) {
		this.isTransitive = isTransitive;
	}

	public Boolean getIsSymmetric() {
		return this.isSymmetric;
	}

	public void setIsSymmetric(Boolean isSymmetric) {
		this.isSymmetric = isSymmetric;
	}

	public Boolean getIsReflexive() {
		return this.isReflexive;
	}

	public void setIsReflexive(Boolean isReflexive) {
		this.isReflexive = isReflexive;
	}

	public Boolean getIsObsolete() {
		return this.isObsolete;
	}

	public void setIsObsolete(Boolean isObsolete) {
		this.isObsolete = isObsolete;
	}

}

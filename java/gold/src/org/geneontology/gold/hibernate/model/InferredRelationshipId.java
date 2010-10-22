package org.geneontology.gold.hibernate.model;

// Generated Oct 22, 2010 9:25:51 AM by Hibernate Tools 3.4.0.Beta1

/**
 * InferredRelationshipId generated by hbm2java
 */
public class InferredRelationshipId implements java.io.Serializable {

	private String cls;
	private String targetCls;
	private String relation;
	private Boolean isDirect;
	private Boolean isReflexive;
	private String ontology;

	public InferredRelationshipId() {
	}

	public InferredRelationshipId(String cls, String targetCls,
			String relation, Boolean isDirect, Boolean isReflexive,
			String ontology) {
		this.cls = cls;
		this.targetCls = targetCls;
		this.relation = relation;
		this.isDirect = isDirect;
		this.isReflexive = isReflexive;
		this.ontology = ontology;
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

	public boolean equals(Object other) {
		if ((this == other))
			return true;
		if ((other == null))
			return false;
		if (!(other instanceof InferredRelationshipId))
			return false;
		InferredRelationshipId castOther = (InferredRelationshipId) other;

		return ((this.getCls() == castOther.getCls()) || (this.getCls() != null
				&& castOther.getCls() != null && this.getCls().equals(
				castOther.getCls())))
				&& ((this.getTargetCls() == castOther.getTargetCls()) || (this
						.getTargetCls() != null
						&& castOther.getTargetCls() != null && this
						.getTargetCls().equals(castOther.getTargetCls())))
				&& ((this.getRelation() == castOther.getRelation()) || (this
						.getRelation() != null
						&& castOther.getRelation() != null && this
						.getRelation().equals(castOther.getRelation())))
				&& ((this.getIsDirect() == castOther.getIsDirect()) || (this
						.getIsDirect() != null
						&& castOther.getIsDirect() != null && this
						.getIsDirect().equals(castOther.getIsDirect())))
				&& ((this.getIsReflexive() == castOther.getIsReflexive()) || (this
						.getIsReflexive() != null
						&& castOther.getIsReflexive() != null && this
						.getIsReflexive().equals(castOther.getIsReflexive())))
				&& ((this.getOntology() == castOther.getOntology()) || (this
						.getOntology() != null
						&& castOther.getOntology() != null && this
						.getOntology().equals(castOther.getOntology())));
	}

	public int hashCode() {
		int result = 17;

		result = 37 * result
				+ (getCls() == null ? 0 : this.getCls().hashCode());
		result = 37 * result
				+ (getTargetCls() == null ? 0 : this.getTargetCls().hashCode());
		result = 37 * result
				+ (getRelation() == null ? 0 : this.getRelation().hashCode());
		result = 37 * result
				+ (getIsDirect() == null ? 0 : this.getIsDirect().hashCode());
		result = 37
				* result
				+ (getIsReflexive() == null ? 0 : this.getIsReflexive()
						.hashCode());
		result = 37 * result
				+ (getOntology() == null ? 0 : this.getOntology().hashCode());
		return result;
	}

}

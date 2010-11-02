package org.geneontology.gold.hibernate.model;

// Generated Nov 2, 2010 3:53:51 PM by Hibernate Tools 3.4.0.Beta1

/**
 * NeverSomeRelationshipId generated by hbm2java
 */
public class NeverSomeRelationshipId implements java.io.Serializable {

	private String cls;
	private String targetCls;
	private String relation;
	private String ontology;

	public NeverSomeRelationshipId() {
	}

	public NeverSomeRelationshipId(String cls, String targetCls,
			String relation, String ontology) {
		this.cls = cls;
		this.targetCls = targetCls;
		this.relation = relation;
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
		if (!(other instanceof NeverSomeRelationshipId))
			return false;
		NeverSomeRelationshipId castOther = (NeverSomeRelationshipId) other;

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
				+ (getOntology() == null ? 0 : this.getOntology().hashCode());
		return result;
	}

}

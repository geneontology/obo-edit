package org.geneontology.gold.hibernate.model;

// Generated Oct 22, 2010 9:25:51 AM by Hibernate Tools 3.4.0.Beta1

/**
 * AnnotationAssertionId generated by hbm2java
 */
public class AnnotationAssertionId implements java.io.Serializable {

	private String relation;
	private String obj;
	private String targetObj;
	private String ontology;

	public AnnotationAssertionId() {
	}

	public AnnotationAssertionId(String relation, String obj, String targetObj,
			String ontology) {
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

	public boolean equals(Object other) {
		if ((this == other))
			return true;
		if ((other == null))
			return false;
		if (!(other instanceof AnnotationAssertionId))
			return false;
		AnnotationAssertionId castOther = (AnnotationAssertionId) other;

		return ((this.getRelation() == castOther.getRelation()) || (this
				.getRelation() != null && castOther.getRelation() != null && this
				.getRelation().equals(castOther.getRelation())))
				&& ((this.getObj() == castOther.getObj()) || (this.getObj() != null
						&& castOther.getObj() != null && this.getObj().equals(
						castOther.getObj())))
				&& ((this.getTargetObj() == castOther.getTargetObj()) || (this
						.getTargetObj() != null
						&& castOther.getTargetObj() != null && this
						.getTargetObj().equals(castOther.getTargetObj())))
				&& ((this.getOntology() == castOther.getOntology()) || (this
						.getOntology() != null
						&& castOther.getOntology() != null && this
						.getOntology().equals(castOther.getOntology())));
	}

	public int hashCode() {
		int result = 17;

		result = 37 * result
				+ (getRelation() == null ? 0 : this.getRelation().hashCode());
		result = 37 * result
				+ (getObj() == null ? 0 : this.getObj().hashCode());
		result = 37 * result
				+ (getTargetObj() == null ? 0 : this.getTargetObj().hashCode());
		result = 37 * result
				+ (getOntology() == null ? 0 : this.getOntology().hashCode());
		return result;
	}

}

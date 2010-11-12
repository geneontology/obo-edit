package org.geneontology.gold.hibernate.model;

/**
 * 
 * @author Shahid Manzoor
 *
 */
public class AllSomeRelationship extends GOModel implements java.io.Serializable {

	private Cls cls;
	private Relation relation;
	private Cls targetCls;
	private Ontology ontology;

	public AllSomeRelationship() {
		String[] uniqueConstraintFields = {"cls", "relation", "targetCls", "ontology"};
		this.initUniqueConstraintFields(AllSomeRelationship.class, uniqueConstraintFields);
	}

	public AllSomeRelationship(Cls cls, Relation relation, Cls targetCls,
			Ontology ontology) {
		this();
		this.cls = cls;
		this.relation = relation;
		this.targetCls = targetCls;
		this.ontology = ontology;
		
		
	}

	public Cls getCls() {
		return this.cls;
	}

	public void setCls(Cls cls) {
		this.cls = cls;
	}

	public Relation getRelation() {
		return this.relation;
	}

	public void setRelation(Relation relation) {
		this.relation = relation;
	}

	public Cls getTargetCls() {
		return this.targetCls;
	}

	public void setTargetCls(Cls targetCls) {
		this.targetCls = targetCls;
	}

	public Ontology getOntology() {
		return this.ontology;
	}

	public void setOntology(Ontology ontology) {
		this.ontology = ontology;
	}

	/*public boolean equals(Object other) {
		if ((this == other))
			return true;
		if ((other == null))
			return false;
		if (!(other instanceof AllOnlyRelationshipId))
			return false;
		AllSomeRelationship castOther = (AllSomeRelationship) other;

		return ((this.getCls() == castOther.getCls()) || (this.getCls() != null
				&& castOther.getCls() != null && this.getCls().equals(
				castOther.getCls())))
				&& ((this.getRelation() == castOther.getRelation()) || (this
						.getRelation() != null
						&& castOther.getRelation() != null && this
						.getRelation().equals(castOther.getRelation())))
				&& ((this.getTargetCls() == castOther.getTargetCls()) || (this
						.getTargetCls() != null
						&& castOther.getTargetCls() != null && this
						.getTargetCls().equals(castOther.getTargetCls())))
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
				+ (getRelation() == null ? 0 : this.getRelation().hashCode());
		result = 37 * result
				+ (getTargetCls() == null ? 0 : this.getTargetCls().hashCode());
		result = 37 * result
				+ (getOntology() == null ? 0 : this.getOntology().hashCode());
		return result;
	}*/

}

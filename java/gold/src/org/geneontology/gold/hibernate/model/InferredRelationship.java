package org.geneontology.gold.hibernate.model;


/**
 * 
 * @author Shahid Manzoor
 *
 */
public class InferredRelationship extends GOModel implements java.io.Serializable {

	private Cls cls;
	private Cls targetCls;
	private Relation relation;
	private Boolean isDirect;
	private Boolean isReflexive;
	private Ontology ontology;

	public InferredRelationship() {
		String uniqueKeys[] = {"cls", "targetCls", "relation", "ontology"};
		this.initUniqueConstraintFields(InferredRelationship.class, uniqueKeys);
	}

	public InferredRelationship(Cls cls, Cls targetCls,
			Relation relation, Boolean isDirect, Boolean isReflexive,
			Ontology ontology) {
		this();
		this.cls = cls;
		this.targetCls = targetCls;
		this.relation = relation;
		this.isDirect = isDirect;
		this.isReflexive = isReflexive;
		this.ontology = ontology;
	}

	public Cls getCls() {
		return this.cls;
	}

	public void setCls(Cls cls) {
		this.cls = cls;
	}

	public Cls getTargetCls() {
		return this.targetCls;
	}

	public void setTargetCls(Cls targetCls) {
		this.targetCls = targetCls;
	}

	public Relation getRelation() {
		return this.relation;
	}

	public void setRelation(Relation relation) {
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

	public Ontology getOntology() {
		return this.ontology;
	}

	public void setOntology(Ontology ontology) {
		this.ontology = ontology;
	}

}

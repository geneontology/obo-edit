package org.geneontology.gold.hibernate.model;


/**
 * 
 * @author Shahid Manzoor
 *
 */
public class NeverSomeRelationship extends GOModel implements java.io.Serializable {

	private Cls cls;
	private Cls targetCls;
	private Relation relation;
	private Ontology ontology;

	public NeverSomeRelationship() {
		String uniqueKeys[] = {"cls", "targetCls", "relation", "ontology"};
		this.initUniqueConstraintFields(NeverSomeRelationship.class, uniqueKeys);
	}

	public NeverSomeRelationship(Cls cls, Cls targetCls,
			Relation relation, Ontology ontology) {
		this();
		this.cls = cls;
		this.targetCls = targetCls;
		this.relation = relation;
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

	public Ontology getOntology() {
		return this.ontology;
	}

	public void setOntology(Ontology ontology) {
		this.ontology = ontology;
	}

}

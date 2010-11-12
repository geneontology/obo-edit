package org.geneontology.gold.hibernate.model;


/**
 * 
 * @author Shahid Manzoor
 *
 */
public class ClsIntersectionOf extends GOModel implements java.io.Serializable {

	private Cls cls;
	private Relation relation;
	private Cls targetCls;
	private Ontology ontology;

	public ClsIntersectionOf() {
		String uniqueKeys[] = {"cls", "relation", "targetCls", "ontology"};
		this.initUniqueConstraintFields(ClsIntersectionOf.class, uniqueKeys);
	}

	public ClsIntersectionOf(Cls cls, Relation relation, Cls targetCls,
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

}

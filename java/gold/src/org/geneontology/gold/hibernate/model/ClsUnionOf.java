package org.geneontology.gold.hibernate.model;


/**
 * 
 * @author Shahid Manzoor
 *
 */
public class ClsUnionOf extends GOModel implements java.io.Serializable {

	private Cls cls;
	private Cls targetCls;
	private Ontology ontology;

	public ClsUnionOf() {
		String uniqueKeys[] = {"cls", "targetCls", "ontology"};
		this.initUniqueConstraintFields(ClsUnionOf.class, uniqueKeys);
	}

	public ClsUnionOf(Cls cls, Cls targetCls, Ontology ontology) {
		this();
		this.cls = cls;
		this.targetCls = targetCls;
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

	public Ontology getOntology() {
		return this.ontology;
	}

	public void setOntology(Ontology ontology) {
		this.ontology = ontology;
	}

}

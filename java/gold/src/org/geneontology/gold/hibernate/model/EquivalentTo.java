package org.geneontology.gold.hibernate.model;

/**
 * 
 * @author Shahid Manzoor
 *
 */
public class EquivalentTo extends GOModel implements java.io.Serializable {

	private Cls cls;
	private Cls equivalentCls;
	private Ontology ontology;

	public EquivalentTo() {
		String uniqueKeys[] = {"cls", "equivalentCls", "ontology"};
		this.initUniqueConstraintFields(EquivalentTo.class, uniqueKeys);
	}

	public EquivalentTo(Cls cls, Cls equivalentCls, Ontology ontology) {
		this();
		this.cls = cls;
		this.equivalentCls = equivalentCls;
		this.ontology = ontology;
	}

	public Cls getCls() {
		return this.cls;
	}

	public void setCls(Cls cls) {
		this.cls = cls;
	}

	public Cls getEquivalentCls() {
		return this.equivalentCls;
	}

	public void setEquivalentCls(Cls equivalentCls) {
		this.equivalentCls = equivalentCls;
	}

	public Ontology getOntology() {
		return this.ontology;
	}

	public void setOntology(Ontology ontology) {
		this.ontology = ontology;
	}

}

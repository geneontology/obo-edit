package org.geneontology.gold.hibernate.model;


/**
 * 
 * @author Shahid Manzoor
 *
 */
public class DisjointWith extends GOModel implements java.io.Serializable {

	private Cls cls;
	private Cls  disjointCls;
	private Ontology ontology;

	
	public DisjointWith() {
		String[] uniqueKeys = {"cls, disjointCls, ontology"};
		this.initUniqueConstraintFields(DisjointWith.class, uniqueKeys);
	}

	public DisjointWith(Cls cls, Cls disjointCls, Ontology ontology) {
		this();
		this.cls = cls;
		this.disjointCls = disjointCls;
		this.ontology = ontology;
	}	
	
	public Cls getCls() {
		return this.cls;
	}

	public void setCls(Cls cls) {
		this.cls = cls;
	}

	public Cls getDisjointCls() {
		return this.disjointCls;
	}

	public void setDisjointCls(Cls disjointCls) {
		this.disjointCls = disjointCls;
	}

	public Ontology getOntology() {
		return this.ontology;
	}

	public void setOntology(Ontology ontology) {
		this.ontology = ontology;
	}
	
	

}

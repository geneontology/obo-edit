package org.geneontology.gold.hibernate.model;


/**
 * 
 * @author Shahid Manzoor
 *
 */
public class DisjointWith extends GOModel implements java.io.Serializable {

	private Ontology ontology;
	private Cls cls;
	private Cls disjointCls;
	
	
	public DisjointWith() {
		String[] uniqueConstraintFields = {"cls", "disjointCls", "ontology"};
		this.initUniqueConstraintFields(DisjointWith.class, uniqueConstraintFields);
	}

	public DisjointWith(Ontology ontology, Cls disjointCls,
			Cls cls) {

		this();
		this.cls = cls;
		this.disjointCls = disjointCls;
		this.ontology = ontology;
		
	}
	
	public Ontology getOntology() {
		return ontology;
	}

	public void setOntology(Ontology ontology) {
		this.ontology = ontology;
	}

	public Cls getCls() {
		return cls;
	}

	public void setCls(Cls cls) {
		this.cls = cls;
	}

	public Cls getDisjointCls() {
		return disjointCls;
	}

	public void setDisjointCls(Cls disjointCls) {
		this.disjointCls = disjointCls;
	}

	

}

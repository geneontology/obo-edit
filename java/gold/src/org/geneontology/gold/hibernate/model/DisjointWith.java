package org.geneontology.gold.hibernate.model;


/**
 * 
 * @author Shahid Manzoor
 *
 */
public class DisjointWith extends GOModel implements java.io.Serializable {

	private String cls;
	private String ontology;
	private String disjointCls;
	
	private Cls clsObject;
	private Ontology ontologyObject;
	private Cls disjointClsObject;
	
	public DisjointWith() {
		String[] uniqueConstraintFields = {"cls", "disjointCls", "ontology"};
		this.initUniqueConstraintFields(DisjointWith.class, uniqueConstraintFields);
	}

	public DisjointWith(String ontology, String disjointCls,
			String cls) {

		this();
		this.cls = cls;
		this.disjointCls = disjointCls;
		this.ontology = ontology;
		
	}
	
	public String getOntology() {
		return ontology;
	}

	public void setOntology(String ontology) {
		this.ontology = ontology;
	}

	public String getCls() {
		return cls;
	}

	public void setCls(String cls) {
		this.cls = cls;
	}

	public String getDisjointCls() {
		return disjointCls;
	}

	public void setDisjointCls(String disjointCls) {
		this.disjointCls = disjointCls;
	}

	public Cls getClsObject() {
		if(clsObject == null && clsObject != null){
			clsObject = (Cls) getHibernateObject(Cls.class, "id", getCls());			
		}
		
		return clsObject;
	}

	public Cls getDisjointClsObject() {
		if(disjointClsObject == null && disjointCls != null){
			disjointClsObject = (Cls)getHibernateObject(Cls.class, "id", disjointCls);
		}
		
		return disjointClsObject;
	}


	public Ontology getOntologyObject() {
		if(ontologyObject == null && ontology != null){
			ontologyObject =(Ontology) getHibernateObject(Ontology.class, "id", this.ontology);
		}
		
		return ontologyObject;
	}
	

}

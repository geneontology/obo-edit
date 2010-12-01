package org.geneontology.gold.hibernate.model;


/**
 * 
 * @author Shahid Manzoor
 *
 */
public class DisjointWith extends GOModel implements java.io.Serializable {

	private Ontology ontology;
	private Cls cls;
	private Cls superCls;
	
	
	public DisjointWith() {
	}

//	public SubclassOf(String cls, String superCls, String ontology, Ontology ontologyObject, Cls clsBySuperCls,
	//		Cls clsByCls) {
	public DisjointWith(Ontology ontology, Cls superCls,
			Cls cls) {

	//this.id = id;
		this.cls = cls;
		this.superCls = superCls;
		this.ontology = ontology;
		//this.ontologyObject = ontologyObject;
	//	this.clsBySuperCls = clsBySuperCls;
		//this.clsByCls = clsByCls;
		
		String[] uniqueConstraintFields = {"cls", "superCls", "ontology"};
		this.initUniqueConstraintFields(SubclassOf.class, uniqueConstraintFields);
	}
	
	
	

	/*public String getCls() {
		return this.cls;
	}

	public void setCls(String cls) {
		this.cls = cls;
	}

	public String getSuperCls() {
		return this.superCls;
	}

	public void setSuperCls(String superCls) {
		this.superCls = superCls;
	}

	public String getOntology() {
		return this.ontology;
	}

	public void setOntology(String ontology) {
		this.ontology = ontology;
	}*/

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

	public Cls getSuperCls() {
		return superCls;
	}

	public void setSuperCls(Cls superCls) {
		this.superCls = superCls;
	}

	
	
	/*public SubclassOfId getId() {
		return this.id;
	}

	public void setId(SubclassOfId id) {
		this.id = id;
	}*/

	/*public Ontology getOntologyObject() {
		return this.ontologyObject;
	}

	public void setOntologyObject(Ontology ontologyObject) {
		this.ontologyObject = ontologyObject;
	}

	public Cls getClsBySuperCls() {
		return this.clsBySuperCls;
	}

	public void setClsBySuperCls(Cls clsBySuperCls) {
		this.clsBySuperCls = clsBySuperCls;
	}

	public Cls getClsByCls() {
		return this.clsByCls;
	}

	public void setClsByCls(Cls clsByCls) {
		this.clsByCls = clsByCls;
	}*/

}

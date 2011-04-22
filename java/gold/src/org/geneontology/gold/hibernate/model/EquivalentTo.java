package org.geneontology.gold.hibernate.model;

/**
 * 
 * @author Shahid Manzoor
 *
 */
public class EquivalentTo extends GOModel implements java.io.Serializable {

	private String cls;
	private String equivalentCls;
	private String ontology;

	private Cls clsObject;
	private Cls equivalentClsObject;
	private Ontology ontologyObject;
	
	public EquivalentTo() {
		String uniqueKeys[] = {"cls", "equivalentCls", "ontology"};
		this.initUniqueConstraintFields(EquivalentTo.class, uniqueKeys);
	}

	public EquivalentTo(String cls, String equivalentCls, String ontology) {
		this();
		this.cls = cls;
		this.equivalentCls = equivalentCls;
		this.ontology = ontology;
	}

	public String getCls() {
		return this.cls;
	}

	public void setCls(String cls) {
		this.cls = cls;
	}

	public String getEquivalentCls() {
		return this.equivalentCls;
	}

	public void setEquivalentCls(String equivalentCls) {
		this.equivalentCls = equivalentCls;
	}

	public String getOntology() {
		return this.ontology;
	}

	public void setOntology(String ontology) {
		this.ontology = ontology;
	}

	public Cls getClsObject() {
		if(clsObject == null && clsObject != null){
			clsObject = (Cls) getHibernateObject(Cls.class, "id", getCls());			
		}
		
		return clsObject;
	}

	public Cls getEquivalentClsObject() {
		if(equivalentClsObject == null && equivalentCls != null){
			equivalentClsObject = (Cls)getHibernateObject(Cls.class, "id", equivalentCls);
		}
		
		return equivalentClsObject;
	}


	public Ontology getOntologyObject() {
		if(ontologyObject == null && ontology != null){
			ontologyObject =(Ontology) getHibernateObject(Ontology.class, "id", this.ontology);
		}
		
		return ontologyObject;
	}
	
	
}

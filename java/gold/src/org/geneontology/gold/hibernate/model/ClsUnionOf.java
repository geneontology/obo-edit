package org.geneontology.gold.hibernate.model;


/**
 * 
 * @author Shahid Manzoor
 *
 */
public class ClsUnionOf extends GOModel implements java.io.Serializable {

	private String cls;
	private String targetCls;
	private String ontology;

	private Cls clsObject;
	private Cls targetClsObject;
	private Ontology ontologyObject;

	public ClsUnionOf() {
		String uniqueKeys[] = {"cls", "targetCls", "ontology"};
		this.initUniqueConstraintFields(ClsUnionOf.class, uniqueKeys);
	}

	public ClsUnionOf(String cls, String targetCls, String ontology) {
		this();
		this.cls = cls;
		this.targetCls = targetCls;
		this.ontology = ontology;
	}

	public String getCls() {
		return this.cls;
	}

	public void setCls(String cls) {
		this.cls = cls;
	}

	public String getTargetCls() {
		return this.targetCls;
	}

	public void setTargetCls(String targetCls) {
		this.targetCls = targetCls;
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

	public Cls getTargetClsObject() {
		if(targetClsObject == null && targetCls != null){
			targetClsObject = (Cls)getHibernateObject(Cls.class, "id", getTargetCls());
		}
		
		return targetClsObject;
	}


	public Ontology getOntologyObject() {
		if(ontologyObject == null && ontology != null){
			ontologyObject =(Ontology) getHibernateObject(Ontology.class, "id", this.ontology);
		}
		
		return ontologyObject;
	}

}

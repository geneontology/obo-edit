package org.geneontology.gold.hibernate.model;

/**
 * 
 * @author Shahid Manzoor
 *
 */
public class Cls extends GOModel implements java.io.Serializable{

	private String id;
	private String label;
	private String ontology;
	private String oboNamespace;
	private String textComment;
	private String textDefinition;
	private Boolean isObsolete;

	private Ontology ontologyObject;

	public Cls() {
		String uniqueKeys[] = {"id"};
		this.initUniqueConstraintFields(Cls.class, uniqueKeys);
	}

	public Cls(String id) {
		this();
		this.id = id;
	}

	public Cls(String id, String label, String ontology, String oboNamespace,
			String textComment, String textDefinition, Boolean isObsolete){
			//,Set subclassOfsForCls, Set subclassOfsForSuperCls) {
		this.id = id;
		this.label = label;
		this.ontology = ontology;
		this.oboNamespace = oboNamespace;
		this.textComment = textComment;
		this.textDefinition = textDefinition;
		this.isObsolete = isObsolete;
	//	this.subclassOfsForCls = subclassOfsForCls;
	//	this.subclassOfsForSuperCls = subclassOfsForSuperCls;
	
		String[] uniqueConstraintFields = {"id"};
		this.initUniqueConstraintFields(Cls.class, uniqueConstraintFields);
	}

	public String getId() {
		return this.id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getLabel() {
		return this.label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public String getOntology() {
		return this.ontology;
	}

	public void setOntology(String ontology) {
		this.ontology = ontology;
	}

	public String getOboNamespace() {
		return this.oboNamespace;
	}

	public void setOboNamespace(String oboNamespace) {
		this.oboNamespace = oboNamespace;
	}

	public String getTextComment() {
		return this.textComment;
	}

	public void setTextComment(String textComment) {
		this.textComment = textComment;
	}

	public String getTextDefinition() {
		return this.textDefinition;
	}

	public void setTextDefinition(String textDefinition) {
		this.textDefinition = textDefinition;
	}

	public Boolean getIsObsolete() {
		return this.isObsolete;
	}

	public void setIsObsolete(Boolean isObsolete) {
		this.isObsolete = isObsolete;
	}

	public Ontology getOntologyObject() {
		if(ontologyObject == null && ontology != null){
			ontologyObject =(Ontology) getHibernateObject(Ontology.class, "id", this.ontology);
		}
		
		return ontologyObject;
	}
	

}

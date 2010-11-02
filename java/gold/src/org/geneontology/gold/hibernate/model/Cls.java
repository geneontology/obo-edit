package org.geneontology.gold.hibernate.model;

// Generated Nov 2, 2010 3:53:51 PM by Hibernate Tools 3.4.0.Beta1

import java.util.HashSet;
import java.util.Set;

/**
 * Cls generated by hbm2java
 */
public class Cls implements java.io.Serializable {

	private String id;
	private String label;
	private String ontology;
	private String oboNamespace;
	private String textComment;
	private String textDefinition;
	private Boolean isObsolete;
	private Set subclassOfsForSuperCls = new HashSet(0);
	private Set subclassOfsForCls = new HashSet(0);

	public Cls() {
	}

	public Cls(String id) {
		this.id = id;
	}

	public Cls(String id, String label, String ontology, String oboNamespace,
			String textComment, String textDefinition, Boolean isObsolete,
			Set subclassOfsForSuperCls, Set subclassOfsForCls) {
		this.id = id;
		this.label = label;
		this.ontology = ontology;
		this.oboNamespace = oboNamespace;
		this.textComment = textComment;
		this.textDefinition = textDefinition;
		this.isObsolete = isObsolete;
		this.subclassOfsForSuperCls = subclassOfsForSuperCls;
		this.subclassOfsForCls = subclassOfsForCls;
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

	public Set getSubclassOfsForSuperCls() {
		return this.subclassOfsForSuperCls;
	}

	public void setSubclassOfsForSuperCls(Set subclassOfsForSuperCls) {
		this.subclassOfsForSuperCls = subclassOfsForSuperCls;
	}

	public Set getSubclassOfsForCls() {
		return this.subclassOfsForCls;
	}

	public void setSubclassOfsForCls(Set subclassOfsForCls) {
		this.subclassOfsForCls = subclassOfsForCls;
	}

}


package org.geneontology.gold.hibernate.model;

import java.io.Serializable;

public class OntologyAlternateLabelType extends GOModel implements Serializable {

	private String id;
	private String defaultScope;
	private String description;
	
	public OntologyAlternateLabelType(){
		String keys [] = {"id"};
		this.initUniqueConstraintFields(OntologyAlternateLabelType.class, keys);
	}

	public OntologyAlternateLabelType(String id, String defaultScope,
			String description) {
		this();
		this.id = id;
		this.defaultScope = defaultScope;
		this.description = description;
	}



	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getDefaultScope() {
		return defaultScope;
	}

	public void setDefaultScope(String defaultScope) {
		this.defaultScope = defaultScope;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}
	
	
	
}

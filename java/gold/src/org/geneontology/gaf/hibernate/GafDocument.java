package org.geneontology.gaf.hibernate;

import java.io.Serializable;

import org.geneontology.gold.hibernate.model.GOModel;

public class GafDocument extends GOModel implements Serializable {

	private String id;
	private String documentPath;
	
	public GafDocument(){
		String keys [] = {"id"};
		this.initUniqueConstraintFields(GafDocument.class, keys);
		
	}
	
	public GafDocument(String id, String documentPath) {
		this();
		this.id = id;
		this.documentPath = documentPath;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getDocumentPath() {
		return documentPath;
	}

	public void setDocumentPath(String documentPath) {
		this.documentPath = documentPath;
	}
	
	
}

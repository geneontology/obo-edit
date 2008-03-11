package org.geneontology.refgenome.client.model;

import java.util.ArrayList;
import java.util.List;


import com.google.gwt.user.client.rpc.IsSerializable;


public class NodeDTO implements IsSerializable {
	/**
	 * 
	 */
	
	protected String id;
	protected String label;
	 /**
	   * @gwt.typeArgs <Statement>
	   */
	private List statements = new ArrayList();
	protected String sourceId;
	protected boolean isAnonymous;
	protected int metatype;
	

	public NodeDTO() {
	}

	public NodeDTO(String id) {
		this.id = id;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String name) {
		this.label = name;
	}
	

	public List getStatements() {
		return statements;
	}
	
		

	public void setStatements(List statements) {
		this.statements = statements;
	}
	
	public void addStatement(StatementDTO statement) {
		statements.add(statement);
	}
	
	public void addStatements(List newStatements) {
		statements.addAll(newStatements);
	}

	
	public String getSourceId() {
		return sourceId;
	}
	public void setSourceId(String sourceId) {
		this.sourceId = sourceId;
	}

	public boolean isAnonymous() {
		return isAnonymous;
	}

	public void setAnonymous(boolean isAnonymous) {
		this.isAnonymous = isAnonymous;
	}


	public String toString() {
		String s = id + " \""+label+"\"";
		if (isAnonymous)
			s = " ANON:"+id;
		if (sourceId != null)
			s = s + " src:"+sourceId;
		return s;
	}
	

	public int hashCode() {
		return id.hashCode();
	}
	
}

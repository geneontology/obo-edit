package org.geneontology.refgenome.client;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;

public class NodeDTO implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	public enum Metatype {CLASS, INSTANCE, RELATION};
	protected String id;
	protected String label;
	protected Collection<StatementDTO> statements;
	protected String sourceId;
	protected boolean isAnonymous;
	protected Metatype metatype;
	

	public NodeDTO() {
		statements = new HashSet<StatementDTO>();
	}

	public NodeDTO(String id) {
		this.id = id;
		statements = new HashSet<StatementDTO>();
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
	
	

	public Metatype getMetatype() {
		return metatype;
	}

	public void setMetatype(Metatype metatype) {
		this.metatype = metatype;
	}

	// WSDL-friendly : use arrays
	public StatementDTO[] getStatements() {
		return (StatementDTO[])statements.toArray(new StatementDTO[0]);
	}
	
		

	public void setStatements(Collection<StatementDTO> statements) {
		this.statements = statements;
	}
	
	public void addStatement(StatementDTO statement) {
		if (statements == null)
			statements = new HashSet<StatementDTO>();
		statements.add(statement);
	}
	
	public void addStatements(Collection<StatementDTO> newStatements) {
		if (statements == null)
			statements = new HashSet<StatementDTO>();
		statements.addAll(newStatements);
	}

	public void clearStatements() {
		statements = new LinkedList<StatementDTO>();
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

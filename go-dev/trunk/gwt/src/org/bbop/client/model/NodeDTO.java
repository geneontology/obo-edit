package org.bbop.client.model;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.bbop.client.model.StatementDTO;

import com.google.gwt.user.client.rpc.IsSerializable;


public class NodeDTO implements IsSerializable {
	/**
	 * 
	 */

	protected String id;
	protected String label;
	/**
	 * @gwt.typeArgs <org.bbop.client.model.StatementDTO>
	 */
	private List statements = new ArrayList();
	protected String sourceId;
	protected boolean isAnonymous;
	protected int metatype;
	protected NodeDTO inOrganismType;


	public NodeDTO() {
	}

	public NodeDTO(String id) {
		this.id = id;
	}



	public NodeDTO(String id, String label, String sourceId) {
		super();
		this.id = id;
		this.label = label;
		this.sourceId = sourceId;
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
	public void setStatements(StatementDTO[] newStatements) {
		this.statements = new ArrayList();
		addStatements(newStatements);

	}

	public void addStatement(StatementDTO statement) {
		statements.add(statement);
	}

	/**
	 * @gwt.typeArgs newStatements <org.bbop.client.model.StatementDTO>
	 */
	public void addStatements(List newStatements) {
		statements.addAll(newStatements);
	}

	public void addStatements(StatementDTO[] newStatements) {
		for (int i=0; i<newStatements.length; i++)
			statements.add(newStatements[i]);
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

	/**
	 * @param relationId
	 * @gwt.typeArgs <org.bbop.client.model.StatementDTO>
	 */
	public List getTargetIds(String relationId) {
		ArrayList idArr = new ArrayList();
		Iterator it = getStatements().iterator();
		while (it.hasNext()) {
			StatementDTO stmt = (StatementDTO) it.next();
			if (stmt.getRelationId().equals(relationId))
				idArr.add(stmt.getTargetId());
		}
		return idArr;
	}

	// TODO: move these out of generic Node object..
	public NodeDTO getInOrganismType() {
		return inOrganismType;
	}

	public void setInOrganismType(NodeDTO inOrganismType) {
		this.inOrganismType = inOrganismType;
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

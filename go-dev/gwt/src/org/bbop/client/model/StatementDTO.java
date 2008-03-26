package org.bbop.client.model;

import java.util.ArrayList;
import java.util.List;


import com.google.gwt.user.client.rpc.IsSerializable;

/**
 * @author cjm
 *
 */
public class StatementDTO extends NodeDTO implements IsSerializable {
	protected String nodeId;
	protected String relationId;
	protected String targetId;
	protected String positedByNodeId;
	protected String contextId;
	protected boolean isInferred;
	
	 /**
	   * @gwt.typeArgs <org.bbop.client.model.StatementDTO>
	   */
	protected List subStatements = new ArrayList();
	protected boolean appliesToAllInstancesOf;
	protected boolean intersectionSemantics;
	protected boolean unionSemantics;
//	protected InstanceQuantifier instanceQuantifier;
	
	public StatementDTO() {
		super();
	}
	public StatementDTO(String id) {
		super();
		nodeId = id;
	}
	public StatementDTO(String n, String r, String t) {
		super();
		nodeId = n;
		relationId = r;
		targetId = t;
	}

	public boolean isAppliesToAllInstancesOf() {
		return appliesToAllInstancesOf;
	}
	public void setAppliesToAllInstancesOf(boolean appliesToAllInstancesOf) {
		this.appliesToAllInstancesOf = appliesToAllInstancesOf;
	}
	
	public String getPositedByNodeId() {
		return positedByNodeId;
	}
	public void setPositedByNodeId(String positedByNodeId) {
		this.positedByNodeId = positedByNodeId;
	}
	/**
	 * @return the identifier of the node of which this statement is about
	 */
	public String getNodeId() {
		return nodeId;
	}
	public void setNodeId(String nodeId) {
		this.nodeId = nodeId;
	}
	/**
	 * @return the identifier of the relation node used in this statement
	 */
	public String getRelationId() {
		return relationId;
	}
	public void setRelationId(String relationId) {
		this.relationId = relationId;
	}
	/**
	 * @return the identifier of the relatum node
	 */
	public String getTargetId() {
		return targetId;
	}
	public void setTargetId(String targetId) {
		this.targetId = targetId;
	}
	
	public String getContextId() {
		return contextId;
	}
	public void setContextId(String contextId) {
		this.contextId = contextId;
	}
	
	
	public List getSubStatements() {
		return subStatements;
	}
	public void setSubStatements(List subStatements) {
		this.subStatements = subStatements;
	}
	public void addSubStatement(StatementDTO s) {
		subStatements.add(s);
	}
	
	public int hashCode() {
		return toString().hashCode();
	}

}

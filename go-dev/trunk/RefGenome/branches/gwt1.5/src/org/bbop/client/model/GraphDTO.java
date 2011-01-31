package org.bbop.client.model;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;


import com.google.gwt.user.client.rpc.IsSerializable;

/**
 * @author cjm
 *
 */
public class GraphDTO extends NodeDTO implements IsSerializable {

	 /**
	   * @gwt.typeArgs <java.lang.String,org.bbop.client.model.NodeDTO>
	   */
	protected Map nodeMap = new HashMap ();

	 /**
	   * @gwt.typeArgs <org.bbop.client.model.StatementDTO>
	   */
	protected Collection statements;
	
	 /**
	   * @gwt.typeArgs <java.lang.String,java.util.Collection<org.bbop.client.model.StatementDTO>>
	   */
	protected Map statementsByNodeMap = new HashMap ();

	
	public GraphDTO() {
		super();
		// TODO Auto-generated constructor stub
	}

	public GraphDTO(Collection statements) {
		super();
		this.statements = statements;
	}

	public NodeDTO[] getNodes() {
		return  (NodeDTO[]) nodeMap.values().toArray(new NodeDTO[0]);
	}

	public NodeDTO getNode(String id) {
		return (NodeDTO) (nodeMap.containsKey(id) ? nodeMap.get(id) : null);
	}


}

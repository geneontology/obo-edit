package org.geneontology.refgenome.client.model;

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
	   * @gwt.typeArgs <String,NodeDTO>
	   */
	protected Map nodeMap = new HashMap ();

	 /**
	   * @gwt.typeArgs <StatementDTO>
	   */
	protected Collection statements;
	
	 /**
	   * @gwt.typeArgs <String,Collection<StatementDTO>>
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

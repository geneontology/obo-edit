

package org.obd.ws.coreResource;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.obd.model.Graph;
import org.obd.model.Node;
import org.obd.model.Statement;
import org.obd.model.bridge.OBDJSONBridge;
import org.obd.model.bridge.OBDXMLBridge;
import org.obd.model.bridge.OBOBridge;
import org.obd.model.bridge.OWLBridge;
import org.obd.query.QueryTerm.Aspect;
import org.restlet.Context;
import org.restlet.data.MediaType;
import org.restlet.data.Request;
import org.restlet.data.Response;
import org.restlet.resource.Representation;
import org.restlet.resource.StringRepresentation;
import org.restlet.resource.Variant;

/**
 * Resource for a node
 * 
 * @author cjm
 */
public class NestedAnnotationResource extends NodeResource {

	protected String relationId;
	protected String aspect;
	protected String dataSource;
	
	/**
	 * Constructor.
	 * 
	 * @param context
	 *            The parent context.
	 * @param request
	 *            The request to handle.
	 * @param response
	 *            The response to return.
	 */
	public NestedAnnotationResource(Context context, Request request, Response response) {
		super(context, request, response);
		relationId = (String) request.getAttributes().get("relation");
		aspect = (String) request.getAttributes().get("aspect");
		this.dataSource = (String) request.getAttributes().get("dataSource");
		
		getVariants().clear();
		if (getNode() != null) {
			getVariants().add(new Variant(MediaType.TEXT_HTML));
		}
		
	}


	protected void iterativeFetch(Graph graph, String id, Map<String,Aspect> followRelationMap) {
		Collection<Statement> outStmts = getShard(this.dataSource).getStatementsForNode(id);
		Collection<Statement> inStmts = getShard(this.dataSource).getStatementsForTarget(id);
		Set<String> nodeIds = new HashSet<String>();
		for (Statement stmt : outStmts) {
			String relId = stmt.getRelationId();
			if (followRelationMap.containsKey(relId) &&
					followRelationMap.get(relId).equals(Aspect.TARGET)) {
				nodeIds.add(stmt.getTargetId());
			}
		}
		for (Statement stmt : inStmts) {
			String relId = stmt.getRelationId();
			if (followRelationMap.containsKey(relId) &&
					followRelationMap.get(relId).equals(Aspect.SELF)) {
				nodeIds.add(stmt.getNodeId());
			}
		}
		graph.addStatements(outStmts);
		graph.addStatements(inStmts);
		for (String nid : nodeIds)
			iterativeFetch(graph, nid, followRelationMap);
	}

	protected Graph getGraph() {
		Graph graph = new Graph();

		Map<String,Aspect> rmap =
			new HashMap<String,Aspect>();
		rmap.put("OBO_REL:descended_from", Aspect.SELF);
		//rmap.put("OBO_REL:in_organism", Aspect.TARGET);
		rmap.put("oboMetamodel:xref", Aspect.TARGET);
		rmap.put("OBO_REL:variant_of", Aspect.SELF);

		String id = getNodeId();
		iterativeFetch(graph, id, rmap);
		if (true) {
			String[] nids = graph.getReferencedNodeIds();
			for (String nid : nids) {
				Node n = getShard(this.dataSource).getNode(nid);
				graph.addNode(n);
			}
		}
		return graph;
	}

	@Override
	public Representation getRepresentation(Variant variant) {
		Representation result = null;

		//Collection<Statement> stmts = getStatements();
		Graph g =  getGraph();

		if (format == null) {
			format = "";
		}

		// TODO: DRY - general purpose converter rather than switches
		if (format.equals("json")) {
			result = new StringRepresentation(OBDJSONBridge.toJSON(g).toString());
			return result;

		}
		else if (format.equals("owl")) {
			result = new StringRepresentation(OWLBridge.toOWLString(g, uriString));
			return result;
		}
		else if (format.equals("obo")) {
			result = new StringRepresentation(OBOBridge.toOBOString(g));
			return result;
		}
		else if (format.equals("obdxml")) {
			result = new StringRepresentation(OBDXMLBridge.toXML(g), MediaType.TEXT_XML);
			return result;
		}
		else if (format.equals("view")) {
			TreeMap<String, Object> map = new TreeMap<String, Object>();
			map.put("graph", g);
			map.put("focusId",getNodeId());
			return getTemplateRepresentation("StatementsResourceView",map);
		}
		else {
			StringBuilder sb = new StringBuilder();
			sb.append("<pre>");
			sb.append("------------\n");
			sb.append("Statements\n");
			sb.append("------------\n\n");

			for (Statement s : g.getStatements()) {
				sb.append(hrefStatement(s,this.dataSource));
				sb.append("\n");
			}
			sb.append(hrefToOtherFormats("/nodes/"+getNodeId()+"/statements/"+aspect,this.dataSource));

			sb.append("</pre>");
			result = new StringRepresentation(sb, MediaType.TEXT_HTML);
		}

		return result;
	}



}

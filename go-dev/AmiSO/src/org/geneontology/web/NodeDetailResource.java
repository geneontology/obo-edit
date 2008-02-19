

package org.geneontology.web;

import java.util.Collection;
import java.util.TreeMap;

import org.obd.model.Graph;
import org.obd.model.Node;
import org.obd.model.Statement;
import org.obd.model.TermView;
import org.obd.model.bridge.OBDJSONBridge;
import org.obd.model.bridge.OBDXMLBridge;
import org.obd.model.bridge.OBOBridge;
import org.obd.model.bridge.OWLBridge;
import org.obd.query.LinkQueryTerm;
import org.obd.query.impl.OBOSessionShard;
import org.obd.ws.NodeResource;
import org.obo.datamodel.OBOSession;
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
public class NodeDetailResource extends NodeResource {

	protected String relationId;
	protected String aspect;

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
	public NodeDetailResource(Context context, Request request, Response response) {
		super(context, request, response);
		relationId = (String) request.getAttributes().get("relation");
		aspect = (String) request.getAttributes().get("aspect");
		getVariants().clear();
		if (getNode() != null) {
			getVariants().add(new Variant(MediaType.TEXT_HTML));
		}
	}

	protected Graph getGraph() { // TODO - DRY
		Graph graph;


		LinkQueryTerm lq = new LinkQueryTerm();
		lq.setNode(getNodeId());

		Collection<Statement> stmts = getShard().getStatementsByQuery(lq);
		lq = new LinkQueryTerm();
		lq.setTarget(getNodeId());
		stmts.addAll(getShard().getStatementsByQuery(lq));
		graph = new Graph(stmts);
		if (true) {
			String[] nids = graph.getReferencedNodeIds();
			for (String nid : nids) {
				Node n = getShard().getNode(nid);
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
			TermView termview = new TermView(g);
			map.put("graph", g);
			map.put("termview", termview);
			map.put("focusId",getNodeId());
			if (getShard() instanceof OBOSessionShard) {
				OBOSession session = ((OBOSessionShard)getShard()).getSession();
				
			}
			//return getTemplateRepresentation("NodeDetailView",map, "src/org/geneontology/web/pages/templates/");
			return getTemplateRepresentation("templates/NodeDetailView",map);
		}
		else {
			StringBuilder sb = new StringBuilder();
			sb.append("<pre>");
			sb.append("------------\n");
			sb.append("Statements\n");
			sb.append("------------\n\n");

			for (Statement s : g.getStatements()) {
				sb.append(hrefStatement(s));
				sb.append("\n");
			}
			sb.append(hrefToOtherFormats("/nodes/"+getNodeId()+"/statements/"+aspect));

			sb.append("</pre>");
			result = new StringRepresentation(sb, MediaType.TEXT_HTML);
		}

		return result;
	}



}



package org.obd.ws.coreResource;

import java.util.Collection;
import java.util.TreeMap;

import org.obd.model.Graph;
import org.obd.model.Node;
import org.obd.model.Statement;
import org.obd.model.bridge.OBDJSONBridge;
import org.obd.model.bridge.OBDXMLBridge;
import org.obd.model.bridge.OBOBridge;
import org.obd.model.bridge.OWLBridge;
import org.obd.query.AnnotationLinkQueryTerm;
import org.obd.query.LinkQueryTerm;
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
public class StatementsResource extends NodeResource {

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
	public StatementsResource(Context context, Request request, Response response) {
		super(context, request, response);
		relationId = (String) request.getAttributes().get("relation");
		aspect = (String) request.getAttributes().get("aspect");
		this.dataSource = (String) request.getAttributes().get("dataSource");
		getVariants().clear();
		if (getNode() != null) {
			getVariants().add(new Variant(MediaType.TEXT_HTML));
		}
	}



	protected Graph getGraph() {
		Graph graph;

		if (aspect == null || aspect.equals(""))
			aspect = "about";

		LinkQueryTerm lq = new LinkQueryTerm();
		if (relationId != null)
			lq.setRelation(relationId);
		if (aspect.equals("annotations")) {
			lq = new AnnotationLinkQueryTerm(getNodeId());
		}
		else {
			if (aspect.equals("about") || aspect.equals("all"))
				lq.setNode(getNodeId());
			else if (aspect.equals("to"))
				lq.setTarget(getNodeId());
			else if (aspect.equals("source"))
				lq.setSource(getNodeId());
			else
				lq.setNode(getNodeId());
		}

		Collection<Statement> stmts = getShard(this.dataSource).getStatementsByQuery(lq);

		if (aspect.equals("all")) {
			lq = new LinkQueryTerm();
			lq.setRelation(relationId);
			lq.setTarget(getNodeId());
			stmts.addAll(getShard(this.dataSource).getStatementsByQuery(lq));
		}
		graph = new Graph(stmts);
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
		} else if (format.equals("view")) {
			TreeMap<String, Object> map = new TreeMap<String, Object>();
			map.put("graph", g);
			map.put("focusId",getNodeId());
    		map.put("contextName", this.getContextName());
    		map.put("dataSource", this.dataSource);
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

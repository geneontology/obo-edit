
package org.obd.ws.coreResource;

import java.util.Collection;

import org.obd.model.Graph;
import org.obd.model.Node;
import org.obd.model.Statement;
import org.obd.model.bridge.OBDJSONBridge;
import org.obd.model.bridge.OBOBridge;
import org.obd.model.bridge.OWLBridge;
import org.obd.query.LinkQueryTerm;
import org.restlet.Context;
import org.restlet.data.MediaType;
import org.restlet.data.Request;
import org.restlet.data.Response;
import org.restlet.resource.Representation;
import org.restlet.resource.StringRepresentation;
import org.restlet.resource.Variant;

/**
 * Resource for a graph
 * 
 * @author cjm
 */
public class GraphResource extends StatementsResource {

	protected String dataSource;
    /**
     * Constructor.
     * 
     * @param context
     *                The parent context.
     * @param request
     *                The request to handle.
     * @param response
     *                The response to return.
     */
    public GraphResource(Context context, Request request, Response response) {
        super(context, request, response);
        this.dataSource = (String) request.getAttributes().get("dataSource");
    }

 
 
    @Override
    public Representation getRepresentation(Variant variant) {
    	Representation result = null;

    	//Collection<Statement> stmts = getStatements();
    	Graph graph = getGraph();
    	//Collection<Statement> stmts = graph.getStatements();
    	for (String nid : graph.getReferencedNodeIds()) {
    		Node n = getShard(this.dataSource).getNode(nid);   
    		graph.addNode(n);
    		LinkQueryTerm q = new LinkQueryTerm();
    		q.setNode(nid);
    		q.setInferred(false);
    		q.setIsAnnotation(false); // we already have the requested annotations
    		Collection<Statement> mdLinks = getShard(this.dataSource).getStatementsByQuery(q);
    		graph.addStatements(mdLinks);
    	}
    	//graph.addStatements(stmts);
    	graph.nestStatementsUnderNodes();

    	//LinkQueryTerm q = new LinkQueryTerm();
    	//q.setNode(getNodeId());
    	//Graph graph = getShard().getGraphByQuery(q, null, null);

    	if (format == null) {
    		format = "";
    	}

    	if (format.equals("json")) {
    		result = new StringRepresentation(OBDJSONBridge.toJSON(graph).toString());
    	}
    	else if (format.equals("obo")) {
    		result = new StringRepresentation(OBOBridge.toOBOString(graph));
    		return result;
    	}
    	else if (format.equals("owl")) {
    		result = new StringRepresentation(OWLBridge.toOWLString(graph));
    		return result;
    	}

    	else {
    		// Creates a text representation
    		StringBuilder sb = new StringBuilder();
    		sb.append("<pre>");
    		sb.append("------------\n");
    		sb.append("Graph details\n");
    		sb.append("------------\n\n");

    		for (Node n : graph.getNodes()) {
    			sb.append(href(n,this.dataSource));
    			sb.append("  ");
    			sb.append(n.getLabel());
    			sb.append("\n");
    			for (Statement s : n.getStatements()) {
    				sb.append("  ");
    				sb.append(hrefStatement(graph,s,this.dataSource));
    				sb.append("\n");
    			}
    		}
    		sb.append("\n");

    		sb.append("------------\n");
    		sb.append("Statements at graph level:\n");
    		sb.append("------------\n\n");
    		for (Statement s : graph.getStatements()) {
    			sb.append(hrefStatement(graph,s,this.dataSource));
    			sb.append("\n");
    		}
    		sb.append("</pre>");

    		result = new StringRepresentation(sb, MediaType.TEXT_HTML);
    	}
    	return result;
    }




  }

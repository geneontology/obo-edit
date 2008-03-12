

package org.obd.ws.coreResource;

import java.util.Collection;
import java.util.HashSet;

import org.obd.model.Node;
import org.obd.model.Statement;
import org.obd.model.bridge.OBDJSONBridge;
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
public class StatementsBySearchResource extends NodesBySearchResource {
	
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
     * @throws Exception 
     */
    public StatementsBySearchResource(Context context, Request request, Response response) throws Exception {
        super(context, request, response);
        aspect = (String) request.getAttributes().get("aspect");
        this.dataSource = (String) request.getAttributes().get("dataSource");
        
      }

    @Override
    public Representation getRepresentation(Variant variant) {
    	Representation result = null;

    	try {
			nodes = findNodes();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}

    	Collection<Statement> stmts =
    		new HashSet<Statement>();

    	if (aspect == null || aspect.equals(""))
    		aspect = "about";
    	for (Node node : nodes) {
    		String nodeId = node.getId();
    		LinkQueryTerm lq = new LinkQueryTerm();
    		if (relationId != null)
    			lq.setRelation(relationId);
    		if (aspect.equals("annotations")) {
    			lq = new AnnotationLinkQueryTerm(nodeId);
    		}
    		else {
    			if (aspect.equals("about") || aspect.equals("all"))
    				lq.setNode(nodeId);
    			else if (aspect.equals("to"))
    				lq.setTarget(nodeId);
    			else if (aspect.equals("from"))
    				lq.setSource(nodeId);
    			else
    				lq.setNode(nodeId);
    		}

    		stmts.addAll(getShard(this.dataSource).getStatementsByQuery(lq));

    		if (aspect.equals("all")) {
    			lq = new LinkQueryTerm();
    			lq.setRelation(relationId);
    			lq.setTarget(nodeId);
    			stmts.addAll(getShard(this.dataSource).getStatementsByQuery(lq));
    		}
    	}
    	if (format == null) {
    		format = "";
    	}

    	if (format.equals("json")) {
    		result = new StringRepresentation(OBDJSONBridge.toJSON(stmts).toString());
    		return result;

    	}
        else if (format.equals("owl")) {
        	result = new StringRepresentation(OWLBridge.toOWLString(stmts));
        	return result;
        }
        else if (format.equals("obo")) {
        	result = new StringRepresentation(OBOBridge.toOBOString(stmts));
        	return result;
        }
     	else {
    		//if (variant.getMediaType().equals(MediaType.TEXT_HTML)) {
    		StringBuilder sb = new StringBuilder();
    		sb.append("<pre>");
    		sb.append("------------\n");
    		sb.append("Statements\n");
    		sb.append("------------\n\n");

    		for (Statement s : stmts) {
    			sb.append(hrefStatement(s,this.dataSource));
    			sb.append("\n");
    		}
    		sb.append("</pre>");
    		result = new StringRepresentation(sb, MediaType.TEXT_HTML);
    	}

    	return result;
    }
    


}

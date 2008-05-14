

package org.obd.ws.coreResource;


import java.util.Collection;
import java.util.LinkedList;

import org.obd.model.Graph;
import org.obd.model.Node;
import org.obd.model.bridge.OBDJSONBridge;
import org.obd.model.bridge.OBDXMLBridge;
import org.obd.model.bridge.OBOBridge;
import org.obd.model.bridge.OWLBridge;
import org.obd.query.ComparisonQueryTerm.Operator;
import org.restlet.Context;
import org.restlet.data.MediaType;
import org.restlet.data.Reference;
import org.restlet.data.Request;
import org.restlet.data.Response;
import org.restlet.resource.Representation;
import org.restlet.resource.StringRepresentation;
import org.restlet.resource.Variant;

/**
 * Resource for a node
 */
public class NodesBySearchResource extends NodeResource {

    protected Collection<Node> nodes;
	protected String format;
	protected String searchTerm;
	protected String operatorString;
	protected String dataSource;

    public String getFormat() {
		return format;
	}


	public void setFormat(String format) {
		this.format = format;
	}


	/**
     * Constructor.
     * 
     * @param context
     *                The parent context.
     * @param request
     *                The request to handle.
     * @param response
     *                The response to return.
	 * @throws Exception 
     */
    public NodesBySearchResource(Context context, Request request, Response response) throws Exception {
        super(context, request, response);
        this.searchTerm = (String) request.getAttributes().get("term");
        this.searchTerm = Reference.decode(searchTerm);
        this.operatorString = (String) request.getAttributes().get("operator");
        this.dataSource = (String) request.getAttributes().get("dataSource");
        //format = request.getResourceRef().getQueryAsForm().getFirstValue("format");
        format = (String) request.getAttributes().get("format");
  
        this.nodes = findNodes();
        getVariants().add(new Variant(MediaType.TEXT_PLAIN));
    }

 
    /**
     * Finds the associated node.
     * 
     * @return The node found or null.
     * @throws Exception 
     */
    public Collection<Node> findNodes() throws Exception {
        nodes = new LinkedList<Node>();

        if (searchTerm != null) { 
        	Operator op = null;
        	if (operatorString != null) {
        		for (Operator currOp : Operator.values()) {
        			System.out.println(currOp.toString());
        			if (currOp.toString().equals(operatorString))
        				op = currOp;
        		}
        	}
        	//String[] searchTerms = searchTerm.split("\\s");
        	//for (String s : searchTerms) {
        		//if (op == null)
        			//nodes.addAll(getShard(this.dataSource).getNodesBySearch(s));
        		//else
        	if (op == null){
        		op = Operator.CONTAINS_ALL;
        	}
        	nodes.addAll(getShard(this.dataSource).getNodesBySearch(searchTerm, op));
        	//}
        }

        return nodes;
    }

 
    @Override
    public Representation getRepresentation(Variant variant) {
    	Representation result = null;

    	if (format == null) {
    		format = "";
    	}

    	Graph g = new Graph();
    	g.setNodes(nodes);
    	if (format.equals("json")) {
    		result = new StringRepresentation(OBDJSONBridge.nodesToJSON(nodes).toString());
    	}
    	else if (format.equals("obo")) {
    		result = new StringRepresentation(OBOBridge.toOBOString(g));
    		return result;
    	}
        else if (format.equals("obdxml")) {
        	result = new StringRepresentation(OBDXMLBridge.toXML(g), MediaType.TEXT_XML);
        	return result;
        }
     	else if (format.equals("owl")) {
    		result = new StringRepresentation(OWLBridge.toOWLString(g));
    		return result;
    	}
    	else {
    		// Creates a text representation
    		StringBuilder sb = new StringBuilder();
    		sb.append("<pre>");
    		sb.append("------------\n");
    		sb.append("Node list\n");
    		sb.append("------------\n\n");
    		sb.append("<table>\n");
    		for (Node node : nodes) {
    			sb.append("<tr>");
    			sb.append("<td>"+href(node,this.dataSource)+"</td>");
    			sb.append("<td>"+node.getLabel()+"</td>");
    			sb.append("<td>"+node.getSourceId()+"</td>");
    			sb.append("</tr>");

    		}
    		sb.append("</table>\n");
           	sb.append(hrefToOtherFormats("/search/"+operatorString+"/"+searchTerm,this.dataSource));
            
    		result = new StringRepresentation(sb, MediaType.TEXT_HTML);
    	}
    	return result;
    }
    
 
    

}

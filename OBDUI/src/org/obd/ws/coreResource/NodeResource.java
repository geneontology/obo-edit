
package org.obd.ws.coreResource;


import java.util.Collection;
import java.util.HashSet;
import java.util.TreeMap;

import org.obd.model.Node;
import org.obd.model.bridge.OBDJSONBridge;
import org.obd.model.bridge.OBOBridge;
import org.obd.model.bridge.OWLBridge;
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
public class NodeResource extends OBDResource {

    private Node node;
    private String nodeId;
	protected String format;
	protected String dataSource;
	protected String uriString;

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
     */
    public NodeResource(Context context, Request request, Response response) {
        
    	super(context, request, response);
        
        this.nodeId = (String) request.getAttributes().get("id");
        this.format = (String) request.getAttributes().get("format");
        this.dataSource = (String) request.getAttributes().get("dataSource");
        
        if (nodeId != null){
        	this.nodeId = Reference.decode(nodeId);
        }

        uriString = (String) request.getResourceRef().toString();
  
        this.node = findNode();

        if (node != null) {
            getVariants().add(new Variant(MediaType.TEXT_PLAIN));
        }
    }

 
    /**
     * Finds the associated node.
     * 
     * @return The node found or null.
     */
    public Node findNode() {
        Node result = null;

        System.out.print("Looking for Node " + nodeId + " in data source " + this.dataSource + ".");
        if (nodeId != null) {       	
        	result = getShard(this.dataSource).getNode(nodeId);
        } 
        
        
        return result;
    }
    
    public Collection<Node> findNodes() throws Exception {
    	HashSet<Node> nodes = new HashSet<Node>();
    	Node n = findNode();
    	if (n!= null)
    		nodes.add(n);
    	return nodes;
    }

 
    @Override
    public Representation getRepresentation(Variant variant) {
    	
    	
    	Representation result = null;

    	if (format == null) {
    		format = "";
    	}

    	if (format.equals("json")) {
    		result = new StringRepresentation(OBDJSONBridge.toJSON(this.node).toString());
    	}
    	else if (format.equals("obo")) {
    		result = new StringRepresentation(OBOBridge.toOBOString(node));
    		return result;
    	}
    	else if (format.equals("owl")) {
    		result = new StringRepresentation(OWLBridge.toOWLString(node));
    		return result;
    	}
    	else {
    		TreeMap<String, Object> resourceMap = new TreeMap<String, Object>();

    		
    		// Creates a text representation
    		/*
    		StringBuilder sb = new StringBuilder();
    		sb.append("<pre>");
    		sb.append("------------\n");
    		sb.append("Node details\n");
    		sb.append("------------\n\n");
    		sb.append("Id:  ").append(href(node,this.dataSource)).append('\n');
    		sb.append("Label: ").append(this.node.getLabel()).append("\n");
    		sb.append("Source: ").append(this.node.getSourceId()).append("\n\n");
    		sb.append("Statements: ").append(hrefStatementsFor(node.getId(),this.dataSource)).append("\n\n");
    		sb.append("Description: ").append(hrefDescriptionFor(node.getId(),this.dataSource)).append("\n\n");
    		sb.append("Graph: ").append(hrefGraph(node.getId(),this.dataSource)).append("\n\n");
           	sb.append(hrefToOtherFormats("/nodes/"+getNodeId(),this.dataSource));
            
    		sb.append("<pre>");
    		result = new StringRepresentation(sb, MediaType.TEXT_HTML);
    		*/
    		resourceMap.put("contextName", this.getContextName());
    		resourceMap.put("dataSource", this.dataSource);
    		resourceMap.put("formatLinks",mapToOtherFormats("/nodes/"+this.nodeId,this.dataSource));
    		return getTemplateRepresentation("NodeDetails",resourceMap);
    	}
    	return result;
    }
    
 

    /**
     * Returns the associated node.
     * 
     * @return The associated node.
     */
    public Node getNode() {
        return this.node;
    }

  
    /**
     * Sets the associated node.
     * 
     * @param node
     *                The node to set.
     */
    public void setNode(Node node) {
        this.node = node;
    }


	public String getNodeId() {
		return nodeId;
	}


	public void setNodeId(String nodeId) {
		this.nodeId = nodeId;
	}
    
    

}

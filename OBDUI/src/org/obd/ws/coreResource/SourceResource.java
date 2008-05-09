
package org.obd.ws.coreResource;


import org.obd.model.Node;
import org.obd.model.bridge.OBDJSONBridge;
import org.restlet.Context;
import org.restlet.data.MediaType;
import org.restlet.data.Request;
import org.restlet.data.Response;
import org.restlet.resource.Representation;
import org.restlet.resource.StringRepresentation;
import org.restlet.resource.Variant;

/**
 * Resource for a node
 */
public class SourceResource extends OBDResource {

    private Node node;
    private String nodeId;
	protected String format;
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
     */
    public SourceResource(Context context, Request request, Response response) {
        super(context, request, response);
        this.nodeId = (String) request.getAttributes().get("id");
        //format = request.getResourceRef().getQueryAsForm().getFirstValue("format");
        format = (String) request.getAttributes().get("format");
        this.dataSource = (String) request.getAttributes().get("dataSource");
  
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

        if (nodeId != null) {       	
        	result = getShard(this.dataSource).getNode(nodeId);    	
        }

        return result;
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
         else {
          	// Creates a text representation
        	StringBuilder sb = new StringBuilder();
        	sb.append("<pre>");
          	sb.append("------------\n");
          	sb.append("Node details\n");
        	sb.append("------------\n\n");
        	sb.append("Id:  ").append(href(node,dataSource)).append('\n');
          	sb.append("Label: ").append(this.node.getLabel()).append("\n");
          	String base = "sources/"+node.getId()+"/";
          	sb.append("Nodes: ").append(href(base+"/node",node.getId(),dataSource)).append("\n\n");
          	sb.append("Statements: ").append(href(base+"/statements",node.getId(),dataSource)).append("\n\n");
          	sb.append("Graph: ").append(href(base+"/graph",node.getId(),dataSource)).append("\n\n");
          	          	            
          	sb.append("<pre>");
          	result = new StringRepresentation(sb, MediaType.TEXT_HTML);
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

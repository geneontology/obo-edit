

package org.obd.ws.coreResource;


import java.util.Collection;

import org.obd.model.Graph;
import org.obd.model.Node;
import org.obd.model.bridge.OBDJSONBridge;
import org.obd.model.bridge.OBDXMLBridge;
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
public class SourcesResource extends OBDResource {

    private Collection<Node> sources;
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
     */
    public SourcesResource(Context context, Request request, Response response) {
        super(context, request, response);
        this.searchTerm = (String) request.getAttributes().get("term");
        this.operatorString = (String) request.getAttributes().get("operator");
        format = (String) request.getAttributes().get("format");
        this.dataSource = (String) request.getAttributes().get("dataSource");
        
        this.sources = getShard(this.dataSource).getSourceNodes();
        getVariants().add(new Variant(MediaType.TEXT_PLAIN));
    }

 
 
 
    @Override
    public Representation getRepresentation(Variant variant) {
        Representation result = null;
        
        if (format == null) {
        	format = "";
        }
    
        if (format.equals("json")) {
            result = new StringRepresentation(OBDJSONBridge.nodesToJSON(sources).toString());
        } else if (format.equals("obdxml")){
        	Graph g = new Graph();
        	g.setNodes(sources);
        	result = new StringRepresentation(OBDXMLBridge.toXML(g).toString());
        }
        
        else {
          	// Creates a text representation
        	StringBuilder sb = new StringBuilder();
        	sb.append("<pre>");
          	sb.append("------------\n");
          	sb.append("Source Node list\n");
           	sb.append("------------\n\n");
         	sb.append("<table>\n");
         	for (Node node : sources) {
         		sb.append("<tr>");
         		sb.append("<td>"+href(node,dataSource)+"</td>");
        		sb.append("<td>"+node.getLabel()+"</td>");
        		sb.append("</tr>");
         		
         	}
         	sb.append("</table>\n");
         	                   	
         	result = new StringRepresentation(sb, MediaType.TEXT_HTML);
        }
        return result;
    }
    
 
    

}

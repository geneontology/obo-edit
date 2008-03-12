

package org.obd.ws.coreResource;


import java.util.List;
import org.obd.model.stats.ScoredNode;
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
public class ScoredNodesResource extends NodeResource {
	
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
    public ScoredNodesResource(Context context, Request request, Response response) {
        super(context, request, response);
        this.dataSource = (String) request.getAttributes().get("dataSource");
        
        getVariants().clear();
        if (getNode() != null) {
            getVariants().add(new Variant(MediaType.TEXT_HTML));
        }
        
        
    }
    
    protected List<ScoredNode> getScoredNodes() {
       List<ScoredNode> scoredNodes = getShard(this.dataSource).getSimilarNodes(getNodeId());
          return scoredNodes;
    }

    @Override
    public Representation getRepresentation(Variant variant) {
        Representation result = null;

        List<ScoredNode> scoredNodes = getScoredNodes();
         
        if (format == null) {
        	format = "";
        }

          if (format.equals("json")) {
 //       	result = new StringRepresentation(OBDJSONBridge.toJSON(scoredNodes).toString());
  //      	return result;

        }
        else if (format.equals("owl")) {
 //       	result = new StringRepresentation(OWLBridge.toOWLString(scoredNodes));
  //      	return result;
        }
        else if (format.equals("obo")) {
 //       	result = new StringRepresentation(OBOBridge.toOBOString(scoredNodes));
  //      	return result;
        }
        else {
        	//if (variant.getMediaType().equals(MediaType.TEXT_HTML)) {
        	StringBuilder sb = new StringBuilder();
        	sb.append("<pre>");
        	sb.append("------------\n");
        	sb.append("Statements\n");
        	sb.append("------------\n\n");
          	 
        	for (ScoredNode sn : scoredNodes) {
        		sb.append(sn.getScore()+" :: "+href(sn.getNodeId(),dataSource));
        		sb.append("\n");
             }
        	sb.append(hrefToOtherFormats("/nodes/"+getNodeId()+"/blast/",this.dataSource));
        	 
         	sb.append("</pre>");
         	result = new StringRepresentation(sb, MediaType.TEXT_HTML);
        }

        return result;
    }
    


}

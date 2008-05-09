

package org.obd.ws.coreResource;

import org.obd.model.CompositionalDescription;
import org.obd.model.CompositionalDescription.Predicate;
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
 * 
 * @author cjm
 */
public class DescriptionResource extends NodeResource {
	
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
    public DescriptionResource(Context context, Request request, Response response) {
        super(context, request, response);
        getVariants().clear();
        if (getNode() != null) {
            getVariants().add(new Variant(MediaType.TEXT_HTML));
        }
        
        this.dataSource = (String) request.getAttributes().get("dataSource");
    }

    @Override
    public Representation getRepresentation(Variant variant) {
        Representation result = null;
   
        // TODO: expose option for full traversal
         CompositionalDescription desc = getShard(this.dataSource).getCompositionalDescription(getNodeId(), false);
          
           
        if (format == null) {
        	format = "";
        }
        if (format.equals("json")) {
        	result = new StringRepresentation(OBDJSONBridge.toJSON(desc).toString());
            return result;
     
        }
       	else if (format.equals("obo")) {
    		result = new StringRepresentation(OBOBridge.toOBOString(desc.getSourceGraph()));
    		return result;
    	}
    	else if (format.equals("owl")) {
    		result = new StringRepresentation(OWLBridge.toOWLString(desc.getSourceGraph()));
    		return result;
    	}

        else {
        	//if (variant.getMediaType().equals(MediaType.TEXT_HTML)) {
          	StringBuilder sb = new StringBuilder();
           	sb.append("<pre>");
           	sb.append("------------\n");
        	sb.append("Description\n");
        	sb.append("------------\n\n");
          	sb.append(toHTML(desc));
          	sb.append(hrefToOtherFormats("/node/"+getNodeId()+"/description",this.dataSource));
           	sb.append("</pre>");
         	result = new StringRepresentation(sb, MediaType.TEXT_HTML);
        }

        return result;
    }
    
    public String toHTML(CompositionalDescription desc) {
     	StringBuilder sb = new StringBuilder();
		if (desc.isAtomic())
			return href(desc.getNodeId(),this.dataSource);
		
     	sb.append(desc.getPredicate().toString());
      	sb.append("( ");
      	if (desc.getPredicate().equals(Predicate.RESTRICTION)) {
            	sb.append(desc.getRelationId());
         	sb.append(" SOME ");
         	
      	}
      	for (CompositionalDescription arg : desc.getArguments()) {
      		sb.append(toHTML(arg));
      		sb.append(" ");
      	}
      	sb.append(" )");
      	return sb.toString();   	
    }
    
     @Override
    public void handleGet() {
        // Make sure that the Uri ends with a "/" without changing the query.
        Reference ref = getRequest().getResourceRef();
        if (!ref.getPath().endsWith("/")) {
            ref.setPath(ref.getPath() + "/");
            getResponse().redirectPermanent(ref);
        } else {
            super.handleGet();
        }
    }

}

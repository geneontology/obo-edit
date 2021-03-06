package org.obd.ws.coreResource;


import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.TreeMap;

import org.apache.commons.lang.StringEscapeUtils;
import org.obd.model.bridge.OBDJSONBridge;
import org.obd.model.stats.ScoredNode;
import org.restlet.Context;
import org.restlet.data.MediaType;
import org.restlet.data.Reference;
import org.restlet.data.Request;
import org.restlet.data.Response;
import org.restlet.resource.Representation;
import org.restlet.resource.StringRepresentation;
import org.restlet.resource.Variant;

import freemarker.template.SimpleHash;

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
        /*
        getVariants().clear();
        if (getNode() != null) {
            getVariants().add(new Variant(MediaType.TEXT_HTML));
        }
        */
        
        
    }
    
    protected List<ScoredNode> getScoredNodes() {
    	List<ScoredNode> scoredNodes = getShard(this.dataSource).getSimilarNodes(getNodeId());
    	return scoredNodes;
    }

    @Override
    public Representation getRepresentation(Variant variant) {
        Representation result = null;
        this.findNode();
        Collection<ScoredNode> scoredNodes = getScoredNodes();
         
        if (format == null) {
        	format = "";
        }

        if (format.equals("json")) {
        	result = new StringRepresentation(OBDJSONBridge.scoredNodesToJSON(scoredNodes).toString());
        	return result;
        }
        else if (format.equals("owl")) {
 //       	result = new StringRepresentation(OWLBridge.toOWLString(scoredNodes));
 //       	return result;
        }
        else if (format.equals("obo")) {
 //       	result = new StringRepresentation(OBOBridge.toOBOString(scoredNodes));
  //      	return result;
        } else if (format.equals("html")){
        	
        	TreeMap<String, Object> resourceMap = new TreeMap<String, Object>();
    		resourceMap.put("contextName", this.getContextName());
    		resourceMap.put("dataSource", this.dataSource);
    		resourceMap.put("node",this.hashifyNode(this.getNode().getId(),"/" + this.getContextName() + "/" + dataSource + "/html/node/" + Reference.encode(this.getNode().getId())));
    		resourceMap.put("nodeId", StringEscapeUtils.escapeHtml(this.getNodeId()));
    		
        	
    		List<SimpleHash> scores = new ArrayList<SimpleHash>();
        	for (ScoredNode sn : scoredNodes) {
        		
        		SimpleHash scoredNodeHash = this.hashifyNode(sn.getNodeId(), ("/" + this.getContextName() + "/" + dataSource + "/html/node/" + Reference.encode(sn.getNodeId())));
        		Double score = new Double(sn.getScore());
        		score = -score;
        		scoredNodeHash.put("score", score.toString());
        		scoredNodeHash.put("spHref", ("/" + this.getContextName() + "/" + dataSource + "/html/similarityPair/" + Reference.encode(sn.getNodeId()) + "+" + Reference.encode(this.getNode().getId())));
        		
        		scores.add(scoredNodeHash);
        		
        		
        		
            }
        	if (scores.size()>0){
        		resourceMap.put("results", scores);
        	}
        	
        	return getTemplateRepresentation("ScoredNodeResult",resourceMap);
        	
        } else {
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
        	sb.append(hrefToOtherFormats("/node/"+getNodeId()+"/blast/",this.dataSource));
        	 
         	sb.append("</pre>");
         	result = new StringRepresentation(sb, MediaType.TEXT_HTML);
        }

        return result;
    }
    


}

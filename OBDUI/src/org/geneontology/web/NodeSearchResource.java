

package org.geneontology.web;


import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.obd.model.Graph;
import org.obd.model.Node;
import org.obd.model.bridge.OBDJSONBridge;
import org.obd.model.bridge.OBDXMLBridge;
import org.obd.model.bridge.OBOBridge;
import org.obd.model.bridge.OWLBridge;
import org.obd.query.ComparisonQueryTerm.Operator;
import org.obd.ws.coreResource.NodeResource;
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
public class NodeSearchResource extends NodeResource {

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
    public NodeSearchResource(Context context, Request request, Response response) throws Exception {
        super(context, request, response);
        this.searchTerm = (String) request.getAttributes().get("term");
        this.searchTerm = Reference.decode(searchTerm);
        this.operatorString = (String) request.getAttributes().get("operator");
        this.dataSource = (String) request.getAttributes().get("dataSource");
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
        			if (currOp.toString().equals(operatorString)){
        				op = currOp;
        			}
        		}
        	}

        	if (op == null) {
        		nodes.addAll(getShard(this.dataSource).getNodesBySearch(searchTerm));
        	} else {
        		nodes.addAll(getShard(this.dataSource).getNodesBySearch(searchTerm, op));
        	}
        	
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
    	} else {
    		
    		Map<String,String> categoryMap = this.groupingInit();
    		TreeMap<String, Object> resourceMap = new TreeMap<String, Object>();
    		Set<HashMap<String,String>> resultNodes = new HashSet<HashMap<String,String>>();
    		
    		
    		for (Node n : nodes){
    			
    			
    			if (n.getLabel() != null){
	    			HashMap<String, String> nodeProperties = new HashMap<String,String>();
	    			nodeProperties.put("id", n.getId());
	    			String label = "" + n.getLabel();
	    			nodeProperties.put("label", label);
	    			
	    			nodeProperties.put("linkedLabel",hrefLabel(label,n.getId(),this.dataSource));
	    			
	    			String source = "";
	    			source += n.getSourceId();
	    			nodeProperties.put("source",source);
	    			nodeProperties.put("linkedId", href(n,this.dataSource));
	    			if (categoryMap.keySet().contains(n.getSourceId())){
	    				nodeProperties.put("category", categoryMap.get(n.getSourceId()));
	    			} else {
	    				nodeProperties.put("category", "Other");
	    			}
	    			resultNodes.add(nodeProperties);
    			}
    			
    		}
    		
    		resourceMap.put("contextName", this.getContextName());
    		resourceMap.put("dataSource", this.dataSource);
    		resourceMap.put("nodeProperties", resultNodes);
    		resourceMap.put("formatLinks",mapToOtherFormats("/search/contains_all/"+searchTerm,this.dataSource));
    		
    		
    		return getTemplateRepresentation("searchResults",resourceMap);
    		
    	}
    	return result;
    }
    
    private Map<String,String> groupingInit(){
    	
    	Map<String,String> groups = new HashMap<String,String>();
    	
        groups.put("NCBI:gene", "Gene");
        groups.put("ncbi:gene", "Gene");
        groups.put("NCBIGene", "Gene");
        groups.put("MGI", "Gene");
        
        groups.put("fma", "Anatomical Part");
        groups.put("adult_mouse_anatomy.gxd", "Anatomical Part");
        groups.put("zebrafish_anatomy", "Anatomical Part");
        groups.put("fly_anatomy.ontology", "Anatomical Part");
        groups.put("ZFA","Anatomical Part");
        groups.put("cell", "Anatomical Part");
        
        groups.put("disease_ontology", "Disease");
        groups.put("NCBI:OMIM", "Disease");
        return groups;
    }
    

}

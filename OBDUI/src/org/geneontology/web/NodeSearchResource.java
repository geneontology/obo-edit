

package org.geneontology.web;


import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.lang.StringEscapeUtils;
import org.obd.model.Graph;
import org.obd.model.Node;
import org.obd.model.bridge.OBDJSONBridge;
import org.obd.model.bridge.OBDXMLBridge;
import org.obd.model.bridge.OBOBridge;
import org.obd.model.bridge.OWLBridge;
import org.obd.query.ComparisonQueryTerm.Operator;
import org.obd.query.LabelQueryTerm.AliasType;
import org.obd.ws.coreResource.NodeResource;
import org.obd.ws.coreResource.sorter.NodeLabelComparator;
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
 */
public class NodeSearchResource extends NodeResource {

    protected Collection<Node> nodes;
	protected String format;
	protected String searchTerm;
	protected String operatorString;
	protected String dataSource;
	protected String target;
	protected String source;
	
	

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
        this.target = (String) request.getAttributes().get("target");
        this.source = (String) request.getAttributes().get("source");
        this.format = (String) request.getAttributes().get("format");
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
        			//System.out.println(currOp.toString());
        			if (currOp.toString().equals(operatorString)){
        				op = currOp;
        			}
        		}
        	}

        	AliasType at = AliasType.ANY_LABEL;
        	if (this.target != null){
        		if (!this.target.equals("any")){
        			for (AliasType cat : AliasType.values()){
        				if (cat.toString().equals(this.target)){
        					at = cat;
        				}
        			}
        		}
        	}
        	
        	if (this.source.equals("all")||this.source.equals("")){
        		this.source = null;
        	}
        	        	
        	nodes.addAll(getShard(this.dataSource).getNodesBySearch(searchTerm,op,source,at));
        }

        return nodes;
    }

 
    @Override
    public Representation getRepresentation(Variant variant) {
    	
    	try {
			this.nodes = findNodes();
		} catch (Exception e1) {
			System.err.println("Can't  Find Nodes.");
			e1.printStackTrace();
		}
    	
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
    		
 
    		TreeMap<String, Object> resourceMap = new TreeMap<String, Object>();
    		List<SimpleHash> resultNodes = new ArrayList<SimpleHash>();
    		List<Node> nodeList = Arrays.asList(this.nodes.toArray(new Node[0]));
    		Map<String,Integer> categoryCount = new HashMap<String,Integer>();
    		
    		Collections.sort(nodeList,new NodeLabelComparator());
    		
    		
    		for (Node n : nodeList){
    			if (n.getLabel() != null){
    				
	    			SimpleHash nodeProperties = new SimpleHash();
	    			
	    			String nodeLabel = null;
	    			if (n.getLabel() != null){
	    				nodeLabel = n.getLabel();
	    			} else {
	    				nodeLabel = n.getId();
	    			}
	    			if (n != null && (n.getSourceId() != null) && n.getSourceId().equals("ZFIN")){
	    				nodeProperties.put("nodeId", n.getId());
	    				nodeProperties.put("nodeLabel", nodeLabel);
	    			} else {
	    				nodeProperties.put("nodeId", StringEscapeUtils.escapeHtml(n.getId()));
	    				nodeProperties.put("nodeLabel", StringEscapeUtils.escapeHtml(nodeLabel));
	    			}
	    			
	    			nodeProperties.put("nodeHref","/" + this.getContextName() + "/" + this.dataSource + "/html/node/" + Reference.encode(n.getId()));
	    			
	    			String source = "";
	    			source += n.getSourceId();
	    			
	    			nodeProperties.put("source",source);
	    			String category = this.getGroup(source);
	    			nodeProperties.put("category",category);
	    			if (!categoryCount.containsKey(category)){
	    				categoryCount.put(category, 0);
	    			}
	    			categoryCount.put(category,(categoryCount.get(category)+1));
	    			resultNodes.add(nodeProperties);
    			}
    		}
    		
    		List<SimpleHash> resultCounts = new ArrayList<SimpleHash>();
    		for (String category : categoryCount.keySet()){
    			SimpleHash count = new SimpleHash();
    			count.put("category", category);
    			count.put("count", categoryCount.get(category));
    			resultCounts.add(count);
    		}
    		if (resultCounts.size()>0){
    			resourceMap.put("resultCounts", resultCounts);
    		}
    		
    		String hostname = null;
    		try {
    			hostname = java.net.InetAddress.getLocalHost().getCanonicalHostName();
    		} catch (UnknownHostException e) {
    			// TODO Auto-generated catch block
    			e.printStackTrace();
    		}
    		resourceMap.put("hostname", hostname);
    		resourceMap.put("contextName", this.getContextName());
    		resourceMap.put("dataSource", this.dataSource);
    		resourceMap.put("resultNodes", resultNodes);
    		resourceMap.put("searchTerm",searchTerm);
    		
    		
    		return getTemplateRepresentation("searchResults",resourceMap);
    		
    	}
    	return result;
    }
    
    private String getGroup(String source){
    	
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
        
        if (groups.containsKey(source)){
        	return groups.get(source);
        } else {
        	return "Other";
        }
    }
    

}

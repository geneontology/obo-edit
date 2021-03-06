package org.obd.ws.coreResource;


import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.TreeMap;
import org.obd.model.Graph;
import org.obd.model.Node;
import org.obd.model.Statement;
import org.obd.model.bridge.OBDJSONBridge;
import org.obd.model.bridge.OBDXMLBridge;
import org.obd.model.bridge.OBOBridge;
import org.obd.model.bridge.OWLBridge;
import org.obd.query.AnnotationLinkQueryTerm;
import org.obd.query.BooleanQueryTerm;
import org.obd.query.LinkQueryTerm;
import org.obd.query.BooleanQueryTerm.BooleanOperator;
import org.obd.ws.coreResource.sorter.StatementHashComparator;
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
public class NodesResource extends NodeResource {

    protected List<Node> nodes;
	
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
    public NodesResource(Context context, Request request, Response response) {
        
    	super(context, request, response);
        this.nodes = new ArrayList<Node>();
        this.nodeString = (String) request.getAttributes().get("nodeList");
        if (this.nodeString != null){
        	//getVariants().add(new Variant(MediaType.TEXT_HTML));
        	
        	for (String uid : this.nodeString.split("\\+")){
        		uid = Reference.decode(uid);
        		System.out.println("Nodes List Contains: " + uid);
        		Node n = this.getShard(this.dataSource).getNode(uid);
        		if (n != null){
        			this.nodes.add(n);
        		}
        	}
        }
    }
 
    @Override
    public Representation getRepresentation(Variant variant) {
    	   	
    	Representation result = null;

    	if (format == null) {
    		format = "";
    	}

    	Graph g = new Graph();
    	
    	
    	g.setNodes(this.nodes);
    	if (format.equals("json")) {
    		result = new StringRepresentation(OBDJSONBridge.nodesToJSON(this.nodes).toString());
    	}  	else if (format.equals("obo")) {
    		result = new StringRepresentation(OBOBridge.toOBOString(g));
    		return result;
    	}  	else if (format.equals("owl")) {
    		result = new StringRepresentation(OWLBridge.toOWLString(g));
    		return result;
    	}	else if (format.equals("obdxml")){
    	    result = new StringRepresentation(OBDXMLBridge.toXML(g),MediaType.TEXT_XML);    
    	    return result;
    	} else if (format.equals("html")){
    		TreeMap<String, Object> resourceMap = new TreeMap<String, Object>();
    		resourceMap.put("contextName", this.getContextName());
    		resourceMap.put("dataSource", this.dataSource);
    		resourceMap.put("nodeString", this.nodeString);
    		resourceMap.put("encodedNodeString", Reference.encode(this.nodeString));
    		resourceMap.put("manyNodes",true);
    		try {
    			InetAddress addr = InetAddress.getLocalHost();
    			resourceMap.put("hostname",addr.getCanonicalHostName());
    		} catch (UnknownHostException e) {
    			System.err.println("Hostname fetching error: " + e.getMessage());
    		}
    		
    		// Annotation Statements 
    		List<SimpleHash> annotationStatements = this.getHashifiedStatements("annotation","html");
    		if (annotationStatements.size()>0){
    			Collections.sort(annotationStatements,new StatementHashComparator());
    			resourceMap.put("annotationStatements", annotationStatements);
    		}
    		
    		// Statements to Node
    		List<SimpleHash> toStatements = this.getHashifiedStatements("to","html");
    		if (toStatements.size()>0){
    			Collections.sort(toStatements,new StatementHashComparator());
    			resourceMap.put("toStatements", toStatements);
    		}
    		
    		// Statements about node
    		List<SimpleHash> aboutStatements = this.getHashifiedStatements("about","html");
    		if (aboutStatements.size()>0){
    			Collections.sort(aboutStatements,new StatementHashComparator());
    			resourceMap.put("aboutStatements", aboutStatements);
    		}
    		
    		List<SimpleHash> nodes = new ArrayList<SimpleHash>();
    		for (Node n : this.nodes){
    			nodes.add(this.hashifyNode(n.getId(), ("/" + this.getContextName() + "/" + this.dataSource + "/html/node/" + Reference.encode(n.getId()))));
    		}
    		resourceMap.put("nodes", nodes);
    		return getTemplateRepresentation("NodesDetails",resourceMap);
    		
    	} else {
    		// Creates a text representation
    		StringBuilder sb = new StringBuilder();
    		sb.append("<pre>");
    		sb.append("------------\n");
    		sb.append("Nodes details\n");
    		sb.append("------------\n\n");
    		for (Node node : this.nodes){
	    		sb.append("Id:  ").append(href(node,this.dataSource)).append('\n');
	    		sb.append("Label: ").append(node.getLabel()).append("\n");
	    		sb.append("Source: ").append(node.getSourceId()).append("\n\n");
	    		sb.append("Statements: ").append(hrefStatementsFor(node.getId(),this.dataSource)).append("\n\n");
	    		sb.append("Description: ").append(hrefDescriptionFor(node.getId(),this.dataSource)).append("\n\n");
	    		sb.append("Graph: ").append(hrefGraph(node.getId(),this.dataSource)).append("\n\n");
	           	sb.append(hrefToOtherFormats("/node/"+node.getId(),this.dataSource));
	           	sb.append("\n");
    		}
    		sb.append("</pre>");
    		return new StringRepresentation(sb, MediaType.TEXT_HTML);

    	}
    	return result;
    }
    
 

	@Override
	protected Graph getGraph(String aspect) {
		Graph graph;
		if (aspect == null || aspect.equals("")){
			aspect = "about";
		} else if (aspect.equals("all")){
			
		}

		BooleanQueryTerm bqt = new BooleanQueryTerm();
		bqt.setOperator(BooleanOperator.AND);
		
		for (Node n : this.nodes){
			LinkQueryTerm lq = new LinkQueryTerm();
			if (aspect.equals("annotation")) {
				lq = new AnnotationLinkQueryTerm(n.getId());
			} else {
				if (aspect.equals("about")){
					lq.setNode(n.getId());
					lq.setInferred(false);
				} else if (aspect.equals("all")){
					lq.setNode(n.getId());
				} else if (aspect.equals("to")){ 
					lq.setTarget(n.getId());
				} else if (aspect.equals("source")){
					lq.setSource(n.getId());
				} else {
					lq.setNode(n.getId());
				}
			}
			bqt.addQueryTerm(lq);
		}

		Collection<Statement> stmts = getShard(this.dataSource).getStatementsByQuery(bqt);

		if (aspect.equals("all")) {
			bqt = new BooleanQueryTerm();
			bqt.setOperator(BooleanOperator.AND);
			for (Node n : this.nodes){
				LinkQueryTerm lq = new LinkQueryTerm();
				lq.setTarget(n.getId());
				bqt.addQueryTerm(lq);
			}
			stmts.addAll(getShard(this.dataSource).getStatementsByQuery(bqt));
		}
		
		graph = new Graph(stmts);
		if (true) {
			Collection<String> nids = graph.getReferencedNodeIds();
			for (String nid : nids) {
				Node n = getShard(this.dataSource).getNode(nid);
				graph.addNode(n);
			}
		}
		return graph;
	}
	
}
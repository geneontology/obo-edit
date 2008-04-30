
package org.obd.ws.coreResource;


import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import org.obd.model.Graph;
import org.obd.model.LinkStatement;
import org.obd.model.LiteralStatement;
import org.obd.model.Node;
import org.obd.model.Statement;
import org.obd.model.bridge.OBDJSONBridge;
import org.obd.model.bridge.OBOBridge;
import org.obd.model.bridge.OWLBridge;
import org.obd.query.AnnotationLinkQueryTerm;
import org.obd.query.LinkQueryTerm;
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
        System.err.println("Node is: " + this.nodeId);
        if (this.nodeId != null){
        	this.nodeId = Reference.decode(this.nodeId);
        	System.out.println("Decoded is: " + this.nodeId);
        }
        this.format = (String) request.getAttributes().get("format");
        this.dataSource = (String) request.getAttributes().get("dataSource");
        
        if (nodeId != null){
        	this.nodeId = Reference.decode(nodeId);
        }

        uriString = (String) request.getResourceRef().toString();
  
        this.node = findNode();

        if (node != null) {
            getVariants().add(new Variant(MediaType.TEXT_HTML));
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
    	} else if (format.equals("html")||(format.equals("bioPortal"))){
    		TreeMap<String, Object> resourceMap = new TreeMap<String, Object>();
    		
    		resourceMap.put("contextName", this.getContextName());
    		resourceMap.put("dataSource", this.dataSource);
    		resourceMap.put("node",this.node);
    		//resourceMap.put("nodeId",this.nodeId);
    		resourceMap.put("encodedId", Reference.encode(this.nodeId));

    		try {
    			InetAddress addr = InetAddress.getLocalHost();
    			resourceMap.put("hostname",addr.getCanonicalHostName());
    		} catch (UnknownHostException e) {
    			System.err.println("Hostname fetching error: " + e.getMessage());
    		}
    		
    		// Annotation Statements 
    		List<SimpleHash> annotationStatements = new ArrayList<SimpleHash>();
    		for (Statement s : this.getGraph("annotations").getStatements()){
        		SimpleHash annotationStatement = new SimpleHash();
    			annotationStatement.put("statement", this.hashifyStatement(s));
        		Set<String> assignmentSources = new HashSet<String>();
    			Set<String> provenanceSources = new HashSet<String>();
    			for (Statement subStatement : s.getSubStatements()){
    				if (subStatement.getRelationId().equals("oban:assigned_by")){
    					assignmentSources.add(this.href(subStatement.getTargetId(),dataSource));
    				}
    				if (subStatement.getRelationId().equals("oban:has_data_source")){
    					provenanceSources.add(this.href(subStatement.getTargetId(),dataSource));
    				}
        			annotationStatement.put("assigned_by",assignmentSources);
        			annotationStatement.put("provenance",provenanceSources);
    			}
    			annotationStatements.add(annotationStatement);
    		}
    		if (annotationStatements.size()>0){
    			Collections.sort(annotationStatements,new StatementComparator());
    			resourceMap.put("annotationStatements", annotationStatements);
    		}
    		
    		
    		// Statements to Node
    		List<SimpleHash> toStatements = new ArrayList<SimpleHash>();
    		for (Statement s : this.getGraph("to").getStatements()){
    			SimpleHash m = new SimpleHash();
    			m.put("statement",this.hashifyStatement(s));
    			if (s.isInferred()){
    				m.put("entailment","I");
    			} else {
    				m.put("entailment","A");
    			}
    			toStatements.add(m);
    		}
    		if (toStatements.size()>0){
    			Collections.sort(toStatements,new StatementComparator());
    			resourceMap.put("toStatements", toStatements);
    		}
    		
    		// Statements about node
    		List<SimpleHash> aboutStatements = new ArrayList<SimpleHash>();
    		for (Statement s : this.getGraph("about").getStatements()){
    			SimpleHash m = new SimpleHash();
    			m.put("statement",this.hashifyStatement(s));
    			if (s.isInferred()){
    				m.put("entailment","I");
    			} else {
    				m.put("entailment","A");
    			}
    			aboutStatements.add(m);
    			
    			if ((s.getRelationId().equals("oboMetamodel:description")||s.getRelationId().equals("oboInOwl:hasDefinition")) && (s instanceof LiteralStatement)){
    				resourceMap.put("nodeDefinition",((LiteralStatement)s).getSValue());
    			}
    		}
    		if (aboutStatements.size()>0){
    			Collections.sort(aboutStatements,new StatementComparator());
    			resourceMap.put("aboutStatements", aboutStatements);
    		}
    		if (format.equals("html")){
    			return getTemplateRepresentation("NodeDetails",resourceMap);
    		} else {
    			return getTemplateRepresentation("BPNodeDetails",resourceMap);
    		}
    	} else {
    		

    		
    		// Creates a text representation
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
    		sb.append("</pre>");
    		return new StringRepresentation(sb, MediaType.TEXT_HTML);

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
    
	
	protected Graph getGraph(String aspect) {
		Graph graph;
		if (aspect == null || aspect.equals(""))
			aspect = "about";

		LinkQueryTerm lq = new LinkQueryTerm();
		if (aspect.equals("annotations")) {
			lq = new AnnotationLinkQueryTerm(getNodeId());
		} else {
			if (aspect.equals("about")){
				lq.setNode(getNodeId());
				lq.setInferred(false);
			} else if (aspect.equals("all")){
				lq.setNode(getNodeId());
			} else if (aspect.equals("to")){ 
				lq.setTarget(getNodeId());
			} else if (aspect.equals("source")){
				lq.setSource(getNodeId());
			} else {
				lq.setNode(getNodeId());
			}
		}

		Collection<Statement> stmts = getShard(this.dataSource).getStatementsByQuery(lq);

		if (aspect.equals("all")) {
			lq = new LinkQueryTerm();
			//lq.setRelation(relationId);
			lq.setTarget(getNodeId());
			stmts.addAll(getShard(this.dataSource).getStatementsByQuery(lq));
		}
		graph = new Graph(stmts);
		if (true) {
			String[] nids = graph.getReferencedNodeIds();
			for (String nid : nids) {
				Node n = getShard(this.dataSource).getNode(nid);
				graph.addNode(n);
			}
		}
		return graph;
	}
	
	public SimpleHash hashifyStatement(Statement s) {
				
		SimpleHash statementHash = new SimpleHash();
		Node sourceNode = this.getOBDRestApplication().getShard(dataSource).getNode(s.getNodeId());

		if (sourceNode != null && sourceNode.getLabel() != null){
			statementHash.put("sourceLabel", sourceNode.getLabel());
		} else {
			statementHash.put("sourceLabel", s.getNodeId());
		}
		statementHash.put("sourceHref", "/" + this.getContextName() + "/" + this.dataSource + "/html/nodes/" + Reference.encode(s.getNodeId()));
		
		Node rn = this.getOBDRestApplication().getShard(dataSource).getNode(s.getRelationId());
		if (rn != null && rn.getLabel() != null){
			statementHash.put("relationLabel", this.prettifyRelationshipTerm(rn.getLabel()));
		} else {
			statementHash.put("relationLabel", this.prettifyRelationshipTerm(s.getRelationId()));
		}
		statementHash.put("relationHref","/" + this.getContextName() + "/" + this.dataSource + "/html/nodes/" + Reference.encode(s.getRelationId()));
			
		if (s instanceof LinkStatement) {
			
			Node tn = this.getOBDRestApplication().getShard(dataSource).getNode(s.getTargetId());
			if (tn != null && tn.getLabel() != null){
				statementHash.put("targetLabel",tn.getLabel());
			} else {
				statementHash.put("targetLabel",s.getTargetId());
			}
			statementHash.put("targetHref","/" + this.getContextName() + "/" + this.dataSource + "/html/nodes/" + Reference.encode(s.getTargetId()));
			
			LinkStatement ls = (LinkStatement)s;
			if (ls.isIntersectionSemantics()) {
				statementHash.put("nscondition", true);    			
			}
		} else if (s instanceof LiteralStatement) {
			statementHash.put("targetLabel",((LiteralStatement)s).getSValue());
		}
		
		return statementHash;
	}
	
	
	private String prettifyRelationshipTerm(String term){
		if (term.contains("OBO_REL:")||term.contains("OBOL:")||(term.contains("oboInOwl:"))||(term.contains("oboMetamodel:"))){
			return  term.substring(term.indexOf(":")+1).replace("_", " ");
		} else {
			return term.replace("_", " ");
		}
	}
}
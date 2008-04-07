
package org.obd.ws.coreResource;


import java.util.ArrayList;
import java.util.Collection;
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
    	} else if (format.equals("html")){
    		TreeMap<String, Object> resourceMap = new TreeMap<String, Object>();
    		resourceMap.put("contextName", this.getContextName());
    		resourceMap.put("dataSource", this.dataSource);
    		resourceMap.put("node",this.node);
    		
    		
    		// Annotation Statements 
    		List<SimpleHash> annotationStatements = new ArrayList<SimpleHash>();
    		for (Statement s : this.getGraph("annotations").getStatements()){
        		SimpleHash annotationStatement = new SimpleHash();
    			annotationStatement.put("statement", this.prettifyStatement(s, dataSource));
        		Set<String> assignmentSources = new HashSet<String>();
    			Set<String> provenanceSources = new HashSet<String>();
    			for (Statement subStatement : s.getSubStatements()){
    				if (subStatement.getRelationId().equals("oban:assigned_by")){
    					assignmentSources.add(subStatement.getTargetId());
    				}
    				if (subStatement.getRelationId().equals("oban:has_data_source")){
    					provenanceSources.add(subStatement.getTargetId());
    				}
        			annotationStatement.put("assigned_by",assignmentSources);
        			annotationStatement.put("provenance",provenanceSources);
    			}
    			annotationStatements.add(annotationStatement);
    		}
    		if (annotationStatements.size()>0){
    			resourceMap.put("annotationStatements", annotationStatements);
    		}
    		
    		
    		// Statements to Node
    		List<Map<String,String>> toStatements = new ArrayList<Map<String,String>>();
    		for (Statement s : this.getGraph("to").getStatements()){
    			Map<String,String> m = new HashMap<String,String>();
    			m.put("statement",this.prettifyStatement(s, dataSource));
    			if (s.isInferred()){
    				m.put("entailment","I");
    			} else {
    				m.put("entailment","A");
    			}
    			toStatements.add(m);
    		}
    		if (toStatements.size()>0){
    			resourceMap.put("toStatements", toStatements);
    		}
    		
    		
    		// Statements about node
    		List<Map<String,String>> aboutStatements = new ArrayList<Map<String,String>>();
    		for (Statement s : this.getGraph("about").getStatements()){
    			Map<String,String> m = new HashMap<String,String>();
    			m.put("statement",this.prettifyStatement(s, dataSource));
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
    			resourceMap.put("aboutStatements", aboutStatements);
    		}
    		
    		return getTemplateRepresentation("NodeDetails",resourceMap);
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
			if (aspect.equals("about") || aspect.equals("all"))
				lq.setNode(getNodeId());
			else if (aspect.equals("to"))
				lq.setTarget(getNodeId());
			else if (aspect.equals("source"))
				lq.setSource(getNodeId());
			else
				lq.setNode(getNodeId());
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
	
	public String prettifyStatement(Statement s,String dataSource) {
				
		Graph g = new Graph();
		StringBuilder sb = new StringBuilder();
		Node sourceNode = this.getOBDRestApplication().getShard(dataSource).getNode(s.getNodeId());
		if (sourceNode != null && sourceNode.getLabel() != null){
			sb.append(hrefLabel(sourceNode.getLabel(),sourceNode.getId(),dataSource));
		} else {
			sb.append(href(s.getNodeId(),dataSource));
		}
		Node n = g.getNode(s.getNodeId());
		if (n != null && n.getLabel() != null)
			sb.append(" \""+n.getLabel()+"\"");
		sb.append("  ");
		
		String label="";
		Node rn = this.getOBDRestApplication().getShard(dataSource).getNode(s.getRelationId());
		if (rn != null && rn.getLabel() != null){
			label = rn.getLabel();
		} else {
			label = s.getRelationId();
		}
		label = this.prettifyRelationshipTerm(label);
	
		sb.append(hrefLabel(label,s.getRelationId(),dataSource));
		sb.append(" ");
		
		n = g.getNode(s.getTargetId());
		
		if (n != null && n.getLabel() != null)
			sb.append(" \""+n.getLabel()+"\"");
		if (s instanceof LinkStatement) {
			
			Node target = this.getOBDRestApplication().getShard(dataSource).getNode(s.getTargetId());
			if (target != null && target.getLabel() != null){
				sb.append(hrefLabel(target.getLabel(),target.getId(),dataSource));	
			} else {
				sb.append(href(s.getTargetId(),dataSource));
			}
			
			LinkStatement ls = (LinkStatement)s;
			if (ls.isIntersectionSemantics()) {
				sb.append(" [n+s condition element]");    			
			}
		}		

		if (s instanceof LiteralStatement) {
			sb.append(" "+((LiteralStatement)s).getSValue());
		}

		return sb.toString();
	}
	
	
	private String prettifyRelationshipTerm(String term){
		if (term.contains("OBO_REL:")||term.contains("OBOL:")||(term.contains("oboInOwl:"))||(term.contains("oboMetamodel:"))){
			return  term.substring(term.indexOf(":")+1).replace("_", " ");
		} else {
			return term.replace("_", " ");
		}
	}
}
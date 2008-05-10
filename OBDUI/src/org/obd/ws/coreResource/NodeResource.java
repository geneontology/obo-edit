
package org.obd.ws.coreResource;


import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
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
import org.obd.ws.coreResource.sorter.StatementComparator;
import org.restlet.Context;
import org.restlet.data.MediaType;
import org.restlet.data.Reference;
import org.restlet.data.Request;
import org.restlet.data.Response;
import org.restlet.resource.Representation;
import org.restlet.resource.StringRepresentation;
import org.restlet.resource.Variant;
import org.apache.commons.lang.StringEscapeUtils;

import freemarker.template.SimpleHash;

/**
 * Resource for a node
 */
public class NodeResource extends OBDResource {

    private Node node;
    protected String nodeString;
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
        
        this.nodeString = (String) request.getAttributes().get("nodeString");
        System.out.println("Requested Node String is: " + this.nodeString);
        if (this.nodeString != null){
        	this.nodeString = Reference.decode(this.nodeString);
        	System.out.println("Decoded is: " + this.nodeString);
        }
        this.format = (String) request.getAttributes().get("format");
        this.dataSource = (String) request.getAttributes().get("dataSource");
        
        uriString = (String) request.getResourceRef().toString();
  
            getVariants().add(new Variant(MediaType.TEXT_HTML));
        
    }

 
    /**
     * Finds the associated node.
     * 
     * @return The node found or null.
     */
    public Node findNode() {
        Node result = null;

        System.out.print("Looking for Node " + this.nodeString + " in data source " + this.dataSource + "..... ");
        if (nodeString != null) {       	
        	result = getShard(this.dataSource).getNode(nodeString);
        } 
        if (result != null){
        	System.out.println("Found.");
        } else {
        	System.out.println("Not Found.");
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
    	   	
    	this.node = findNode();
    	
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
    		resourceMap.put("encodedId", Reference.encode(this.nodeString));
    		
    		try {
    			InetAddress addr = InetAddress.getLocalHost();
    			resourceMap.put("hostname",addr.getCanonicalHostName());
    		} catch (UnknownHostException e) {
    			System.err.println("Hostname fetching error: " + e.getMessage());
    		}
    		
    		if ((node != null) && (node.getSourceId()!=null) && (node.getSourceId().equals("ZFIN"))){
    			resourceMap.put("nodeId",this.nodeString);
    			if (this.node.getLabel() != null){
    				resourceMap.put("nodeLabel", this.node.getLabel());
    			}
    		} else {
    			resourceMap.put("nodeId",StringEscapeUtils.escapeHtml(this.nodeString));
	    		if ((node != null) && (this.node.getLabel() != null)){
	    			resourceMap.put("nodeLabel", StringEscapeUtils.escapeHtml(this.node.getLabel()));
	    		}
    		}
    		

    		// Annotation Statements 
    		List<SimpleHash> annotationStatements = this.getStatements("annotation");
    		if (annotationStatements.size()>0){
    			Collections.sort(annotationStatements,new StatementComparator());
    			resourceMap.put("annotationStatements", annotationStatements);
    		}
    		
    		
    		// Statements to Node
    		List<SimpleHash> toStatements = this.getStatements("to");
    		if (toStatements.size()>0){
    			Collections.sort(toStatements,new StatementComparator());
    			resourceMap.put("toStatements", toStatements);
    		}
    		
    		// Statements about node
    		List<SimpleHash> aboutStatements = this.getStatements("about");
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
           	sb.append(hrefToOtherFormats("/node/"+getNodeId(),this.dataSource));
    		sb.append("</pre>");
    		return new StringRepresentation(sb, MediaType.TEXT_HTML);

    	}
    	return result;
    }
    
    protected List<SimpleHash> getStatements(String aspect){
    	
    	List<SimpleHash> statements = new ArrayList<SimpleHash>();
    	if (aspect.equals("annotation")){
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
    			statements.add(annotationStatement);
    		} 
    	} else if (aspect.equals("about")){
    		for (Statement s : this.getGraph("about").getStatements()){
    			SimpleHash m = new SimpleHash();
    			m.put("statement",this.hashifyStatement(s));
    			if (s.isInferred()){
    				m.put("entailment","I");
    			} else {
    				m.put("entailment","A");
    			}
    			statements.add(m);
    			
    			
    		}
		} else if (aspect.equals("to")){
			for (Statement s : this.getGraph("to").getStatements()){
    			SimpleHash m = new SimpleHash();
    			m.put("statement",this.hashifyStatement(s));
    			if (s.isInferred()){
    				m.put("entailment","I");
    			} else {
    				m.put("entailment","A");
    			}
    			statements.add(m);
    		}
		}  else {
			System.err.println("What Aspect? ");
			Exception e = new Exception();
			e.printStackTrace();
		}
    	return statements;
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
		return nodeString;
	}


	public void setNodeId(String nodeId) {
		this.nodeString = nodeId;
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
	
	protected SimpleHash hashifyStatement(Statement s) {
				
		SimpleHash statementHash = new SimpleHash();
		Node sourceNode = this.getOBDRestApplication().getShard(dataSource).getNode(s.getNodeId());

		String sourceLabel = null;
		if (sourceNode != null && sourceNode.getLabel() != null){
			sourceLabel = sourceNode.getLabel();
		} else {
			sourceLabel = s.getNodeId();
		}
		if (sourceNode != null && (sourceNode.getSourceId() != null) && !sourceNode.getSourceId().equals("ZFIN")){
			sourceLabel = StringEscapeUtils.escapeHtml(sourceLabel);
		} 
		statementHash.put("sourceLabel", sourceLabel);
		statementHash.put("sourceHref", "/" + this.getContextName() + "/" + this.dataSource + "/html/node/" + Reference.encode(s.getNodeId()));
		
		Node rn = this.getOBDRestApplication().getShard(dataSource).getNode(s.getRelationId());
		String relationLabel = null;
		if (rn != null && rn.getLabel() != null){
			relationLabel = this.prettifyRelationshipTerm(rn.getLabel());
		} else {
			relationLabel = this.prettifyRelationshipTerm(s.getRelationId());
		}
		if ((rn!= null) && (rn.getSourceId() != null) && !rn.getSourceId().equals("ZFIN")){
			relationLabel = StringEscapeUtils.escapeHtml(relationLabel);
		}
		statementHash.put("relationLabel", relationLabel);
		statementHash.put("relationHref","/" + this.getContextName() + "/" + this.dataSource + "/html/node/" + Reference.encode(s.getRelationId()));
			
		if (s instanceof LinkStatement) {
			
			Node tn = this.getOBDRestApplication().getShard(dataSource).getNode(s.getTargetId());
			String targetLabel = null;
			if (tn != null && tn.getLabel() != null){
				targetLabel = tn.getLabel();
			} else {
				targetLabel = s.getTargetId();
			}
			if (tn != null && (tn.getSourceId() != null) && !tn.getSourceId().equals("ZFIN")){
				targetLabel = StringEscapeUtils.escapeHtml(targetLabel);
			}
			statementHash.put("targetLabel",targetLabel);
			statementHash.put("targetHref","/" + this.getContextName() + "/" + this.dataSource + "/html/node/" + Reference.encode(s.getTargetId()));
			
			LinkStatement ls = (LinkStatement)s;
			if (ls.isIntersectionSemantics()) {
				statementHash.put("nscondition", true);    			
			}
		} else if (s instanceof LiteralStatement) {
			statementHash.put("targetLabel",((LiteralStatement)s).getSValue());
		}
		
		return statementHash;
	}
	
	protected String prettifyRelationshipTerm(String term){
		if (term.contains("OBO_REL:")||term.contains("OBOL:")||(term.contains("oboInOwl:"))||(term.contains("oboMetamodel:"))||(term.contains("dc:description"))){
			return  term.substring(term.indexOf(":")+1).replace("_", " ");
		} else {
			return term.replace("_", " ");
		}
	}
}
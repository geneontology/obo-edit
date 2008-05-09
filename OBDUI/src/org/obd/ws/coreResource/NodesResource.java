
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

import org.apache.commons.lang.StringEscapeUtils;
import org.obd.model.Graph;
import org.obd.model.LinkStatement;
import org.obd.model.LiteralStatement;
import org.obd.model.Node;
import org.obd.model.Statement;
import org.obd.model.bridge.OBDJSONBridge;
import org.obd.model.bridge.OBOBridge;
import org.obd.model.bridge.OWLBridge;
import org.obd.query.AnnotationLinkQueryTerm;
import org.obd.query.BooleanQueryTerm;
import org.obd.query.LinkQueryTerm;
import org.obd.query.BooleanQueryTerm.BooleanOperator;
import org.obd.ws.coreResource.sorter.StatementComparator;
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
public class NodesResource extends OBDResource {

    private Collection<Node> nodes;
	protected String format;
	protected String dataSource;
	protected String uriString;
	protected String nodeString;
	
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
    public NodesResource(Context context, Request request, Response response) {
        
    	super(context, request, response);
        this.format = (String) request.getAttributes().get("format");
        this.dataSource = (String) request.getAttributes().get("dataSource");
        this.nodeString = (String) request.getAttributes().get("nodeList");
        this.nodes = new HashSet<Node>();
        
        
        if (this.nodeString != null){
        	System.out.print("Node List: " + this.nodeString + "\t");
        	this.nodeString = Reference.decode(nodeString);
        	System.out.println("Decoded: " + this.nodeString);
        	getVariants().add(new Variant(MediaType.TEXT_HTML));
        	for (String uid : this.nodeString.split("\\+")){
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
    		
    		List<SimpleHash> nodes = new ArrayList<SimpleHash>();
    		for (Node n : this.nodes){
    			SimpleHash nodeHash = new SimpleHash();
    			if (n.getLabel() != null){
    				nodeHash.put("label", StringEscapeUtils.escapeHtml(n.getLabel()));
    			} else {
    				nodeHash.put("label", StringEscapeUtils.escapeHtml(n.getId()));
    			}
    			nodeHash.put("id", n.getId());
    			nodeHash.put("href", "/" + this.getContextName() + "/" + this.dataSource + "/html/node/" + Reference.encode(n.getId()));
    			nodes.add(nodeHash);
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
			if (aspect.equals("annotations")) {
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
			statementHash.put("sourceLabel", StringEscapeUtils.escapeHtml(sourceNode.getLabel()));
		} else {
			statementHash.put("sourceLabel", StringEscapeUtils.escapeHtml(s.getNodeId()));
		}
		statementHash.put("sourceHref", "/" + this.getContextName() + "/" + this.dataSource + "/html/node/" + Reference.encode(s.getNodeId()));
		
		Node rn = this.getOBDRestApplication().getShard(dataSource).getNode(s.getRelationId());
		if (rn != null && rn.getLabel() != null){
			statementHash.put("relationLabel", this.prettifyRelationshipTerm(StringEscapeUtils.escapeHtml(rn.getLabel())));
		} else {
			statementHash.put("relationLabel", this.prettifyRelationshipTerm(StringEscapeUtils.escapeHtml(s.getRelationId())));
		}
		statementHash.put("relationHref","/" + this.getContextName() + "/" + this.dataSource + "/html/node/" + Reference.encode(s.getRelationId()));
			
		if (s instanceof LinkStatement) {
			
			Node tn = this.getOBDRestApplication().getShard(dataSource).getNode(s.getTargetId());
			if (tn != null && tn.getLabel() != null){
				statementHash.put("targetLabel",StringEscapeUtils.escapeHtml(tn.getLabel()));
			} else {
				statementHash.put("targetLabel",StringEscapeUtils.escapeHtml(s.getTargetId()));
			}
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
	
	
	private String prettifyRelationshipTerm(String term){
		if (term.contains("OBO_REL:")||term.contains("OBOL:")||(term.contains("oboInOwl:"))||(term.contains("oboMetamodel:"))||(term.contains("dc:description"))){
			return  term.substring(term.indexOf(":")+1).replace("_", " ");
		} else {
			return term.replace("_", " ");
		}
	}
}
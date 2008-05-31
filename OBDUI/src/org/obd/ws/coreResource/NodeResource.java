
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

import org.obd.model.CompositionalDescription;
import org.obd.model.Graph;
import org.obd.model.LinkStatement;
import org.obd.model.LiteralStatement;
import org.obd.model.Node;
import org.obd.model.Statement;
import org.obd.model.CompositionalDescription.Predicate;
import org.obd.model.bridge.OBDJSONBridge;
import org.obd.model.bridge.OBDXMLBridge;
import org.obd.model.bridge.OBOBridge;
import org.obd.model.bridge.OWLBridge;
import org.obd.query.AnnotationLinkQueryTerm;
import org.obd.query.LinkQueryTerm;
import org.obd.ws.coreResource.sorter.StatementHashComparator;
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
	
	protected List<Statement> toAnnotationStatements;
	protected List<Statement> aboutAnnotationStatements;	

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
        
        this.nodeString = (String) request.getAttributes().get("id");
        System.out.println("Requested Node String is: " + this.nodeString);
        if (this.nodeString != null){
        	this.nodeString = Reference.decode(this.nodeString);
        	System.out.println("Decoded is: " + this.nodeString);
        }
        this.format = (String) request.getAttributes().get("format");
        this.dataSource = (String) request.getAttributes().get("dataSource");
        this.uriString = (String) request.getResourceRef().toString();
  
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
        	this.node = result;
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
    	} else if (format.equals("obdxml")){
    		result = new StringRepresentation(OBDXMLBridge.toXML(node));
    		return result;
    	}
    	else if (format.equals("html")||(format.equals("bioPortal"))){
    		
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
    		if (this.nodeString.contains("^")){
				resourceMap.put("nodeIdIsComposed", true);
				resourceMap.put("composedClassName", this.decomposeNode(this.getShard(dataSource).getCompositionalDescription(this.node.getId(), true)));
			}
    		
    		// Statements to Node
    		List<SimpleHash> toStatements = this.getHashifiedStatements("to");
    		if (toStatements.size()>0){
    			Collections.sort(toStatements,new StatementHashComparator());
    			resourceMap.put("toStatements", toStatements);
    		}
    		
    		// Statements about node
    		List<SimpleHash> aboutStatements = this.getHashifiedStatements("about");
    		if (aboutStatements.size()>0){
    			Collections.sort(aboutStatements,new StatementHashComparator());
    			resourceMap.put("aboutStatements", aboutStatements);
    		}
    		
    		// Annotation Statements 
    		List<SimpleHash> annotationStatements = this.getHashifiedStatements("annotations");
    		if (annotationStatements.size()>0){
    			Collections.sort(annotationStatements,new StatementHashComparator());
    			resourceMap.put("annotationStatements", annotationStatements);
    		}
    		
    		

    		
    		if (format.equals("html")){
    			return getTemplateRepresentation("NodeDetails",resourceMap);
    		} else {
    			return getTemplateRepresentation("BPNodeDetails",resourceMap);
    		}
    	} else {
    		//Creates a text representation
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
    
    protected List<SimpleHash> getHashifiedStatements(String aspect){
    	return getHashifiedStatements(aspect,getNodeId());
    }
    
    protected List<SimpleHash> getHashifiedStatements(String aspect,String nodeId){
    	
    	List<SimpleHash> statements = new ArrayList<SimpleHash>();
    	if (aspect.equals("annotations")){
    		statements.addAll(this.hashifyStatements(this.getGraph(aspect,nodeId).getStatements(), true));
    	} else {
    		statements.addAll(this.hashifyStatements(this.getGraph(aspect,nodeId).getStatements()));
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
		return this.nodeString;
	}


	public void setNodeId(String nodeId) {
		this.nodeString = nodeId;
	}
    
	protected Graph getGraph(String aspect){
		return getGraph(aspect,getNodeId());
	}
	
	protected Graph getGraph(String aspect,String nodeId){

		Graph graph;
		if (aspect == null || aspect.equals("")){
			aspect = "about";
		}
		
		Collection<Statement> statements = new ArrayList<Statement>();
		if (aspect.equals("about")){
			for (Statement s : this.getShard(dataSource).getStatementsForNode(nodeId,false)){
				if (!s.isAnnotation()){
					statements.add(s);
				}
			}
			
		} else if (aspect.equals("to")){
			for (Statement s : this.getShard(dataSource).getStatementsForTarget(nodeId,false)){
				if (!s.isAnnotation()){
					statements.add(s);
				}
			}
		} else if (aspect.equals("annotations")){
			statements = this.getShard(dataSource).getAnnotationStatementsForNode(nodeId, null, null);
			statements.addAll(this.getShard(dataSource).getAnnotationStatementsForAnnotatedEntity(nodeId, null, null));
		} else {
			Exception e = new Exception();
			System.err.println("What Aspect? " + aspect);
			e.printStackTrace();
		}
		graph = new Graph(statements);
		if (true) {
			String[] nids = graph.getReferencedNodeIds();
			for (String nid : nids) {
				Node n = getShard(this.dataSource).getNode(nid);
				graph.addNode(n);
			}
		}
		return graph;
	}
	
	
	protected Graph OLD_getGraph(String aspect) {
		Graph graph;
		if (aspect == null || aspect.equals("")){
			aspect = "about";
		}

		LinkQueryTerm lq = new LinkQueryTerm();
		if (aspect.equals("annotations")) {
			lq = new AnnotationLinkQueryTerm(getNodeId());
		} else {
			if (aspect.equals("about")){
				lq.setNode(getNodeId());
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
			lq.setTarget(getNodeId());
			stmts.addAll(getShard(this.dataSource).getStatementsByQuery(lq));
		}
		
		System.out.println(aspect + " statements: " + stmts.size());
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
	
	protected SimpleHash hashifyStatement(Statement s){
		return this.hashifyStatement(s,false); 
	}
	
	protected Collection<SimpleHash> hashifyStatements(Statement[] statements){
		List<SimpleHash> hashifiedStatements = new ArrayList<SimpleHash>();
		for (Statement s : statements){
			hashifiedStatements.add(this.hashifyStatement(s));
		}
		return hashifiedStatements;
	}
	
	protected Collection<SimpleHash> hashifyStatements(Statement[] statements,boolean populateProvenance){
		List<SimpleHash> hashifiedStatements = new ArrayList<SimpleHash>();
		for (Statement s : statements){
			hashifiedStatements.add(this.hashifyStatement(s,populateProvenance));
		}
		return hashifiedStatements;
	}
	protected SimpleHash hashifyStatement(Statement s, boolean populateProvenance){
		SimpleHash statementHash = new SimpleHash();
		
		String hrefBase = "/" + this.getContextName() + "/" + this.dataSource + "/html/node/";
		statementHash.put("subject", this.hashifyNode(s.getNodeId(), (hrefBase+Reference.encode(s.getNodeId()))));
		SimpleHash relationshipHash = this.hashifyNode(s.getRelationId(), (hrefBase+Reference.encode(s.getRelationId())));
		relationshipHash.put("nodeLabel", this.prettifyRelationshipTerm(s.getRelationId()));
		statementHash.put("predicate", relationshipHash);
		
		if (s instanceof LinkStatement) {
			SimpleHash objectHash =this.hashifyNode(s.getTargetId(), (hrefBase+Reference.encode(s.getTargetId())));
			if (s.isIntersectionSemantics()) {
				objectHash.put("nscondition", true);    			
			}
			statementHash.put("object", objectHash);
		} else if (s instanceof LiteralStatement) {
			statementHash.put("object", this.hashifyNode(((LiteralStatement)s).getSValue(), null));
		}
		
		if (s.isInferred()){
			statementHash.put("entailment","I");
		} else {
			statementHash.put("entailment","A");
		}
		
		if (populateProvenance){
			Set<String> assignmentSources = new HashSet<String>();
			Set<String> provenanceSources = new HashSet<String>();
			for (Statement subStatement : s.getSubStatements()){
				if (subStatement.getRelationId().equals("oban:assigned_by")){
					assignmentSources.add(this.href(subStatement.getTargetId(),dataSource));
				}
				if (subStatement.getRelationId().equals("oban:has_data_source")){
					provenanceSources.add(this.href(subStatement.getTargetId(),dataSource));
				}
    			statementHash.put("assigned_by",assignmentSources);
    			statementHash.put("provenance",provenanceSources);
			}
		}
		
		return statementHash;
	}
	
	protected SimpleHash decomposeNode(CompositionalDescription cd){
		SimpleHash nodeHash = new SimpleHash();
		
		if (cd.isGenusDifferentia()){
			Node subjectNode = this.getShard(dataSource).getNode(cd.getGenus().getNodeId());
			if (subjectNode != null && subjectNode.getLabel() != null){
				nodeHash.put("subjectLabel", subjectNode.getLabel());
			} else {
				nodeHash.put("subjectLabel", cd.getGenus().getNodeId());
			}
			nodeHash.put("args",this.decomposeArguments(cd.getDifferentiaArguments()));
		}  else if (cd.isAtomic()){
			nodeHash.put("subjectLabel", cd.toString());
		} else {
			nodeHash.put("relationLabel", this.prettifyRelationshipTerm(cd.getRelationId()));
			nodeHash.put("args", this.decomposeArguments(cd.getArguments()));
		}
		return nodeHash;
	}
	
	protected List<SimpleHash> decomposeArguments(Collection<CompositionalDescription> args){
		List<SimpleHash> decomposedArgs = new ArrayList<SimpleHash>();
		for (CompositionalDescription arg : args){
			SimpleHash target = new SimpleHash();
			if (this.hasSingularAtomicArgument(arg)){
				target.put("relationLabel", this.prettifyRelationshipTerm(arg.getRelationId()));
				String targetLabel = "";
				if (arg.getPredicate().equals(Predicate.RESTRICTION)){
					if (arg.getRestriction().isExistential()){
						targetLabel += " some "; 
					} else if (arg.getRestriction().isUniversal()){
						targetLabel += " all ";
					}
				}
				
				Node targetNode = this.getShard(dataSource).getNode(arg.getFirstArgument().getNodeId());
				if (targetNode != null && targetNode.getLabel() != null){
					targetLabel += targetNode.getLabel();
				} else {
					targetLabel += arg.getFirstArgument().getNodeId();
				}
				target.put("targetLabel", targetLabel);
			} else {
				target.put("targetIsComplex", true);
				target.put("composedClass", decomposeNode(arg));
			}
			decomposedArgs.add(target);
		}
		return decomposedArgs;
	}
	
	
	protected boolean hasSingularAtomicArgument(CompositionalDescription cd){
		if (cd.getArguments().size()==1){
			CompositionalDescription arg = cd.getFirstArgument();
			if (arg.isAtomic()){
				return true;
			}
		}
		return false;
	}
	
	protected String prettifyRelationshipTerm(String term){
		String formatted;
		if (term.contains("OBO_REL:")||term.contains("OBOL:")||(term.contains("oboInOwl:"))||(term.contains("oboMetamodel:"))||(term.contains("dc:description"))){
			formatted = term.substring(term.indexOf(":")+1).replace("_", " ");
		} else {
			formatted  = term.replace("_", " ");
		}
		
		if (term.equals("part of")){
			formatted = "that is part of";
		}
		
		if (term.equals("inheres in")){
			formatted = "that inheres in";
		}
		return formatted;
	}
	
	protected SimpleHash hashifyNode(String nodeId,String href){
		SimpleHash nodeHash = new SimpleHash();
		Node n = this.getShard(dataSource).getNode(nodeId);
		String label = "";
		if (n != null && n.getLabel() != null){
			label = n.getLabel();
		} else {
			label = nodeId;
		}
		if (n != null && n.getSourceId()!=null && !n.getSourceId().equals("ZFIN")){
			label = StringEscapeUtils.escapeHtml(label);
		}
		nodeHash.put("nodeLabel", label);
		if (href != null){
			nodeHash.put("nodeHref", href);
		}
		if (nodeId.contains("^")){
			CompositionalDescription cd = this.getShard(dataSource).getCompositionalDescription(nodeId, true);
			if (cd != null){
				nodeHash.put("nodeIsComposed", true);
				nodeHash.put("composedNode", this.decomposeNode(cd));
			}
		}
		return nodeHash;
	}
}
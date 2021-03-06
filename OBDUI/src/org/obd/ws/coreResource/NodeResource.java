
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
import org.obd.query.LinkQueryTerm;
import org.obd.ws.coreResource.sorter.StatementHashComparator;
import org.obd.ws.coreResource.utility.NodeTyper;
import org.obd.ws.coreResource.utility.NodeTyper.Type;
import org.purl.obo.vocab.RelationVocabulary;
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

		List<SimpleHash> altViews = new ArrayList<SimpleHash>();
		TreeMap<String, Object> resourceMap = new TreeMap<String, Object>();

		resourceMap.put("contextName", this.getContextName());
		resourceMap.put("dataSource", this.dataSource);
		resourceMap.put("node",this.node);
		resourceMap.put("id", this.nodeString);
		resourceMap.put("encodedId", Reference.encode(this.nodeString));

		try {
			InetAddress addr = InetAddress.getLocalHost();
			resourceMap.put("hostname",addr.getCanonicalHostName());
		} catch (UnknownHostException e) {
			System.err.println("Hostname fetching error: " + e.getMessage());
		}

		Type nodeType = NodeTyper.getNodeType(this.nodeString, this.getShard(this.dataSource));
		resourceMap.put("nodeType", nodeType.toString());
		if (nodeType.equals(Type.GENE)){
			SimpleHash view = new SimpleHash();
			view.put("view", "Gene");
			view.put("href", "/" + this.getContextName() + "/" + this.dataSource + "/html/gene/" + Reference.encode(this.nodeString));
			altViews.add(view);
		}

		String otherFormat = "exhibit";
		if (this.format.equals("exhibit")){
			otherFormat = "html";
		}
		SimpleHash view = new SimpleHash();
		view.put("view", otherFormat);
		view.put("lens", "XPTable");
		view.put("href", "/" + this.getContextName() + "/" + this.dataSource + "/" + otherFormat + "/node/" + Reference.encode(this.nodeString));
		altViews.add(view);

		resourceMap.put("nodeViews", altViews);

		Representation result = null;

		if (format == null) {
			format = "";
		}
		if (format.equals("exhibitData")){


			List<SimpleHash> statements = new ArrayList<SimpleHash>();

			String[] aspects = new String[]{"to","about","annotation"};

			for (String aspect : aspects){
				List<SimpleHash> aspectStatements = this.getHashifiedStatements(aspect,"exhibit");
				for (SimpleHash sh : aspectStatements ){
					sh.put("aspect",aspect);
				}
				statements.addAll(aspectStatements);

			}

			resourceMap.put("statements", statements);

			return getTemplateRepresentation("ExhibitNodeData",resourceMap);


		} else if (format.equals("json")) {
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
			if (node==null){
				return null;
			} else {
				result = new StringRepresentation(OBDXMLBridge.toXML(node),MediaType.TEXT_XML);
			}
			return result;
		} else if (format.equals("exhibit")){

			return getTemplateRepresentation("ExhibitNode",resourceMap);

		} else if (format.equals("XPTable")){
			if (this.node != null){
				resourceMap.put("node", this.hashifyNode(this.node.getId(), "/" + this.getContextName() + "/" + this.dataSource + "/html/node/" + Reference.encode(this.node.getId()) ));
			}

			// Annotation Statements 
			List<SimpleHash> ahl = new ArrayList<SimpleHash>();
			Map<String,Integer> relCount = new HashMap<String,Integer>();
			Collection<Statement> stmts = getGraph("annotation",getNodeId()).getStatements();
			for (Statement s : stmts) {

				if (! (s instanceof LinkStatement))
					continue;
				LinkStatement ls = (LinkStatement)s;
				SimpleHash ah = hashifyStatement(ls, true,"html");
				ahl.add(ah);
				// TODO: this gets computed again in hashify nodes
				String tid = ls.getTargetId();
				CompositionalDescription cd = this.getShard(dataSource).getCompositionalDescription(tid, true);
				if ((cd != null) && (cd.getArguments() != null)){
					for (CompositionalDescription arg : cd.getArguments()) {
						String relId = arg.getRelationId();
						if (relId == null)
							relId = "";
						if (!relCount.containsKey(relId))
							relCount.put(relId, 0);
						relCount.put(relId, relCount.get(relId)+1);
						String t = arg.getNodeId();
						if (arg.getPredicate().equals(Predicate.RESTRICTION)) {
							t = arg.getRestriction().getTargetId();
						}
						String href = this.getNodeURL(t);
						ah.put(relId, this.hashifyNode(t, href));
					}
				}

			}
			List<String> rels = new ArrayList<String>();
			// TODO: ordering
			for (String relId : relCount.keySet()) {
				rels.add(relId);
			}
			resourceMap.put("xpRelations", rels);
			resourceMap.put("annotationStatements", ahl);
			return getTemplateRepresentation("NodeDetails",resourceMap);

		} else if (format.equals("html")||(format.equals("bioPortal"))){




			if (this.node != null){
				resourceMap.put("node", this.hashifyNode(this.node.getId(), "/" + this.getContextName() + "/" + this.dataSource + "/html/node/" + Reference.encode(this.node.getId()) ));
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

			// Annotation Statements 
			List<SimpleHash> annotationStatements = this.getHashifiedStatements("annotation","html");
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

	protected List<SimpleHash> getHashifiedStatements(String aspect,String format){
		return getHashifiedStatements(aspect,getNodeId(),format);
	}

	protected List<SimpleHash> getHashifiedStatements(String aspect,String nodeId,String format){
		List<SimpleHash> statements = new ArrayList<SimpleHash>();
		if (aspect.equals("annotation")){
			statements.addAll(this.hashifyStatements(this.getGraph(aspect,nodeId).getStatements(), true,format));
		} else {
			statements.addAll(this.hashifyStatements(this.getGraph(aspect,nodeId).getStatements(),false,format));
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
			for (Statement s : this.getShard(dataSource).getNonRedundantStatementsForNode(nodeId)){
				if (!s.isAnnotation()){
					statements.add(s);
				}
			}
			// make sure asserted links are included, even if redundant
			for (Statement s : this.getShard(dataSource).getStatementsForNode(nodeId,false)){
				if (!s.isAnnotation()){
					statements.add(s);
				}
			}
		} else if (aspect.equals("to")){
			// only asserted
			for (Statement s : this.getShard(dataSource).getStatementsForTarget(nodeId,false)){
				if (!s.isAnnotation()){
					statements.add(s);
				}
			}
			// include inferred with instance-of links
			RelationVocabulary rv = new RelationVocabulary();
			LinkQueryTerm qt = new LinkQueryTerm(rv.instance_of(), nodeId);
			for (Statement s : this.getShard(dataSource).getLinkStatementsByQuery(qt)) {
				if (!s.isAnnotation()){
					statements.add(s);
				}
			}
		
			
		} else if (aspect.equals("annotation")){
			statements = this.getShard(dataSource).getAnnotationStatementsForNode(nodeId, null, null);
			statements.addAll(this.getShard(dataSource).getAnnotationStatementsForAnnotatedEntity(nodeId, null, null));
		} else {
			Exception e = new Exception();
			System.err.println("What Aspect? " + aspect);
			e.printStackTrace();
		}
		graph = new Graph(statements);
		if (true) {
			Collection<String> nids = graph.getReferencedNodeIds();
			for (String nid : nids) {
				Node n = getShard(this.dataSource).getNode(nid);
				graph.addNode(n);
			}
		}
		return graph;
	}



	protected SimpleHash hashifyStatement(Statement s){
		return this.hashifyStatement(s,false,"html"); 
	}


	protected Collection<SimpleHash> hashifyStatements(Statement[] statements){
		List<SimpleHash> hashifiedStatements = new ArrayList<SimpleHash>();
		for (Statement s : statements){
			hashifiedStatements.add(this.hashifyStatement(s));
		}
		return hashifiedStatements;
	}

	protected Collection<SimpleHash> hashifyStatements(Collection<Statement> statements,boolean populateProvenance,String format){
		List<SimpleHash> hashifiedStatements = new ArrayList<SimpleHash>();
		for (Statement s : statements){
			hashifiedStatements.add(this.hashifyStatement(s,populateProvenance,format));
		}
		return hashifiedStatements;
	}
	/**
	 * @param s
	 * @param populateProvenance : if true will get substatements and populate assigned_by etc tags
	 * @param format
	 * @return
	 */
	protected SimpleHash hashifyStatement(Statement s, boolean populateProvenance, String format){

		SimpleHash statementHash = new SimpleHash();
		
		//System.out.println("hashifying "+s);

		if (format==null){
			format="html";
		}
		String hrefBase = "/" + this.getContextName() + "/" + this.dataSource + "/" + format + "/node/";
		statementHash.put("subject", this.hashifyNode(s.getNodeId(), (hrefBase+Reference.encode(s.getNodeId()))));
		SimpleHash relationshipHash = this.hashifyNode(s.getRelationId(), (hrefBase+Reference.encode(s.getRelationId())));
		relationshipHash.put("label", this.prettifyRelationshipTerm(s.getRelationId()));
		statementHash.put("predicate", relationshipHash);

		if (s instanceof LinkStatement) {
			SimpleHash objectHash =this.hashifyNode(s.getTargetId(), (hrefBase+Reference.encode(s.getTargetId())));
			if (s.isIntersectionSemantics()) {
				statementHash.put("nscondition", true);    			
			}
			statementHash.put("object", objectHash);
		} else if (s instanceof LiteralStatement) {
			SimpleHash valHash = new SimpleHash();
			valHash.put("label", ((LiteralStatement)s).getSValue());
			statementHash.put("object", valHash);
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
		//System.out.print("Trying to decompose: " + cd.toString() + ".....");
		if (cd.isGenusDifferentia() && cd.getGenus() != null){
			//System.out.println("Is GD.");
			Node subjectNode = this.getShard(dataSource).getNode(cd.getGenus().getNodeId());
			if (subjectNode != null && subjectNode.getLabel() != null){
				nodeHash.put("subjectLabel", subjectNode.getLabel());
				nodeHash.put("subjectURL", this.getNodeURL(subjectNode.getId()));
			} else {
				nodeHash.put("subjectLabel", cd.getGenus().getNodeId());
				nodeHash.put("subjectURL", this.getNodeURL(cd.getGenus().getNodeId()));
			}
			nodeHash.put("args",this.decomposeArguments(cd.getDifferentiaArguments()));
		} else if (cd.isAtomic()){
			//System.out.println("Is Atomic.");
			nodeHash.put("subjectLabel", cd.toString());
		} else {
			//System.out.println("Has Many Args.");
			nodeHash.put("relationId", cd.getRelationId());
			nodeHash.put("relationURL", this.getNodeURL(cd.getRelationId()));
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
				target.put("relationId", arg.getRelationId());
				target.put("relationURL", this.getNodeURL(arg.getRelationId()));
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
					target.put("targetId", targetNode.getId());
					target.put("targetURL", this.getNodeURL(targetNode));
					targetLabel += targetNode.getLabel();
				} else {
					target.put("targetId", arg.getFirstArgument().getNodeId());
					target.put("targetURL", this.getNodeURL(arg.getFirstArgument().getNodeId()));
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

	protected String prettifyRelationshipTerm(String id){
		Node n = getShard(this.dataSource).getNode(id);
		if (n != null && n.getLabel() != null)
			return n.getLabel();
		
		String formatted;
		if (id.contains("OBO_REL:")||id.contains("OBOL:")||(id.contains("oboInOwl:"))||(id.contains("oboMetamodel:"))||(id.contains("dc:description"))){
			formatted = id.substring(id.indexOf(":")+1).replace("_", " ");
		} else {
			formatted  = id.replace("_", " ");
		}

		if (id.equals("part of")){
			formatted = "that is part of";
		}

		if (id.equals("inheres in")){
			formatted = "that inheres in";
		}
		return formatted;
	}

	protected List<SimpleHash> hashifyNodes(Collection<String> nodeIds,String hrefPrefix){
		List<SimpleHash> hashifiedNodes = new ArrayList<SimpleHash>();
		for (String nodeId : nodeIds){
			hashifiedNodes.add(this.hashifyNode(nodeId, hrefPrefix + Reference.encode(nodeId)));
		}
		return hashifiedNodes;
	}

	protected SimpleHash hashifyNode(String nodeId,String href){
		SimpleHash nodeHash = new SimpleHash();
		Node n = this.getShard(dataSource).getNode(nodeId);
		if (n == null) {
			return nodeHash;
		}
		String label = "";
		if (n != null && n.getLabel() != null){
			label = n.getLabel();
		} else {
			label = nodeId;
		}
		if (n != null && n.getSourceId()!=null && !n.getSourceId().equals("ZFIN")){
			label = StringEscapeUtils.escapeHtml(label);
		}
		nodeHash.put("id", nodeId);
		nodeHash.put("encodedId", Reference.encode(nodeId));
		nodeHash.put("label", label);
		if (n != null)
			nodeHash.put("source",n.getSourceId());
		if (href != null){
			nodeHash.put("href", href);
		}

		if (nodeId.contains("^")){
			//System.out.println("getting CD for "+nodeId);
			SimpleHash cdhash = getCDHash(nodeId);
			if (cdhash != null) {
				nodeHash.put("isComposed", true);
				nodeHash.put("composedNode", cdhash);
			}
		}
		return nodeHash;
	}
	
	Map<String,SimpleHash> cdhMap = new HashMap<String,SimpleHash>();
	/**
	 * fetches a compositional description. uses a cache
	 * @param nodeId
	 * @return
	 */
	private SimpleHash getCDHash(String nodeId) {
		// TODO - prevent map getting too full
		if (cdhMap.containsKey(nodeId))
			return cdhMap.get(nodeId);
		SimpleHash cdhash = null;
		CompositionalDescription cd = this.getShard(dataSource).getCompositionalDescription(nodeId, false);
		if ((cd != null) && (cd.getArguments() != null)){
			 cdhash = this.decomposeNode(cd);
			cdhash.put("composedNodeURL", getNodeURL(nodeId));
		}
		cdhMap.put(nodeId, cdhash);
		return cdhash;
	}

	/**
	 * @param node
	 * @return string containing relative REST URL portion
	 */
	protected String getNodeURL(Node node) {
		return getNodeURL(node, "node");
	}

	/**
	 * @param nodeId
	 * @return string containing relative REST URL portion
	 */
	protected String getNodeURL(String id) {
		return getNodeURL(id, "node");
	}

	/**
	 * @param node
	 * @param view - e.g. node
	 * @return string containing relative REST URL portion
	 * @author cjm
	 */
	protected String getNodeURL(Node node, String view) {
		return getNodeURL(node.getId(), view);
	}

	/**
	 * @param nodeId
	 * @param view - e.g. node
	 * @return string containing relative REST URL portion
	 * @author cjm
	 */
	protected String getNodeURL(String id, String view) {
		return "/" + this.getContextName() + 
		"/" + this.dataSource + 
		"/html/"+view+"/" + 
		Reference.encode(id);
	}



}
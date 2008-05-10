
package org.obd.ws.coreResource;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.obd.model.Graph;
import org.obd.model.LinkStatement;
import org.obd.model.LiteralStatement;
import org.obd.model.Node;
import org.obd.model.Statement;
import org.obd.query.Shard;
import org.obd.ws.restServlet.OBDRestApplication;
import org.restlet.Context;
import org.restlet.data.Form;
import org.restlet.data.MediaType;
import org.restlet.data.Reference;
import org.restlet.data.Request;
import org.restlet.data.Response;
import org.restlet.ext.freemarker.TemplateRepresentation;
import org.restlet.resource.Representation;
import org.restlet.resource.Resource;
import org.restlet.resource.Variant;
import freemarker.ext.beans.BeansWrapper;
import freemarker.template.Configuration;

/**
 * Resource for a node
 */
public abstract class OBDResource extends Resource {

	protected Form form;
	protected String queryString;
	protected int limit;
	protected int from;
	
	protected Set<String> imports;
	
	protected String[] fmts = {"json","html","obo","owl","obdxml"};

	public OBDResource(Context context, Request request, Response response) {
		super(context, request, response);
		getVariants().add(new Variant(MediaType.TEXT_XML));

		queryString = request.getResourceRef().getQuery();
		form = request.getResourceRef().getQueryAsForm();
		String limitVal = form.getFirstValue("limit");
		if (limitVal != null)
			limit = Integer.parseInt(limitVal);
		String fromVal = form.getFirstValue("from");
		if (fromVal != null)
			limit = Integer.parseInt(fromVal);
		
	}
	
	/**
	 * Returns the parent OBDRestApplication.
	 * 
	 * @return the parent OBDRestApplication.
	 */
	
	public OBDRestApplication getOBDRestApplication() {
		return (OBDRestApplication) getContext().getAttributes().get(OBDRestApplication.KEY);
	}

	/**
	 * Returns the database container.
	 * 
	 * @return the database container.
	 */
	//public Shard getShard() {
	//	return getOBDRestApplication().getShard();
	//}
	
	public Shard getShard(String resourceName){
		return ((OBDRestApplication)this.getApplication()).getShard(resourceName);
	}


	public String href(Node node,String dataSource) {
		String id = node.getId();
		return href(id,dataSource);
	}
	
	public String hrefLabel(String label, String id, String dataSource){
		
		String eid = Reference.encode(id);
		
		return "<a href=\"/" + this.getContextName() + "/" +  dataSource + "/html/node/"+eid+"\">"+this.HTMLEntityEncode(label)+"</a>";
	}
	public String href(String id,String dataSource) {
		String eid = Reference.encode(id);
		return "<a href=\"/" + this.getContextName() + "/" +  dataSource + "/html/node/"+eid+"\">"+this.HTMLEntityEncode(id)+"</a>";
	}
	
	public String href(String path, String id,String dataSource) {
		return "<a href=\"/" + this.getContextName() + "/" + dataSource + "/html/"+path+"\">"+id+"</a>";
	}
	public String hrefNodesBySearch(String n,String dataSource) {
		return "<a href=\"/" + this.getContextName() + "/" + dataSource + "/html/nodes-by-search/"+n+"\">"+n+"</a>";
	}
	public String hrefStatementsFor(String id,String dataSource) {
		String eid = Reference.encode(id);
		

		return "[ <a href=\"/" + this.getContextName() + "/" + dataSource + "/html/node/"+eid+"/statements/about\">about</a> | " +
		"<a href=\"/" + this.getContextName() + "/" + dataSource + "/html/node/"+eid+"/statements/to\">to</a> | " +
		"<a href=\"/" + this.getContextName() + "/" + dataSource + "/html/node/"+eid+"/statements/annotations\">annotations</a> | " +
		"<a href=\"/" + this.getContextName() + "/" + dataSource + "/html/node/"+eid+"/statements/all\">all</a> ]";
	}

	public String hrefDescriptionFor(String id,String dataSource) {
		String eid = Reference.encode(id);
		return "<a href=\"/" + this.getContextName() + "/" + dataSource + "/html/node/"+eid+"/description\">"+id+"</a>";
	}

	public String hrefStatement(Statement s,String dataSource) {
		return  hrefStatement(new Graph(), s,dataSource);
	}
	

	public String hrefStatement(Graph g, Statement s,String dataSource) {
		StringBuilder sb = new StringBuilder();
		sb.append(href(s.getNodeId(),dataSource));
		Node n = g.getNode(s.getNodeId());
		if (n != null && n.getLabel() != null)
			sb.append(" \""+n.getLabel()+"\"");
		sb.append(" --[ ");
		sb.append(href(s.getRelationId(),dataSource));
		sb.append("]--> ");
		n = g.getNode(s.getTargetId());
		if (n != null && n.getLabel() != null)
			sb.append(" \""+n.getLabel()+"\"");
		if (s instanceof LinkStatement) {
			sb.append(href(s.getTargetId(),dataSource));
			LinkStatement ls = (LinkStatement)s;
			if (ls.isIntersectionSemantics()) {
				sb.append(" [n+s condition element]");    			
			}
		}
		if (s instanceof LiteralStatement) {
			sb.append(" "+((LiteralStatement)s).getSValue());
		}
		if (s.isInferred())
			sb.append(" [Implied]");
		if (s.getSourceId() != null)
			sb.append(" [source:"+href(s.getSourceId(),dataSource)+"]");
		if (s.getPositedByNodeId() != null)
			sb.append(" [positedBy:"+href(s.getPositedByNodeId(),dataSource)+"]");
		for (Statement ss : s.getSubStatements())
			sb.append(" <<"+hrefStatement(g,ss,dataSource)+">> ");

		return sb.toString();
	}
	
	

	public Set<Map<String,String>> mapToOtherFormats(String res,String dataSource){
		Set<Map<String,String>> links = new HashSet<Map<String,String>>();
		for (String fmt : this.fmts) {
			Map<String,String> m = new HashMap<String,String>();
			m.put("format", fmt);
			m.put("link", "<a href=\"/" + this.getContextName() + "/" + dataSource + "/"+fmt+res+"\">"+fmt+"</a>");
			links.add(m);
		}
		return links;
	}
	public String hrefToOtherFormats(String res, String dataSource) {
		
		StringBuilder sb = new StringBuilder();
		int i=0;
		for (String fmt : this.fmts) {
			if (i>0) {
				sb.append(" | ");
			}
			sb.append("<a href=\"/" + this.getContextName() + "/" + dataSource + "/"+fmt+res+"\">"+fmt+"</a>");
			i++;
		}
		return "\nRelated resources: [ " + sb + " ]\n";
	}


	public String hrefGraph(String id,String dataSource) {
		return "<a href=\"/" + this.getContextName() + "/" + dataSource + "/html/node/"+id+"/graph\">"+id+"</a>";
	}

	@SuppressWarnings("unchecked")
	public Representation getTemplateRepresentation(String view, Map map) {
		Representation result = null;
		try {
			String path = view + ".ftl";
			Configuration fmc = new Configuration();
			fmc.setServletContextForTemplateLoading(((OBDRestApplication)this.getApplication()).getServerServlet().getServletContext(), ((OBDRestApplication)this.getApplication()).getConfiguration().getFremarkerTemplateDirectory());
			BeansWrapper wrapper = new BeansWrapper();
			wrapper.setExposureLevel(BeansWrapper.EXPOSE_ALL);
			fmc.setObjectWrapper(wrapper);
			map.put("baseUri", this.getRequest().getResourceRef(). getParentRef().toString());
			result = new TemplateRepresentation(path, fmc,map, MediaType.TEXT_HTML);
		} catch (Exception ex) {
			System.err.println("Unable to get Template Representation: " + ex.getMessage());
			result = null;
		}
		return result;
	}
	
	public String getContextName(){
		return ((OBDRestApplication)this.getApplication()).getServerServlet().getServletContext().getServletContextName();
	}
	
	
	protected String HTMLEntityEncode(String s){
		StringBuffer buf = new StringBuffer();
		int len = (s == null ? -1 : s.length());

		for ( int i = 0; i < len; i++ ){
			char c = s.charAt( i );
			if ( c>='a' && c<='z' || c>='A' && c<='Z' || c>='0' && c<='9' ){
				buf.append( c );
			} else {
				buf.append( "&#" + (int)c + ";" );
			}
		}
		
		return buf.toString();
	}

	protected Graph getGraph(String aspect) {
		// TODO Auto-generated method stub
		return null;
	}
}

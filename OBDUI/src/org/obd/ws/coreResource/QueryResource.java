package org.obd.ws.coreResource;

import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.TreeMap;

import org.obd.model.Node;
import org.obd.query.QueryTerm;
import org.obd.query.Translator;
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
 * 
 * @author cjm
 */
public class QueryResource extends NodeResource {

	private String dataSource;
	private String format;
	private String queryString;
	
	public QueryResource(Context context, Request request, Response response) {
		super(context, request, response);
		
		this.dataSource = (String) request.getAttributes().get("dataSource");
		this.format = (String) request.getAttributes().get("format");
		this.queryString = (String) request.getAttributes().get("queryString");
		if (this.queryString!=null){
			this.queryString = Reference.decode(this.queryString);
		}
		System.out.println("Query String is: " + this.queryString);
	}

	
	@Override
	public Representation getRepresentation(Variant variant) {
		
		
		
		if (this.format.equals("someformat")){
			// Do some other stuff....
			return new StringRepresentation(queryString,MediaType.TEXT_HTML);
		} else {
			
			TreeMap<String, Object> resourceMap = new TreeMap<String, Object>();
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
    		
    		
			
			if (this.queryString == "" || this.queryString == null){

				return getTemplateRepresentation("QueryForm",resourceMap);
				
			} else {
				// HTML Output.
				Translator t = new Translator();
				QueryTerm qt = null;
				try {
					qt = t.parse(queryString);
				} catch (Exception e1) {
					System.err.println("Error translating query.");
					e1.printStackTrace();
					return new StringRepresentation("ERROR: Unable to translate query " + queryString + "\n\n" + e1.getMessage(),MediaType.TEXT_HTML);
				}
				

	    		resourceMap.put("queryString",queryString);
	    		resourceMap.put("translatedQuery",qt.toString());
	    		
	    		
	    		Collection<Node> nodes = this.getShard(dataSource).getNodesByQuery(qt);
	    		resourceMap.put("resultCount", nodes.size());
	    		
	    		Collection<String> nodeIds = new ArrayList<String>();
	    		for (Node n : nodes){
	    			nodeIds.add(n.getId());
	    		}
	    		resourceMap.put("resultNodes", this.hashifyNodes(nodeIds, "/" + this.getContextName() + "/" + this.dataSource + "html/node/"));
	    		
	    		return getTemplateRepresentation("QueryResults",resourceMap);
			}
		}
		
	}



}

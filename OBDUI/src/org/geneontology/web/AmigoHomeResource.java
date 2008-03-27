package org.geneontology.web;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.Vector;
import org.obd.ws.coreResource.NodeResource;
import org.restlet.Context;
import org.restlet.data.MediaType;
import org.restlet.data.Request;
import org.restlet.data.Response;
import org.restlet.resource.Representation;
import org.restlet.resource.Variant;

/**
 * Resource for a node
 * 
 * @author cjm
 */
public class AmigoHomeResource extends NodeResource {

	/**
	 * Constructor.
	 * 
	 * @param context
	 *            The parent context.
	 * @param request
	 *            The request to handle.
	 * @param response
	 *            The response to return.
	 */
	public AmigoHomeResource(Context context, Request request, Response response) {
		super(context, request, response);
		getVariants().clear();
		getVariants().add(new Variant(MediaType.TEXT_HTML));
	}

	
	@Override
	public Representation getRepresentation(Variant variant) {
		TreeMap<String, Object> resourceMap = new TreeMap<String, Object>();
		Set<Map<String,String>> pathMaps = new HashSet<Map<String,String>>();
		
		// This is dumbed down for use with a single data source. Should be extended to use multiple data sources.
		String dataSource =  this.getOBDRestApplication().getResourceMap().keySet().toArray(new String[0])[0];
		
		for (String s : this.getOBDRestApplication().getConfiguration().getPathResourceMap().keySet()){
			if (!s.trim().equals("")){
				Map<String,String> m = new HashMap<String,String>();
				m.put("mappedPath", s);
				m.put("className", this.getOBDRestApplication().getConfiguration().getPathResourceMap().get(s).getSimpleName());
				pathMaps.add(m);
			}
		}
		
		//String[] mappedPaths = (String[]) this.getOBDRestApplication().getConfiguration().getPathResourceMap().keySet().toArray(new String[0]);
		Vector<String> messages = new Vector<String>();
		
		for (String key : this.getOBDRestApplication().getConfiguration().getSourceMessages().keySet()){
			messages.add((key + " message: " + this.getOBDRestApplication().getConfiguration().getSourceMessages().get(key)));
		}
		for (String message : this.getOBDRestApplication().getConfiguration().getPathMappingMessages()){
			messages.add(message);
		}
		
		resourceMap.put("contextName", this.getContextName());
		resourceMap.put("dataSource", dataSource);
		resourceMap.put("pathMaps",pathMaps);
		resourceMap.put("configurationMessages", messages);
		
		Representation result = getTemplateRepresentation("explorer",resourceMap);
		return result; 
	}



}

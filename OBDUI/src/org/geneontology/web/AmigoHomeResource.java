package org.geneontology.web;

import java.util.Map;
import java.util.TreeMap;
import java.util.Vector;

import org.obd.ws.coreResource.NodeResource;
import org.restlet.Context;
import org.restlet.data.MediaType;
import org.restlet.data.Request;
import org.restlet.data.Response;
import org.restlet.resource.Representation;
import org.restlet.resource.Variant;

import freemarker.template.SimpleHash;

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
		TreeMap<String, Object> map = new TreeMap<String, Object>();
		String[] dataSourceKeys = (String[]) this.getOBDRestApplication().getResourceMap().keySet().toArray(new String[0]);
		String[] mappedPaths = (String[]) this.getOBDRestApplication().getConfiguration().getPathResourceMap().keySet().toArray(new String[0]);
		Vector<String> messages = new Vector<String>();
		
		for (String key : this.getOBDRestApplication().getConfiguration().getSourceMessages().keySet()){
			messages.add((key + " message: " + this.getOBDRestApplication().getConfiguration().getSourceMessages().get(key)));
		}
		for (String message : this.getOBDRestApplication().getConfiguration().getPathMappingMessages()){
			messages.add(message);
		}
		
		map.put("contextName", this.getContextName());
		map.put("dataSources", dataSourceKeys);
		map.put("mappedPaths", mappedPaths);
		map.put("configurationMessages", messages);
		
		Representation result = getTemplateRepresentation("explorer",map);
		return result; 
	}



}

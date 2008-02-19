

package org.geneontology.web;

import java.util.TreeMap;

import org.obd.ws.NodeResource;
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
		TreeMap<String, Object> map = new TreeMap<String, Object>();
		map.put("shard",getShard());
		System.out.println("amigo home page");
		//return getTemplateRepresentation("explorer",map, "src/org/geneontology/web/pages/templates/");
		return getTemplateRepresentation("templates/explorer",map);
	}



}

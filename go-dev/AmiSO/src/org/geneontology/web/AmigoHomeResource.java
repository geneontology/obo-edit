

package org.geneontology.web;

import java.util.Collection;
import java.util.TreeMap;

import org.obd.model.Graph;
import org.obd.model.Node;
import org.obd.model.Statement;
import org.obd.model.TermView;
import org.obd.model.bridge.OBDJSONBridge;
import org.obd.model.bridge.OBDXMLBridge;
import org.obd.model.bridge.OBOBridge;
import org.obd.model.bridge.OWLBridge;
import org.obd.query.AnnotationLinkQueryTerm;
import org.obd.query.LinkQueryTerm;
import org.obd.query.impl.OBOSessionShard;
import org.obd.ws.NodeResource;
import org.obo.datamodel.OBOSession;
import org.restlet.Context;
import org.restlet.data.MediaType;
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
		return getTemplateRepresentation("explorer",map, "src/org/geneontology/web/pages/templates/");
	}



}

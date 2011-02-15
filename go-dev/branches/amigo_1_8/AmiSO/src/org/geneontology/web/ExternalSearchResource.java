

package org.geneontology.web;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

import org.obd.model.Graph;
import org.obd.model.Node;
import org.obd.model.Statement;
import org.obd.model.TermView;
import org.obd.model.bridge.OBDJSONBridge;
import org.obd.model.bridge.OBDXMLBridge;
import org.obd.model.bridge.OBOBridge;
import org.obd.model.bridge.OWLBridge;
import org.obd.query.LinkQueryTerm;
import org.obd.query.Shard;
import org.obd.query.impl.MultiShard;
import org.obd.query.impl.OBOSessionShard;
import org.obd.ws.NodeResource;
import org.obo.datamodel.OBOSession;
import org.obo.web.DatabaseSearchWrapper;
import org.obo.web.DatabaseSearchWrapper.SearchableDatabase;
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
public class ExternalSearchResource extends NodeResource {

	public ExternalSearchResource(Context context, Request request, Response response) {
		super(context, request, response);
	}

	@Override
	public Representation getRepresentation(Variant variant) {
		Representation result = null;


		Shard shard = getShard();
		/*
		 * experimental: search term expansion
		 */

		Map<SearchableDatabase,String> sdb2url = new HashMap<SearchableDatabase,String>();
		OBOSessionShard oshard = null;
		if (shard instanceof OBOSessionShard) {
			oshard = (OBOSessionShard)shard;
		}
		else if (shard instanceof MultiShard) {
			for (Shard s : ((MultiShard)shard).getShards()) {
				if (s instanceof OBOSessionShard) 
					oshard = (OBOSessionShard)s;
			}
		}
		else {
			
		}
		if (oshard != null) {
			OBOSession session = oshard.getSession();
			DatabaseSearchWrapper dsw = new DatabaseSearchWrapper(session);
			for (SearchableDatabase sdb : SearchableDatabase.values()) {
				dsw.setSearchableDatabase(sdb);
				String url =
					dsw.expandToSearchURL(session.getObject(getNodeId()));
				if (url != null)
					sdb2url.put(sdb, url);
				System.out.println(sdb+" => "+url);
			}
		}


		if (format == null) {
			format = "";
		}

		TreeMap<String, Object> map = new TreeMap<String, Object>();
		map.put("urlmap", sdb2url);
		map.put("focusId",getNodeId());

		return getTemplateRepresentation("templates/ExternalSearchView",map);
	}



}

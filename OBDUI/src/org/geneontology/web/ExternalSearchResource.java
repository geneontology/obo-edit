package org.geneontology.web;

import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;
import org.obd.query.Shard;
import org.obd.query.impl.MultiShard;
import org.obd.query.impl.OBOSessionShard;
import org.obd.ws.coreResource.NodeResource;
import org.obo.datamodel.OBOSession;
import org.obo.web.DatabaseSearchWrapper;
import org.obo.web.DatabaseSearchWrapper.SearchableDatabase;
import org.restlet.Context;
import org.restlet.data.Request;
import org.restlet.data.Response;
import org.restlet.resource.Representation;
import org.restlet.resource.Variant;

/**
 * Resource for a node
 * 
 * @author cjm
 */
public class ExternalSearchResource extends NodeResource {

	protected String dataSource;
	
	public ExternalSearchResource(Context context, Request request, Response response) {
		super(context, request, response);
		this.dataSource = (String) request.getAttributes().get("dataSource");
	}

	@Override
	public Representation getRepresentation(Variant variant) {
		
		Shard shard = getShard(this.dataSource);
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

package org.obd.ws.bioResource;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.TreeMap;

import org.obd.ws.coreResource.NodeResource;
import org.obd.ws.coreResource.utility.NodeTyper;
import org.restlet.Context;
import org.restlet.data.Reference;
import org.restlet.data.Request;
import org.restlet.data.Response;
import org.restlet.resource.Representation;
import org.restlet.resource.Variant;

public class GeneResource extends NodeResource{

	public GeneResource(Context context, Request request, Response response) {
		super(context, request, response);
	}
	
	public Representation getRepresentation(Variant variant){
		
		TreeMap<String, Object> resourceMap = new TreeMap<String, Object>();
		resourceMap.put("contextName", this.getContextName());
		resourceMap.put("dataSource", this.dataSource);
		
		if (this.getNode() != null && this.getNode().getLabel() != null){
			resourceMap.put("geneLabel", this.getNode().getLabel());
		}
		resourceMap.put("geneId", this.getNodeId());
		
		for (String genotypeId : NodeTyper.getGeneGenotypeIDs(this.getNodeId(),this.getShard(dataSource))){
			
		}
		
		try {
			InetAddress addr = InetAddress.getLocalHost();
			resourceMap.put("hostname",addr.getCanonicalHostName());
		} catch (UnknownHostException e) {
			System.err.println("Hostname fetching error: " + e.getMessage());
		}
		
		return getTemplateRepresentation("GeneNodeDetails",resourceMap);
	}
	
}
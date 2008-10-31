package org.obd.ws.coreResource;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;

import org.obd.model.stats.SimilarityPair;
import org.obd.ws.coreResource.sorter.NodeScoreComparator;
import org.restlet.Context;
import org.restlet.data.Reference;
import org.restlet.data.Request;
import org.restlet.data.Response;
import org.restlet.resource.Representation;
import org.restlet.resource.Variant;

import freemarker.template.SimpleHash;


public class SimilarityPairResource extends NodesResource{

	public SimilarityPairResource(Context context, Request request,Response response) {
		super(context, request, response);
	}
	
	@Override
    public Representation getRepresentation(Variant variant) {
		
		TreeMap<String, Object> resourceMap = new TreeMap<String, Object>();
		resourceMap.put("contextName", this.getContextName());
		resourceMap.put("dataSource", this.dataSource);
		resourceMap.put("nodeString", this.nodeString);
		resourceMap.put("encodedNodeString", Reference.encode(this.nodeString));
		resourceMap.put("manyNodes",true);
		
		try {
			InetAddress addr = InetAddress.getLocalHost();
			resourceMap.put("hostname",addr.getCanonicalHostName());
		} catch (UnknownHostException e) {
			System.err.println("Hostname fetching error: " + e.getMessage());
		}
		
		System.out.println("NodeSize:" + this.nodes.size());
		if (this.nodes.size() != 2){
			
		} else {
			SimilarityPair sp = this.getShard(dataSource).compareAnnotationsByAnnotatedEntityPair(nodes.get(0).getId(), nodes.get(1).getId());			
			this.getShard(dataSource).calculateInformationContentMetrics(sp);
			
			resourceMap.put("contentRatioScore",sp.getInformationContentRatio());
			resourceMap.put("basicSimilarityScore", sp.getBasicSimilarityScore());
			resourceMap.put("commonSubsumerAverageIC", sp.getCommonSubsumerAverageIC());
			resourceMap.put("similarityPair", sp);
			//resourceMap.put("maxContentNode", sp.getNodeWithMaximumInformationContent());
			
			resourceMap.put("node1", this.hashifyNode(nodes.get(0).getId(), ("/" + this.getContextName() + "/" + this.dataSource + "/html/node/" + Reference.encode(nodes.get(0).getId()))));
			resourceMap.put("node2", this.hashifyNode(nodes.get(1).getId(), ("/" + this.getContextName() + "/" + this.dataSource + "/html/node/" + Reference.encode(nodes.get(1).getId()))));
			
			//List<String> commonNodes = new ArrayList<String>(sp.getNonRedundantNodesInCommon());
			List<String> commonNodes = new ArrayList<String>(sp.getBestCommonSubsumers());
			
			//commonNodes.remove(sp.getNodeWithMaximumInformationContent());
			//if (sp.getNodeWithMaximumInformationContent()!=null){
				//commonNodes.add(0,sp.getNodeWithMaximumInformationContent());
			//}
			List<SimpleHash> hashifiedCommonNodes = new ArrayList<SimpleHash>();
			
			for (String nodeId : commonNodes){
				SimpleHash nodeHash = this.hashifyNode(nodeId, "/" + this.getContextName() + "/" + this.dataSource + "/html/node/" + Reference.encode(nodeId));
				nodeHash.put("contentScore", sp.getInformationContent(nodeId));
				hashifiedCommonNodes.add(nodeHash);
			}
			Collections.sort(hashifiedCommonNodes, new NodeScoreComparator());
			Collections.reverse(hashifiedCommonNodes);
			resourceMap.put("intersectionNodes", hashifiedCommonNodes);
			
			//Set<String> set1unique = new HashSet<String>(sp.getNonRedundantNodesInSet1());
			Set<String> set1unique = new HashSet<String>(sp.getAssertedNodesInSet1());
			//set1unique.removeAll(sp.getNodesInCommon());
			//Set<String> set2unique = new HashSet<String>(sp.getNonRedundantNodesInSet2());
			Set<String> set2unique = new HashSet<String>(sp.getAssertedNodesInSet2());
			//set2unique.removeAll(sp.getNodesInCommon());
				
			
			//resourceMap.put("set1unique", this.hashifyNodes(set1unique,("/" + this.getContextName() + "/" + this.dataSource + "/html/node/")));
			//resourceMap.put("set2unique", this.hashifyNodes(set2unique,("/" + this.getContextName() + "/" + this.dataSource + "/html/node/")));
			resourceMap.put("set1unique", hashifyAnnotationSet(sp, set1unique));
			resourceMap.put("set2unique", hashifyAnnotationSet(sp, set2unique));
		}
		
		return getTemplateRepresentation("SimilarityPairDetails",resourceMap);
		
	}
	
	private List<SimpleHash> hashifyAnnotationSet(SimilarityPair sp, Set<String> set) {
		Set<String> nric = sp.getNodesInCommon();
		List<SimpleHash> hashifiedSet = new ArrayList<SimpleHash>();
		for (String nodeId : set) {
			SimpleHash nodeHash = this.hashifyNode(nodeId, "/" + this.getContextName() + "/" + this.dataSource + "/html/node/" + Reference.encode(nodeId));
			String bcs = sp.getBestCommonSubsumer(nodeId);
			if (bcs == null) {
				nodeHash.put("contentScore", 0.0);
			}
			else {
				nodeHash.put("contentScore", sp.getInformationContent(bcs));
			}
			if (nric.contains(nodeId)) {
				nodeHash.put("matchType", "exactMatch");
			}
			hashifiedSet.add(nodeHash);
		}
		Collections.sort(hashifiedSet, new NodeScoreComparator());
		Collections.reverse(hashifiedSet);
		
		return hashifiedSet;
		
	}
	
}
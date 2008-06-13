package org.obd.ws.bioResource;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.TreeMap;
import org.obd.model.stats.SimilarityPair;
import org.obd.ws.coreResource.NodeResource;
import org.obd.ws.coreResource.utility.NodeTyper;
import org.restlet.Context;
import org.restlet.data.Reference;
import org.restlet.data.Request;
import org.restlet.data.Response;
import org.restlet.resource.Representation;
import org.restlet.resource.Variant;

import freemarker.template.SimpleHash;

public class GeneResource extends NodeResource{

	public GeneResource(Context context, Request request, Response response) {
		super(context, request, response);
	}
	
	public Representation getRepresentation(Variant variant){
		
		TreeMap<String, Object> resourceMap = new TreeMap<String, Object>();
		resourceMap.put("contextName", this.getContextName());
		resourceMap.put("dataSource", this.dataSource);
		try {
			InetAddress addr = InetAddress.getLocalHost();
			resourceMap.put("hostname",addr.getCanonicalHostName());
		} catch (UnknownHostException e) {
			System.err.println("Hostname fetching error: " + e.getMessage());
		}
		
		if (this.getNode() != null && this.getNode().getLabel() != null){
			resourceMap.put("geneLabel", this.getNode().getLabel());
		}
		resourceMap.put("geneId",this.getNodeId());
		
		List<SimpleHash> genotypesHash = new ArrayList<SimpleHash>();
		List<String> genotypeIds = NodeTyper.getGeneGenotypeIDs(this.getNodeId(),this.getShard(dataSource));
		
		
		for (String genotypeId : genotypeIds){
			SimpleHash genotype = this.hashifyNode(genotypeId, "/" + this.getContextName() + "/" + dataSource + "/html/node/" + Reference.encode(genotypeId));
			SimpleHash genotypeHash = new SimpleHash();
			genotypeHash.put("genotype", genotype);
			Collection<SimpleHash> as = this.getHashifiedStatements("annotations",genotypeId);
			if (as.size()>0){
				genotypeHash.put("annotationStatements", as);
			}
			genotypesHash.add(genotypeHash);
		}
		resourceMap.put("genotypes", genotypesHash);
		
		double[][] genotypeScores = new double[genotypeIds.size()][genotypeIds.size()];
		
		for (int i=0;i<genotypeIds.size();i++){
			for (int j=i;j<genotypeIds.size();j++){
				if (i!=j){
					System.out.println("Trying for similarity pair " + genotypeIds.get(j) + "\t" + genotypeIds.get(i));
					SimilarityPair sp = this.getShard(dataSource).compareAnnotationsByAnnotatedEntityPair(genotypeIds.get(j), genotypeIds.get(i));
					System.out.println("Metrics for: " + genotypeIds.get(i) + "\tvs\t" + genotypeIds.get(j)+":\t" + sp.getBasicSimilarityScore() + "\t" + sp.getSimilarityByInformationContentRatio() );
					this.getShard(dataSource).calculateInformationContentMetrics(sp);
					
					genotypeScores[j][i] = sp.getBasicSimilarityScore();
					genotypeScores[i][j] = sp.getSimilarityByInformationContentRatio();
					
					
				} else {
					genotypeScores[i][j] = -1;
				}
			}
		}
		
		resourceMap.put("genotypeComparaScores", genotypeScores);
		
		return getTemplateRepresentation("GeneNodeDetails",resourceMap);
	
	}
	
	
}
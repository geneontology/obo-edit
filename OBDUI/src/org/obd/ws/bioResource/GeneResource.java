package org.obd.ws.bioResource;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.obd.model.LinkStatement;
import org.obd.model.Statement;
import org.obd.model.stats.SimilarityPair;
import org.obd.query.LinkQueryTerm;
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
		
		resourceMap.put("encodedId", Reference.encode(this.getNodeId()));
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
		
		List<SimpleHash> altViews = new ArrayList<SimpleHash>();
		
		SimpleHash view = new SimpleHash();
		view.put("view", "node");
		view.put("href", "/" + this.getContextName() + "/" + this.dataSource + "/html/node/" + Reference.encode(this.nodeString));
		altViews.add(view);
		
		SimpleHash nodeView = new SimpleHash();
		nodeView.put("view", "exhibit");
		nodeView.put("href", "/" + this.getContextName() + "/" + this.dataSource + "/exhibit/node/" + Reference.encode(this.nodeString));
		altViews.add(nodeView);
		
		resourceMap.put("nodeViews", altViews);
		
		List<SimpleHash> genotypesHash = new ArrayList<SimpleHash>();
		List<String> genotypeIds = NodeTyper.getGeneGenotypeIDs(this.getNodeId(),this.getShard(dataSource));
		
		int annotationStatementsCount = 0;
		Map<String,Collection<LinkStatement>> gtAnnotMap = new HashMap<String,Collection<LinkStatement>>();
		for (String genotypeId : genotypeIds){
			SimpleHash genotype = this.hashifyNode(genotypeId, "/" + this.getContextName() + "/" + this.dataSource + "/html/node/" + Reference.encode(genotypeId));
			SimpleHash genotypeHash = new SimpleHash();
			genotypeHash.put("genotype", genotype);
			Collection<Statement> stmts = getGraph("annotation",getNodeId()).getStatements();
			Collection<LinkStatement> lstmts = new ArrayList<LinkStatement>();
			for (Statement s : stmts) {
				if (s instanceof LinkStatement)
					lstmts.add((LinkStatement) s);
			}
			gtAnnotMap.put(genotypeId, lstmts);
			Collection<SimpleHash> as = hashifyStatements(stmts, true,"html");
			
			annotationStatementsCount += as.size();
			if (as.size()>0){
				genotypeHash.put("annotationStatements", as);
			}
			genotypesHash.add(genotypeHash);
		}
		resourceMap.put("genotypes", genotypesHash);
		resourceMap.put("annotationStatementCount", annotationStatementsCount);
		
		double[][] genotypeScores = new double[genotypeIds.size()][genotypeIds.size()];
		
		for (int i=0;i<genotypeIds.size();i++){
			for (int j=i;j<genotypeIds.size();j++){
				if (i!=j){
					System.out.println("Trying for similarity pair " + genotypeIds.get(j) + "\t" + genotypeIds.get(i));
					//SimilarityPair sp = this.getShard(dataSource).compareAnnotationsByAnnotatedEntityPair(genotypeIds.get(j), genotypeIds.get(i));
					SimilarityPair sp = this.getShard(dataSource).compareAnnotationSetPair(gtAnnotMap.get(j), gtAnnotMap.get(i), new LinkQueryTerm());
					System.out.println("Metrics for: " + genotypeIds.get(i) + "\tvs\t" + genotypeIds.get(j)+":\t" + sp.getBasicSimilarityScore() + "\t" + sp.getInformationContentRatio() );
					this.getShard(dataSource).calculateInformationContentMetrics(sp);
					
					genotypeScores[j][i] = sp.getBasicSimilarityScore();
					genotypeScores[i][j] = sp.getInformationContentRatio();
					
				} else {
					genotypeScores[i][j] = -1;
				}
			}
		}
		
		resourceMap.put("genotypeComparaScores", genotypeScores);
		resourceMap.put("dbxrefs", this.getDBXrefs());
		
		return getTemplateRepresentation("GeneNodeDetails",resourceMap);
	
	}
	
	private Collection<SimpleHash> getDBXrefs(){
		Set<String> dbxrefIDs = new HashSet<String>();
		
		LinkQueryTerm lq = new LinkQueryTerm();
		lq.setNode(this.nodeString);
		lq.setRelation("oboInOwl:hasDbXref");
		for (Statement s : this.getShard(this.dataSource).getStatementsByQuery(lq)){
			dbxrefIDs.add(s.getTargetId());
		}
		return this.hashifyNodes(dbxrefIDs, "/" + this.getContextName() + "/" + this.dataSource + "/html/node/");
	}
	
	
}
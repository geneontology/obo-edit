package org.obd.ws.coreResource.utility;

import java.util.Collection;
import java.util.HashSet;

import org.obd.model.Node;
import org.obd.model.Statement;
import org.obd.query.LinkQueryTerm;
import org.obd.query.Shard;

public class NodeTyper{
	
	public enum Type {GENE,GENOTYPE,CVTERM,ORGANISM}
	
	public static Type getNodeType(String nodeId,Shard s){
		
		LinkQueryTerm lqt = new LinkQueryTerm();
		lqt.setNode(nodeId);
		lqt.setRelation("OBO_REL:is_a");
		lqt.setInferred(false);
		Collection<Statement> typeStatements = s.getStatementsByQuery(lqt);
		if (typeStatements.size() != 1){
			System.err.println("ERR: Too many is_a statements.");
			return null;
		} else {
			Node n = s.getNode(typeStatements.toArray(new Statement[0])[0].getTargetId());
			if (n != null && n.getLabel()!= null){
				if (n.getLabel().equals("gene")){
					return Type.GENE;
				} else if (n.getLabel().equals("genotype")){
					return Type.GENOTYPE;
				}
			}
		}
		return null;
	}
	
	public static String getGenotypeGeneId(String genotypeId,Shard s){
		LinkQueryTerm lqt = new LinkQueryTerm();
		lqt.setNode(genotypeId);
		lqt.setInferred(false);
		lqt.setRelation("OBO_REL:variant_of");
		Collection<Statement> statements = s.getStatementsByQuery(lqt);
		if (statements.size()!= 1){
			System.err.println("ERR: Too many instance_of statements.");
		} else {
			return statements.toArray(new Statement[0])[0].getTargetId();
		}
		return null;
	}
	
	public static Collection<String> getGeneGenotypeIDs(String geneId, Shard s){
		Collection<String> genotypeIds = new HashSet<String>();
		LinkQueryTerm lqt = new LinkQueryTerm();
		lqt.setTarget(geneId);
		lqt.setInferred(false);
		lqt.setRelation("OBO_REL:variant_of");
		for (Statement statement  : s.getStatementsByQuery(lqt)){
			genotypeIds.add(statement.getNodeId());
		}
		return genotypeIds;
	}
	
	public static String getOrganismId(String nodeId,Shard s){
		Type nodeType = NodeTyper.getNodeType(nodeId, s);
		if (nodeType != null && (nodeType.equals(Type.GENOTYPE)||nodeType.equals(Type.GENE))){
			if (nodeType.equals(Type.GENOTYPE)){
				nodeId = NodeTyper.getGenotypeGeneId(nodeId, s);
				if (nodeId != null){
					LinkQueryTerm lqt = new LinkQueryTerm();
				}
			}
		}
		return null;
	}
}

package org.obd.model.bridge;

import org.geneontology.db.model.Association;
import org.geneontology.db.model.DBXref;
import org.geneontology.db.model.GeneProduct;
import org.geneontology.db.model.Species;
import org.geneontology.db.model.Term;
import org.obd.model.Graph;
import org.obd.model.LinkStatement;
import org.obd.model.Node;

public class GhoulBridge {

	protected Graph graph;

	public GhoulBridge() {
		super();
		graph = new Graph();
	}

	public Graph getGraph() {
		return graph;
	}

	public void setGraph(Graph graph) {
		this.graph = graph;
	}

	public String translateId(Term t) {
		return t.getAcc();
	}
	public String translateId(GeneProduct gp) {
		return translateId(gp.getDbxref());
	}
	public String translateId(Species sp) {
		return "NCBITaxon:"+sp.getNcbi_taxa_id();
	}
	public String translateId(DBXref x) {
		return x.getDb_name()+":"+x.getAccession();
	}

	public Node translate(Term t) {
		Node n = new Node(translateId(t));
		n.setLabel(t.getName());
		return n;
	}

	public LinkStatement translate(Association assoc) {
		LinkStatement s = new LinkStatement();
		String gpid = translateId(assoc.getGene_product()); // populate
		String tid = translateId(assoc.getTerm());
		s.setNodeId(gpid);
		s.setTargetId(tid);
		if (assoc.getIs_not() != 0)
			s.setNegated(true);
		graph.addStatement(s);
		return s;
	}

	public Node translate(Species sp) {
		Node n = new Node(translateId(sp));
		n.setLabel(sp.getGenus()+" "+sp.getSpecies());
		graph.addNode(n);
		return n;
	}
	public Node translate(GeneProduct gp) {
		Node n = translate(gp.getDbxref());
		n.setLabel(gp.getSymbol());
		graph.addNode(n);
		return n;
	}
	public Node translate(DBXref x) {
		Node n = new Node(translateId(x));
		return n;
	}
}

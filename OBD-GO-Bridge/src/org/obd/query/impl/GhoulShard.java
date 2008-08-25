package org.obd.query.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import org.apache.log4j.Logger;
import org.geneontology.db.factory.GOobjectFactory;
import org.geneontology.db.model.Association;
import org.geneontology.db.model.GeneProduct;
import org.geneontology.db.model.Species;
import org.geneontology.db.model.Term;
import org.geneontology.db.test.GHOUL_UnitTest;
import org.geneontology.db.util.HibernateUtil;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.obd.model.Node;
import org.obd.model.Statement;
import org.obd.model.bridge.GhoulBridge;
import org.obd.model.stats.AggregateStatisticCollection;
import org.obd.model.stats.AggregateStatistic.AggregateType;
import org.obd.query.QueryTerm;
import org.obd.query.Shard.EntailmentUse;
import org.obd.query.Shard.GraphExpansionAlgorithm;
import org.obd.query.exception.ShardExecutionException;

public class GhoulShard extends AbstractShard {
	
	GOobjectFactory factory;

	private SessionFactory sessionFactory;
	protected final static Logger logger = Logger.getLogger(GhoulShard.class);
	

	
	public GhoulShard() {
		super();
		factory = initSessionFactory();
	}

	public GhoulShard(SessionFactory sf) {
		super();
		factory = initSessionFactory();
		setSessionFactory(sf);
	}

	public SessionFactory getSessionFactory() {
		return this.sessionFactory;
	}

	public void setSessionFactory(SessionFactory sf) {
		this.sessionFactory = sf;
	}

	private GOobjectFactory initSessionFactory() {
		this.getSessionFactory().getCurrentSession().beginTransaction();
		return (new GOobjectFactory(this.getSessionFactory()));
	}
	


	
	@Override
	public Node getNode(String id) {
		GhoulBridge b = new GhoulBridge();
		if (id.startsWith("NCBITaxon:")) {
			Species sp = (Species) factory.getSpeciesByTaxa(Integer.getInteger(id.replace("NCBITaxon:","")));
			return b.translate(sp);
		}
		GeneProduct gp = (GeneProduct) factory.getGPByDBXrefStr(id);
		if (gp != null) {
			return b.translate(gp);
		}
		Term  t = factory.getTermByAcc(id);
		if (t != null) {
			return b.translate(t);
		}
		
		return null;
	}

	private Query setXrefPair(Query hq, String xs) {
		int pos = xs.indexOf(':');
		String db = xs.substring(0, pos);
		String acc = xs.substring(pos+1);
		return hq.setString(0, db).setString(1, acc);
	}
	
	public Collection<Statement> getAnnotationStatementsForAnnotatedEntity(String id, 
			EntailmentUse entailment, GraphExpansionAlgorithm strategy) {
		GhoulBridge b = new GhoulBridge();
		Query hq = factory.getSession().createQuery("from Association as a where a.GeneProduct.dbxref.db_name = ? and a.GeneProduct.dbxref.accession = ?");
		Iterator<Association> it = setXrefPair(hq,id).iterate();
		return getAnnotationStatementsByIterator(it);
	}
	
	private Collection<Statement> getAnnotationStatementsByIterator(Iterator<Association> it) {
		Collection<Statement> stmts = new ArrayList<Statement>();
		GhoulBridge b = new GhoulBridge();
		while (it.hasNext()) {
			Association assoc = it.next();
			stmts.add(b.translate(assoc));
		}
		return stmts;
	}
	
	public Collection<Node> getAnnotatedEntitiesBelowNodeSet(
			Collection<String> ids, EntailmentUse entailment,
			GraphExpansionAlgorithm gea) {
		// TODO Auto-generated method stub
		return null;
	}

	public int getAnnotatedNodeCount() {
		// TODO Auto-generated method stub
		return 0;
	}

	public Integer getLinkAggregateQueryResults(QueryTerm queryTerm,
			AggregateType aggType) throws Exception {
		// TODO Auto-generated method stub
		return null;
	}

	public Collection<Node> getLinkStatementSourceNodes() {
		// TODO Auto-generated method stub
		return null;
	}

	public Integer getNodeAggregateQueryResults(QueryTerm queryTerm,
			AggregateType aggType) throws Exception {
		// TODO Auto-generated method stub
		return null;
	}

	public Collection<Node> getNodeSourceNodes() {
		// TODO Auto-generated method stub
		return null;
	}

	public Collection<Node> getNodesBelowNodeSet(Collection<String> ids,
			EntailmentUse entailment, GraphExpansionAlgorithm gea) {
		// TODO Auto-generated method stub
		return null;
	}

	public Collection<Node> getSourceNodes() {
		// TODO Auto-generated method stub
		return null;
	}

	public Collection<Statement> getStatements(String nodeId,
			String relationId, String targetId, String sourceId,
			Boolean useImplied, Boolean isReified) {
		// TODO Auto-generated method stub
		return null;
	}

	public Collection<Node> getNodesByQuery(QueryTerm queryTerm) {
		// TODO Auto-generated method stub
		return null;
	}

	public Collection<Statement> getStatementsByQuery(QueryTerm queryTerm) {
		// TODO Auto-generated method stub
		return null;
	}

	public void putNode(Node n) {
		// TODO Auto-generated method stub
		
	}

	public void putStatement(Statement s) {
		// TODO Auto-generated method stub
		
	}

	public void removeNode(String nid) throws ShardExecutionException {
		// TODO Auto-generated method stub
		
	}

	public AggregateStatisticCollection getSummaryStatistics() {
		// TODO Auto-generated method stub
		return null;
	}

}

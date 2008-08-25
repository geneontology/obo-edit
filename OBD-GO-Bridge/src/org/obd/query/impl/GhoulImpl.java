package org.obd.query.impl;

import java.util.Collection;

import org.apache.log4j.Logger;
import org.geneontology.db.factory.GOobjectFactory;
import org.geneontology.db.model.GeneProduct;
import org.geneontology.db.model.Term;
import org.geneontology.db.test.GHOUL_UnitTest;
import org.geneontology.db.util.HibernateUtil;
import org.hibernate.SessionFactory;
import org.obd.model.Node;
import org.obd.model.Statement;
import org.obd.model.bridge.GhoulBridge;
import org.obd.model.stats.AggregateStatisticCollection;
import org.obd.model.stats.AggregateStatistic.AggregateType;
import org.obd.query.QueryTerm;
import org.obd.query.exception.ShardExecutionException;

public class GhoulImpl extends AbstractShard {
	
	GOobjectFactory factory = initSessionFactory();

	private SessionFactory sessionFactory;
	protected final static Logger logger = Logger.getLogger(GhoulImpl.class);
	

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
		GeneProduct gp = (GeneProduct) factory.getGPByDBXrefStr(id);
		if (gp != null) {
			return b.translate(gp);
		}
		Term  t = factory.getTermByName(id);
		if (t != null) {
			return b.translate(t);
		}
		return null;
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

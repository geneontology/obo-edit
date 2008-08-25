package org.obd.test;


import java.util.Iterator;
import java.util.Set;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.apache.log4j.Logger;
import org.geneontology.db.factory.GOobjectFactory;
import org.geneontology.db.model.Association;
import org.geneontology.db.model.DB;
import org.geneontology.db.model.DBXref;
import org.geneontology.db.model.Evidence;
import org.geneontology.db.model.GOModel;
import org.geneontology.db.model.GeneProduct;
import org.geneontology.db.model.HomolSet;
import org.geneontology.db.model.MetaRelation;
import org.geneontology.db.model.ProductSeq;
import org.geneontology.db.model.Relation;
import org.geneontology.db.model.Sequence;
import org.geneontology.db.model.Species;
import org.geneontology.db.model.Term;
import org.geneontology.db.model.TermDBXref;
import org.geneontology.db.model.TermSynonym;
import org.geneontology.db.util.HibernateUtil;
import org.hibernate.SessionFactory;
import org.obd.model.Node;
import org.obd.query.Shard;
import org.obd.query.impl.GhoulShard;

public class GhoulShardTest extends TestCase{
	
	Shard shard;
	private SessionFactory sessionFactory;
	protected final static Logger logger = Logger.getLogger(GhoulShardTest.class);
	
	public GhoulShardTest(){
		try {
			
			this.sessionFactory = HibernateUtil.buildSessionFactory("hibernate.cfg.xml");
			shard = new GhoulShard(sessionFactory);

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	
	public void testShard() {
		Node n = shard.getNode("GO:0008150");
		logger.info("n="+n);
	}
	
}
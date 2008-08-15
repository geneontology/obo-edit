package org.geneontology.db.test;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.apache.log4j.Logger;
import org.geneontology.db.factory.GOobjectFactory;
import org.geneontology.db.model.GOModel;
import org.geneontology.db.util.HibernateUtil;
import org.hibernate.SessionFactory;


public class AbstractGOHibernateAPITest extends TestCase{
	
	private SessionFactory sessionFactory;
	protected final static Logger logger = Logger.getLogger(AbstractGOHibernateAPITest.class);

	
	public AbstractGOHibernateAPITest(){
		try {
			this.sessionFactory = HibernateUtil.buildSessionFactory("hibernate.cfg.xml");
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public SessionFactory getSessionFactory() {
		return this.sessionFactory;
	}

	public void setSessionFactory(SessionFactory sf) {
		this.sessionFactory = sf;
	}
	
	public GOobjectFactory initSessionFactory() {
		this.getSessionFactory().getCurrentSession().beginTransaction();
		return (new GOobjectFactory(this.getSessionFactory()));
	}
	
	public void logResult(GOModel model) {
		prettyPrint(model);
		Assert.assertTrue((model != null));
	}
	
	public void prettyPrint(GOModel model) {
		logger.info(model);
	}
}
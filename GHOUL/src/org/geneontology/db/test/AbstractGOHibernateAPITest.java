package org.geneontology.db.test;

import geneontology.db.util.HibernateUtil;
import junit.framework.TestCase;

import org.hibernate.SessionFactory;


public class AbstractGOHibernateAPITest extends TestCase{
	
	private SessionFactory sessionFactory;
	
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
	
}
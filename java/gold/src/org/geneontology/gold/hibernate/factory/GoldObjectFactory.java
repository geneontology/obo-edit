package org.geneontology.gold.hibernate.factory;

import java.io.File;
import org.geneontology.gold.hibernate.model.Cls;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.Configuration;

public class GoldObjectFactory {

	/** The local {@link SessionFactory} object used to retrieve data. */
	private static SessionFactory sf;
	
	public GoldObjectFactory(){
		if(sf == null){
			sf = buildFactory();
		}
	}
	
	private static SessionFactory buildFactory(){
		Configuration c = new Configuration().configure(new File("conf/hibernate.cfg.xml"));
		SessionFactory sessionFactory = c.buildSessionFactory();
		return sessionFactory;
	}
	
	
	/**
	 * 
	 * @return {@link SessionFactory} object
	 */
	public synchronized Session getSession() {
		Session session = sf.getCurrentSession();
		session.beginTransaction();
		return session;
	}
	
	
	public synchronized Cls getClassById(String id){
		Session session = getSession();
		return (Cls)session.createQuery("from cls where id = ?").setString(0, id).uniqueResult();
	}
}

package org.geneontology.gaf.hibernate;

import java.io.File;
import java.util.List;

import org.apache.log4j.Logger;
import org.geneontology.gold.hibernate.factory.GoldObjectFactory;
import org.geneontology.gold.hibernate.model.Ontology;
import org.geneontology.gold.io.postgres.DeltaQueryInterceptor;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.Configuration;

public class GafObjectsFactory {

	private static Logger LOG = Logger.getLogger(GoldObjectFactory.class);
	
	/** The local {@link SessionFactory} object used to retrieve data. */
	private static SessionFactory sf;
	
	private Session session;
	
	private boolean isDeltaFactory;
	 
	public GafObjectsFactory(){
		if(sf == null){
			sf = buildFactory();
		}
		session = sf.getCurrentSession();
	}
	
	
	private static SessionFactory buildFactory(){
		if(LOG.isDebugEnabled())
			LOG.debug("-");
		Configuration c = new Configuration().configure(new File("conf/hibernate.cfg.xml"));
		
		SessionFactory sessionFactory = c.buildSessionFactory();
		return sessionFactory;
	}
	
	
	/**
	 * 
	 * @return {@link SessionFactory} object
	 */
	public synchronized Session getSession() {
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		if(!session.isOpen()){
			if(isDeltaFactory)
				session = sf.openSession(new DeltaQueryInterceptor());
			else{
				session = sf.getCurrentSession();
			}
		}
	
		session.beginTransaction();
		
		return session;
	}
	
	
	public synchronized List<GeneAnnotation> getGeneAnnotations(String bioentity){

		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		List<GeneAnnotation> list =session.createQuery("from GeneAnnotation where bioentity=?")
							.setString(0, bioentity)
							.list();
		
		session.getTransaction().commit();
		
		return list;
	}
	
	
}

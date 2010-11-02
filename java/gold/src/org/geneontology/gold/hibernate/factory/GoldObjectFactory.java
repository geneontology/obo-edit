package org.geneontology.gold.hibernate.factory;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.SubclassOf;
import org.geneontology.gold.hibernate.model.SubclassOfMapping;
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
		return (Cls)session.createQuery("from Cls where id = ?").setString(0, id).uniqueResult();
	}
	
	public synchronized List<SubclassOf> getSubClassOfAssertions(String cls){
		Session session = getSession();
		List<SubclassOfMapping> results =session.createQuery("from SubclassOfMapping where cls = ?").setString(0, cls).list();
		List<SubclassOf> list = new ArrayList<SubclassOf>();
		
		for(SubclassOfMapping scm: results){
			list.add(new SubclassOf(scm.getClsByCls(), scm.getClsBySuperCls(), scm.getId().getOntology()));
		}
		
		return list;
		
		
	}

	
}

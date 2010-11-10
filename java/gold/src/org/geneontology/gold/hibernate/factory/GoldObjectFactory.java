package org.geneontology.gold.hibernate.factory;

import java.io.File;
import java.util.List;
import org.geneontology.gold.hibernate.model.AllSomeRelationship;
import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.ObjAlternateLabel;
import org.geneontology.gold.hibernate.model.Relation;
import org.geneontology.gold.hibernate.model.SubclassOf;
import org.geneontology.gold.io.postgres.DeltaQueryInterceptor;
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
	//	c.setInterceptor(new DeltaQueryInterceptor());
		
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
	
	/**
	 * This method is only called when a delta update of GOLD is processed.
	 * During delta update temporary tables are created. With the help of this
	 * session the Objects can be built from temporary tables
	 * @return
	 */
	public synchronized Session getDeltaInterceptorSession() {
		Session session = sf.openSession(new DeltaQueryInterceptor());
		sf.openSession();//This again sets a new default session for the rest of the application which
							//current session from the getSession metho.
		session.beginTransaction();
		
		return session;
	}
	
	
	public synchronized Cls getClassById(String id, Session session){
		return (Cls)session.createQuery("from Cls where id = ?").setString(0, id).uniqueResult();
	}
	
	public synchronized Cls getClassById(String id){
		return  getClassById(id, getSession());
		//Session session = getSession();
		//return (Cls)session.createQuery("from Cls where id = ?").setString(0, id).uniqueResult();
	}
	
	public synchronized List<SubclassOf> getSubClassOfAssertions(String cls){
		Session session = getSession();
		List<SubclassOf> results =session.createQuery("from SubclassOf where cls = ?").setString(0, cls).list();
		
		return results;
	}

	public synchronized SubclassOf getSubClassOfAssertion(String ontology, String superCls, String cls){
		Session session = getSession();
		SubclassOf results = (SubclassOf)session.createQuery("from SubclassOf where ontology = ? and super_cls=? cls = ?")
		.setString(0, ontology)
		.setString(1, superCls)
		.setString(2, cls).uniqueResult();
		
		return results;
	}

	
	public synchronized List<ObjAlternateLabel> getObjAlternateLabel(String obj){
		Session session = getSession();
		List<ObjAlternateLabel> results =session.createQuery("from ObjAlternateLabel where obj = ?").setString(0, obj).list();
		
		return results;
	}
	
	public synchronized ObjAlternateLabel getObjAlternateLabelByPk(String obj, String label){
		Session session = getSession();
		ObjAlternateLabel result =(ObjAlternateLabel)session.createQuery("from ObjAlternateLabel where obj = ? and label = ?")
		.setString(0, obj)
		.setString(1, label)
		.uniqueResult();
		
		return result;
	}


	public synchronized AllSomeRelationship getAllSomeRelationshipByPk(String ontology, String targetCls, String cls, String relation){
		Session session = getSession();
		AllSomeRelationship result  = (AllSomeRelationship)session.createQuery("from AllSomeRelationship where ontology = ? and target_cls = ? and cls = ? and relation = ?")
		.setString(0, ontology)
		.setString(1, targetCls)
		.setString(2, cls)
		.setString(3, relation)
		.uniqueResult();
		
		return result;
	}
	
	
	public synchronized List<AllSomeRelationship> getAllSomeRelationship(String cls){
		Session session = getSession();
		List<AllSomeRelationship> results =session.createQuery("from AllSomeRelationship where cls = ?").setString(0, cls).list();
		
		return results;
	}
	

	
	
	public synchronized Relation getRelation(String id){
		Session session = getSession();
		return (Relation)session.createQuery("from Relation where id = ?").setString(0, id).uniqueResult();
	}
	
	
}

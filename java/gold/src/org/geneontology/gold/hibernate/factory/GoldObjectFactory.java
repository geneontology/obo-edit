package org.geneontology.gold.hibernate.factory;

import java.io.File;
import java.util.List;

import org.apache.log4j.Logger;
import org.geneontology.gold.hibernate.model.AllSomeRelationship;
import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.ClsIntersectionOf;
import org.geneontology.gold.hibernate.model.ClsUnionOf;
import org.geneontology.gold.hibernate.model.DisjointWith;
import org.geneontology.gold.hibernate.model.EquivalentTo;
import org.geneontology.gold.hibernate.model.ObjAlternateLabel;
import org.geneontology.gold.hibernate.model.ObjDefinitionXref;
import org.geneontology.gold.hibernate.model.ObjXref;
import org.geneontology.gold.hibernate.model.Relation;
import org.geneontology.gold.hibernate.model.SubclassOf;
import org.geneontology.gold.io.postgres.DeltaQueryInterceptor;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.Configuration;

public class GoldObjectFactory {

	private static Logger LOG = Logger.getLogger(GoldObjectFactory.class);
	
	/** The local {@link SessionFactory} object used to retrieve data. */
	private static SessionFactory sf;
	
	private Session session;
	
	private boolean isDeltaFactory;
	
	private GoldObjectFactory(){
		if(sf == null){
			sf = buildFactory();
		}
		session = sf.getCurrentSession();
	}
	
	public static GoldObjectFactory buildDefaultFactory(){
		return new GoldObjectFactory();
	}
	
	
	public static GoldObjectFactory buildDeltaObjectFactory(){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		GoldObjectFactory factory = new GoldObjectFactory();
		
		factory.session = sf.openSession(new DeltaQueryInterceptor());
		factory.isDeltaFactory = true;
		
		return factory;
	}

	
	private static SessionFactory buildFactory(){
		if(LOG.isDebugEnabled())
			LOG.debug("-");
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
	
	public synchronized Cls getClassById(String id){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		return (Cls)session.createQuery("from Cls where id = ?").setString(0, id).uniqueResult();
	}
	
	public synchronized List<SubclassOf> getSubClassOfAssertions(String cls){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		List<SubclassOf> results =session.createQuery("from SubclassOf where cls = ?").setString(0, cls).list();
		
		return results;
	}

	public synchronized SubclassOf getSubClassOfAssertion(String ontology, String superCls, String cls){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		SubclassOf results = (SubclassOf)session.createQuery("from SubclassOf where ontology = ? and super_cls=? and cls = ?")
		.setString(0, ontology)
		.setString(1, superCls)
		.setString(2, cls).uniqueResult();
		
		return results;
	}

	
	public synchronized List<ObjAlternateLabel> getObjAlternateLabel(String obj){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		List<ObjAlternateLabel> results =session.createQuery("from ObjAlternateLabel where obj = ?").setString(0, obj).list();
		
		return results;
	}
	
	public synchronized ObjAlternateLabel getObjAlternateLabelByPk(String obj, String label){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		ObjAlternateLabel result =(ObjAlternateLabel)session.createQuery("from ObjAlternateLabel where obj = ? and label = ?")
		.setString(0, obj)
		.setString(1, label)
		.uniqueResult();
		
		return result;
	}


	public synchronized AllSomeRelationship getAllSomeRelationshipByPk(String ontology, String targetCls, String cls, String relation){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

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
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		List<AllSomeRelationship> results =session.createQuery("from AllSomeRelationship where cls = ?").setString(0, cls).list();
		
		return results;
	}
	

	public synchronized Relation getRelation(String id){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		return (Relation)session.createQuery("from Relation where id = ?").setString(0, id).uniqueResult();
	}
	
	public synchronized ObjXref getObjXref(String obj, String xref){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		return (ObjXref)session.createQuery("from ObjXref where obj = ? and xref=?")
			.setString(0, obj)
			.setString(1, xref)
			.uniqueResult();
	}

	public synchronized ObjDefinitionXref getObjDefinitionXref(String obj, String xref){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		return (ObjDefinitionXref)session.createQuery("from ObjDefinitionXref where obj = ? and xref=?")
			.setString(0, obj)
			.setString(1, xref)
			.uniqueResult();
	}
	
	public synchronized EquivalentTo getEquivalentTo(String cls, String equivalent_cls, String ontology){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		return (EquivalentTo)session.createQuery("from EquivalentTo where cls = ? and equivalent_cls = ? and ontology = ?")
			.setString(0, cls)
			.setString(1, equivalent_cls)
			.setString(2, ontology)
			.uniqueResult();
	}

	public synchronized DisjointWith getDisjointWith(String cls, String disjoint_cls, String ontology){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		return (DisjointWith)session.createQuery("from DisjointWith where cls = ? and disjoint_cls = ? and ontology = ?")
			.setString(0, cls)
			.setString(1, disjoint_cls)
			.setString(2, ontology)
			.uniqueResult();
	}


	public synchronized ClsUnionOf getClsUnionOf(String cls, String target_cls, String ontology){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		return (ClsUnionOf)session.createQuery("from ClsUnionOf where cls = ? and target_cls = ? and ontology = ?")
			.setString(0, cls)
			.setString(1, target_cls)
			.setString(2, ontology)
			.uniqueResult();
	}
	
	public synchronized ClsIntersectionOf getClsIntersectionOf(String cls, String target_cls, String ontology){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		return (ClsIntersectionOf)session.createQuery("from ClsIntersectionOf where cls = ? and target_cls = ? and ontology = ?")
			.setString(0, cls)
			.setString(1, target_cls)
			.setString(2, ontology)
			.uniqueResult();
	}

	
}

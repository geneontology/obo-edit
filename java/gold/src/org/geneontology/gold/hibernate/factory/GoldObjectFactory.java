package org.geneontology.gold.hibernate.factory;

import java.io.File;
import java.util.List;

import org.apache.log4j.Logger;
import org.geneontology.gold.hibernate.model.AllOnlyRelationship;
import org.geneontology.gold.hibernate.model.AllSomeRelationship;
import org.geneontology.gold.hibernate.model.AnnotationAssertion;
import org.geneontology.gold.hibernate.model.AnnotationProperty;
import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.ClsIntersectionOf;
import org.geneontology.gold.hibernate.model.ClsUnionOf;
import org.geneontology.gold.hibernate.model.DisjointWith;
import org.geneontology.gold.hibernate.model.EquivalentTo;
import org.geneontology.gold.hibernate.model.InferredAllSomeRelationship;
import org.geneontology.gold.hibernate.model.InferredSubclassOf;
import org.geneontology.gold.hibernate.model.NeverSomeRelationship;
import org.geneontology.gold.hibernate.model.ObjAlternateId;
import org.geneontology.gold.hibernate.model.ObjAlternateLabel;
import org.geneontology.gold.hibernate.model.ObjDefinitionXref;
import org.geneontology.gold.hibernate.model.ObjSubset;
import org.geneontology.gold.hibernate.model.ObjXref;
import org.geneontology.gold.hibernate.model.Ontology;
import org.geneontology.gold.hibernate.model.OntologyAlternateLabelType;
import org.geneontology.gold.hibernate.model.OntologyAnnotation;
import org.geneontology.gold.hibernate.model.OntologyImports;
import org.geneontology.gold.hibernate.model.OntologySubset;
import org.geneontology.gold.hibernate.model.Relation;
import org.geneontology.gold.hibernate.model.RelationChain;
import org.geneontology.gold.hibernate.model.RelationDisjointWith;
import org.geneontology.gold.hibernate.model.RelationEquivalenTo;
import org.geneontology.gold.hibernate.model.SubclassOf;
import org.geneontology.gold.hibernate.model.SubrelationOf;
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
	
	
	public synchronized List<Ontology> getOntologies(){

		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		List<Ontology> list =session.createQuery("from Ontology")
							.list();
		
		session.getTransaction().commit();
		
		return list;
	}
	
	public synchronized Ontology getOntology(String id){

		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		Ontology ont =(Ontology)session.createQuery("from Ontology where id = ?")
						.setString(0, id)
						.uniqueResult();
		
		session.getTransaction().commit();
		
		return ont;
	}


	public synchronized OntologyAnnotation getOntologyAnnotation(String ontology, String property, String annotationValue){

		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		OntologyAnnotation ont =(OntologyAnnotation)session.createQuery("from OntologyAnnotation where ontology = ?" +
				" and property=? and annotation_value=?")
						.setString(0, ontology)
						.setString(1, property)
						.setString(2, annotationValue)
						.uniqueResult();
		
		session.getTransaction().commit();
		
		return ont;
	}

	public synchronized ObjAlternateId getObjAlternateId(String obj, String id){

		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		ObjAlternateId ret =(ObjAlternateId)session.createQuery("from ObjAlternateId where obj = ? and id=?")
						.setString(0, obj)
						.setString(1, id)
						.uniqueResult();
		
		session.getTransaction().commit();
		
		return ret;
	}


	public synchronized RelationChain getRelationChain(String inferredRelation, String relation1, 
			String relation2){

		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		RelationChain ret =(RelationChain)session.createQuery("from RelationChain where inferred_relation = ? and relation1=?"
				+ " and relation2 = ?")
						.setString(0, inferredRelation)
						.setString(1, relation1)
						.setString(2, relation2)
						.uniqueResult();
		
		session.getTransaction().commit();
		
		return ret;
	}
	
	
	public synchronized AnnotationProperty getAnnotationProperty(String id){

		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		AnnotationProperty ret =(AnnotationProperty)session.createQuery("from AnnotationProperty where id = ?")
						.setString(0, id)
						.uniqueResult();
		
		session.getTransaction().commit();
		
		return ret;
	}

	public synchronized AnnotationAssertion getAnnotationAssertion(String ontolgy, String obj, 
			String targetObj, String relation){

		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		AnnotationAssertion ret =(AnnotationAssertion)session.createQuery("from AnnotationAssertion where ontology = ?"
				+ " and obj = ? and target_obj=? and relation=?")
						.setString(0, ontolgy)
						.setString(1, obj)
						.setString(2, targetObj)
						.setString(3, relation)
						.uniqueResult();
		
		session.getTransaction().commit();
		
		return ret;
	}
	
	
	public synchronized OntologyImports getOntologyImports(String ontology, String importsOntology){

		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		OntologyImports ont =(OntologyImports)session.createQuery("from OntologyImports where ontology = ?" +
				" and imports_ontology=?")
						.setString(0, ontology)
						.setString(1, importsOntology)
						.uniqueResult();
		
		session.getTransaction().commit();
		
		return ont;
	}
	
	public synchronized OntologySubset getOntologySubset(String id){

		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		OntologySubset ont =(OntologySubset)session.createQuery("from OntologySubset where id = ?")
						.setString(0, id)
						.uniqueResult();
		
		session.getTransaction().commit();
		
		return ont;
	}

	public synchronized ObjSubset getObjSubset(String obj, String ontologySubset){

		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		ObjSubset ont =(ObjSubset)session.createQuery("from ObjSubset where obj = ? and ontology_subset=?")
						.setString(0, obj)
						.setString(1, ontologySubset)
						.uniqueResult();
		
		session.getTransaction().commit();
		
		return ont;
	}
	
	
	
	public synchronized List<SubclassOf> getSubClassOfAssertions(String cls){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		List<SubclassOf> results =session.createQuery("from SubclassOf where cls = ?").setString(0, cls).list();
		
		return results;
	}

	public synchronized SubclassOf getSubClassOf(String ontology, String superCls, String cls){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		SubclassOf results = (SubclassOf)session.createQuery("from SubclassOf where ontology = ? and super_cls=? and cls = ?")
		.setString(0, ontology)
		.setString(1, superCls)
		.setString(2, cls).uniqueResult();
		
		return results;
	}

	public synchronized SubrelationOf getSubrelationOf(String ontology, String superRelation, String relation){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		SubrelationOf results = (SubrelationOf)session.createQuery("from SubrelationOf where ontology = ? and super_relation=? and relation = ?")
		.setString(0, ontology)
		.setString(1, superRelation)
		.setString(2, relation).uniqueResult();
		
		return results;
	}

	
	public synchronized List<ObjAlternateLabel> getObjAlternateLabel(String obj){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		List<ObjAlternateLabel> results =session.createQuery("from ObjAlternateLabel where obj = ?").setString(0, obj).list();
		
		return results;
	}
	
	public synchronized OntologyAlternateLabelType getOntologyAlternateLabelType(String id){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		OntologyAlternateLabelType result =(OntologyAlternateLabelType)session.createQuery("from OntologyAlternateLabelType where id = ?")
		.setString(0, id)
		.uniqueResult();
		
		return result;
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

	public synchronized NeverSomeRelationship getNeverSomeRelationship(String ontology, String targetCls, String cls, String relation){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		NeverSomeRelationship result  = (NeverSomeRelationship)session.createQuery("from NeverSomeRelationship where ontology = ? and target_cls = ? and cls = ? and relation = ?")
		.setString(0, ontology)
		.setString(1, targetCls)
		.setString(2, cls)
		.setString(3, relation)
		.uniqueResult();
		
		return result;
	}
	
	
	public synchronized AllOnlyRelationship getAllOnlyRelationship(String ontology, String targetCls, String cls, String relation){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		AllOnlyRelationship result  = (AllOnlyRelationship)session.createQuery("from AllOnlyRelationship where ontology = ? and target_cls = ? and cls = ? and relation = ?")
		.setString(0, ontology)
		.setString(1, targetCls)
		.setString(2, cls)
		.setString(3, relation)
		.uniqueResult();
		
		return result;
	}

	
	public synchronized AllSomeRelationship getAllSomeRelationship(String ontology, String targetCls, String cls, String relation){
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


	public synchronized RelationEquivalenTo getRelationEquivalenTo(String relation, 
			String equivalent_relation, String ontology){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		return (RelationEquivalenTo)session.createQuery("from RelationEquivalenTo where relation = ? " +
				"and equivalent_relation = ? and ontology = ?")
			.setString(0, relation)
			.setString(1, equivalent_relation)
			.setString(2, ontology)
			.uniqueResult();
	}
	
	
	public synchronized RelationDisjointWith getRelationDisjointWith(String relation, String disjoint_relation, 
			String ontology){
		
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		RelationDisjointWith results = (RelationDisjointWith)session.createQuery("from RelationDisjointWith where ontology = ? and disjoint_relation=? and relation = ?")
		.setString(0, ontology)
		.setString(1, disjoint_relation)
		.setString(2, relation).uniqueResult();
		
		return results;
		
	}
	
	
	public synchronized DisjointWith getDisjointWith(String cls, String disjoint_cls, String ontology){
		
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		DisjointWith results = (DisjointWith)session.createQuery("from DisjointWith where ontology = ? and disjoint_cls=? and cls = ?")
		.setString(0, ontology)
		.setString(1, disjoint_cls)
		.setString(2, cls).uniqueResult();
		
		return results;
		
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


	public synchronized InferredSubclassOf getInferredSubclassOf(String cls, String target_cls, String ontology){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		return (InferredSubclassOf)session.createQuery("from InferredSubclassOf where cls = ? and target_cls = ? and ontology = ?")
			.setString(0, cls)
			.setString(1, target_cls)
			.setString(2, ontology)
			.uniqueResult();
	}
	

	public synchronized InferredAllSomeRelationship getInferredAllSomeRelationship(String cls, String target_cls, String relation, String ontology){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Session session = getSession();
		return (InferredAllSomeRelationship)session.createQuery("from InferredAllSomeRelationship where cls = ? and target_cls = ? and relation = ? and ontology = ?")
			.setString(0, cls)
			.setString(1, target_cls)
			.setString(2, relation)
			.setString(3, ontology)
			.uniqueResult();
	}
	
	
}

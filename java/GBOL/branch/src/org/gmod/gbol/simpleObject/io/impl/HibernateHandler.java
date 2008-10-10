package org.gmod.gbol.simpleObject.io.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.gmod.gbol.simpleObject.AbstractSimpleObject;
import org.gmod.gbol.simpleObject.CVTerm;
import org.gmod.gbol.simpleObject.Feature;
import org.gmod.gbol.simpleObject.FeatureLocation;
import org.gmod.gbol.simpleObject.SimpleObjectIteratorInterface;
import org.gmod.gbol.simpleObject.io.SimpleObjectIOInterface;
import org.gmod.gbol.util.HibernateUtil;
import org.hibernate.HibernateException;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.SessionFactory;

public class HibernateHandler implements SimpleObjectIOInterface {

	private SessionFactory sf;
	private Session session;
	
	public HibernateHandler(String hibernateConfig) throws Exception
	{
		sf = HibernateUtil.buildSessionFactory(hibernateConfig);
		session = sf.openSession();
		beginTransaction();
	}
	
	public Collection<? extends AbstractSimpleObject> readAll()
			throws Exception {
		throw new RuntimeException("Not yet implemented");
		//return null;
	}
	
	public Collection<? extends Feature> getFeaturesByCVTerm(CVTerm cvterm)
	{
		List<Feature> features = new ArrayList<Feature>();
		String hql = "from Feature where type.name=? and type.cv.name=?";
		Query query = session.createQuery(hql);
		query.setString(0, cvterm.getName());
		query.setString(1, cvterm.getCv().getName());
		Iterator<?> i = query.iterate();
		while (i.hasNext()) {
			features.add((Feature)i.next());
		}
		return features;
	}
	
	public Collection<? extends Feature> getAllFeatures()
	{
		String hql = "from Feature";
		List<Feature> features = new ArrayList<Feature>();
		Iterator<?> i = session.createQuery(hql).iterate();
		while (i.hasNext()) {
			features.add((Feature)i.next());
		}
		return features;
	}
	
	public Collection<? extends Feature> getAllFeaturesByRange(FeatureLocation loc) throws Exception {
		String hql = "from Feature as f join f.featureLocations as fl where fl.fmin>=? and fl.fmax<=? and fl.strand=?";
		Query query = session.createQuery(hql);
		query.setInteger(0, loc.getFmin());
		query.setInteger(1, loc.getFmax());
		query.setInteger(2, loc.getStrand());
		List<Feature> features = new ArrayList<Feature>();
		Iterator<?> i = query.iterate();
		while (i.hasNext()) {
			Object []objs = (Object [])i.next();
			features.add((Feature)objs[0]);
		}
		return features;
	}


	public Collection<? extends Feature> getFeaturesByCVTermAndRange(CVTerm cvterm, FeatureLocation loc) throws Exception {
		String hql = "from Feature as f join f.featureLocations as fl where fl.fmin>=? and fl.fmax<=? and fl.strand=? and f.type.name=? and f.type.cv.name=?";
		Query query = session.createQuery(hql);
		query.setInteger(0, loc.getFmin());
		query.setInteger(1, loc.getFmax());
		query.setInteger(2, loc.getStrand());
		query.setString(3, cvterm.getName());
		query.setString(4, cvterm.getCv().getName());
		List<Feature> features = new ArrayList<Feature>();
		Iterator<?> i = query.iterate();
		while (i.hasNext()) {
			Object []objs = (Object [])i.next();
			features.add((Feature)objs[0]);
		}
		return features;
	}

	public boolean write(AbstractSimpleObject simpleObject) {
		try {
			session.saveOrUpdate(simpleObject);
		}
		catch (HibernateException e) {
			return false;
		}
		return true;
	}

	public boolean write(SimpleObjectIteratorInterface iter) {
		while (iter.hasNext()) {
			if (!write(iter.next())) {
				return false;
			}
		}
		return true;
	}
	
	public void beginTransaction()
	{
		session.getTransaction().begin();
	}
	
	public void commitTransaction()
	{
		session.getTransaction().commit();
	}
	
	public void rollbackTransaction()
	{
		session.getTransaction().rollback();
	}
	
	public void closeSession()
	{
		session.close();
	}

}

package org.gmod.gbol.simpleObject.io.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.gmod.gbol.simpleObject.AbstractSimpleObject;
import org.gmod.gbol.simpleObject.CVTerm;
import org.gmod.gbol.simpleObject.Feature;
import org.gmod.gbol.simpleObject.FeatureLocation;
import org.gmod.gbol.simpleObject.io.SimpleObjectIOInterface;
import org.gmod.gbol.util.HibernateUtil;
import org.hibernate.HibernateException;
import org.hibernate.Query;
import org.hibernate.SessionFactory;

public class HibernateHandler implements SimpleObjectIOInterface {

	private SessionFactory sf;
	
	public HibernateHandler(String hibernateConfig) throws Exception
	{
		sf = HibernateUtil.buildSessionFactory(hibernateConfig);
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
		Query query = sf.getCurrentSession().createQuery(hql);
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
		Iterator<?> i = sf.getCurrentSession().createQuery(hql).iterate();
		while (i.hasNext()) {
			features.add((Feature)i.next());
		}
		return features;
	}

	public boolean write(AbstractSimpleObject simpleObject) {
		try {
			sf.getCurrentSession().saveOrUpdate(simpleObject);
		}
		catch (HibernateException e) {
			return false;
		}
		return true;
	}

	public boolean write(Collection<AbstractSimpleObject> simpleObjects) {
		for (AbstractSimpleObject o : simpleObjects) {
			if (!write(o)) {
				return false;
			}
		}
		return true;
	}
	
	public void beginTransaction()
	{
		sf.getCurrentSession().getTransaction().begin();
	}
	
	public void commitTransaction()
	{
		sf.getCurrentSession().getTransaction().commit();
	}
	
	public void rollbackTransaction()
	{
		sf.getCurrentSession().getTransaction().rollback();
	}
	
	public void closeSession()
	{
		sf.getCurrentSession().close();
	}

	public Collection<? extends Feature> getAllFeaturesByRange(
			FeatureLocation loc) throws Exception {
		// TODO Auto-generated method stub
		return null;
	}

	public Collection<? extends Feature> getFeaturesByCVTermAndRange(
			CVTerm cvterm, FeatureLocation loc) throws Exception {
		// TODO Auto-generated method stub
		return null;
	}

}

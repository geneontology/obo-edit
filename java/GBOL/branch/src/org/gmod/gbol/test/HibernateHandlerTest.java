package org.gmod.gbol.test;

import java.util.Collection;

import junit.framework.TestCase;

import org.apache.log4j.PropertyConfigurator;
import org.gmod.gbol.simpleObject.CV;
import org.gmod.gbol.simpleObject.CVTerm;
import org.gmod.gbol.simpleObject.Feature;
import org.gmod.gbol.simpleObject.FeatureLocation;
import org.gmod.gbol.simpleObject.io.impl.HibernateHandler;

public class HibernateHandlerTest extends TestCase {

	private HibernateHandler handler;
	
	public HibernateHandlerTest() throws Exception
	{
		PropertyConfigurator.configure("testSupport/log4j.properties");
		try {
			handler = new HibernateHandler("testSupport/gbolOne.cfg.xml");
		}
		catch (Exception e) {
			e.printStackTrace();
			assertTrue(false);
			throw e;
		}
	}
	
	public void testGetFeaturesByCVTerm() throws Exception
	{
		Collection<? extends Feature> genes =
			handler.getFeaturesByCVTerm(new CVTerm("gene", new CV("SO")));
		assertEquals("Number of genes", genes.size(), 2);
		for (Feature f : genes) {
			System.out.println(f.getName());
		}
	}
	
	public void testGetAllFeaturesByRange() throws Exception
	{
		FeatureLocation loc = new FeatureLocation();
		loc.setFmin(1);
		loc.setFmax(1000);
		loc.setStrand(1);
		Collection<? extends Feature> features = handler.getAllFeaturesByRange(loc);
		assertEquals("Number of features (1-1000)", 4, features.size());
	}

	public void testGetFeaturesByCVTermAndRange() throws Exception
	{
		FeatureLocation loc = new FeatureLocation();
		loc.setFmin(1);
		loc.setFmax(1000);
		loc.setStrand(1);
		Collection<? extends Feature> features = handler.getFeaturesByCVTermAndRange(new CVTerm("gene", new CV("SO")), loc);
		assertEquals("Number of genes (1-1000)", 1, features.size());
	}
	
	protected void setUp()
	{
		//handler.beginTransaction();
	}
	
	protected void tearDown()
	{
		//handler.rollbackTransaction();
		//handler.closeSession();
	}
}

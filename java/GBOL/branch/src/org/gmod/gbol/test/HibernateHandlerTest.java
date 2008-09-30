package org.gmod.gbol.test;

import java.util.Collection;

import junit.framework.TestCase;

import org.apache.log4j.PropertyConfigurator;
import org.gmod.gbol.simpleObject.AbstractSimpleObject;
import org.gmod.gbol.simpleObject.CV;
import org.gmod.gbol.simpleObject.CVTerm;
import org.gmod.gbol.simpleObject.Feature;
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
		assertEquals(genes.size(), 2);
		for (Feature f : genes) {
			System.out.println(f.getName());
		}
	}
	
	protected void setUp()
	{
		handler.beginTransaction();
	}
	
	protected void tearDown()
	{
		//handler.rollbackTransaction();
		//handler.closeSession();
	}
}

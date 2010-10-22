package org.geneontology.gold.hibernate.test;

import org.geneontology.gold.hibernate.factory.GoldObjectFactory;
import org.geneontology.gold.hibernate.model.Cls;

import junit.framework.TestCase;

public class HibernateTests extends TestCase {

	
	public static void testGetClass(){
		GoldObjectFactory f = new GoldObjectFactory();
		
		String id = "go:123";
		
		Cls cls = f.getClassById(id);
		
		assertNotNull(cls);
		
		assertEquals(cls.getId(), id);
	}
	
}

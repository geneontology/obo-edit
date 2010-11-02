package org.geneontology.gold.hibernate.test;

import java.util.List;

import org.geneontology.gold.hibernate.factory.GoldObjectFactory;
import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.SubclassOf;
import junit.framework.TestCase;

public class HibernateTests extends TestCase {

	
	public static void testGetClass(){
		GoldObjectFactory f = new GoldObjectFactory();
		
		String id = "CARO:0000055";
		
		Cls cls = f.getClassById(id);
		
		assertNotNull(cls);
		
		assertEquals(cls.getId(), id);
	}
	
	public static void testGetSubClassOfAssertions(){
		GoldObjectFactory f = new GoldObjectFactory();

		
		String cls = "CARO:0000055";
		
		List<SubclassOf> list =  f.getSubClassOfAssertions(cls);
		
		System.out.println(list);
	}
}

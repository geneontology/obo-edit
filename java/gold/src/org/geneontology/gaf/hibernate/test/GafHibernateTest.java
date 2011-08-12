package org.geneontology.gaf.hibernate.test;

import org.geneontology.gaf.hibernate.Bioentity;
import org.geneontology.gaf.hibernate.GafObjectsFactory;
import org.geneontology.gaf.hibernate.GeneAnnotation;
import org.geneontology.gaf.hibernate.WithInfo;
import org.hibernate.Session;

import junit.framework.TestCase;

public class GafHibernateTest extends TestCase {

	
	public static void testAddBioentity(){
		GafObjectsFactory factory = new GafObjectsFactory();
		
		Session session = factory.getSession();
		
		Bioentity entity = new Bioentity();
		
		entity.setId("MGIMGI:1918911");
		entity.setDb("MGI");
		entity.setNcbiTaxonId("NCBIGene:"+10090);
		entity.setSymbol("0610005C13Rik");
		entity.setFullName("RIKEN");
		entity.setTypeCls("gene");
		
		
		session.saveOrUpdate(entity);
		
		WithInfo wi = new WithInfo("abc123","idxref");
		session.saveOrUpdate(wi);
		
		session.getTransaction().commit();
	}
	
	public static void testAddGeneAnnotation(){
		GafObjectsFactory factory = new GafObjectsFactory();
		
		Session session = factory.getSession();
		
		GeneAnnotation ga = new GeneAnnotation();
		
		ga.setBioentity("MGIMGI:1918911");
		ga.setCls("GO:0008150");
		ga.setReferenceId("MGI:MGI:2156816");
		ga.setEvidenceCls("ND");
		
		
		session.saveOrUpdate(ga);
		session.getTransaction().commit();
	}
	
	public static void testGetGeneAnnotation(){
		GafObjectsFactory factory = new GafObjectsFactory();
		
		GeneAnnotation ga = factory.getGeneAnnotations("MGIMGI:1918911").get(0);
		
		System.out.println(ga.getBioentityObject());

//		System.out.println(ga.getClsObject());
	
		assertNotNull( ga.getBioentityObject() );
		
	//	assertNotNull(ga.getClsObject());
		
	}
	
}

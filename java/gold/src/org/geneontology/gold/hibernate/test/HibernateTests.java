package org.geneontology.gold.hibernate.test;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.geneontology.gold.hibernate.model.AllSomeRelationship;
import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.GoldDeltaFactory;
import org.geneontology.gold.hibernate.model.GoldObjectFactory;
import org.geneontology.gold.hibernate.model.ObjAlternateLabel;
import org.geneontology.gold.hibernate.model.Relation;
import org.geneontology.gold.hibernate.model.SubclassOf;
import org.hibernate.Session;

import junit.framework.TestCase;

public class HibernateTests extends TestCase {

	
	public static void testGetClass(){
		GoldObjectFactory f = GoldObjectFactory.buildDefaultFactory();
		
		String id = "CARO:0000055";
		
		Cls cls = f.getClassById(id);
		
		assertNotNull(cls);
		
		assertEquals(cls.getId(), id);
	}
	
	public static void testGetSubClassOfAssertions(){
		GoldObjectFactory f = GoldObjectFactory.buildDefaultFactory();

		
		String cls = "CARO:0000055";
		
		List<SubclassOf> list =  f.getSubClassOfAssertions(cls);
		
		//System.out.println(list);
		
		for(SubclassOf sub: list){
			System.out.println(sub.getCls() + ", " + sub.getSuperCls() + ", " + sub.getOntology());
		}
	}
	
	
	public static void testGetObjAlternateLabel(){
		GoldObjectFactory f = GoldObjectFactory.buildDefaultFactory();

		
		String obj = "CARO:0000045";
		
		List<ObjAlternateLabel> list =  f.getObjAlternateLabel(obj);
		
		//System.out.println(list);
		
		for(ObjAlternateLabel label: list){
			
			System.out.println(label.getObj() + ", " + label.getLabel());
		}
		
	}

	public static void testGetAllSomeRelationship(){
		GoldObjectFactory f = GoldObjectFactory.buildDefaultFactory();

		
		String cls = "CARO:0000014";
		
		List<AllSomeRelationship> list =  f.getAllSomeRelationship(cls);
		
		//System.out.println(list);
		
		for(AllSomeRelationship rel: list){
			System.out.println(rel.getCls() + ", " + rel.getRelation() + ", " + rel.getTargetCls());
		}
		
	}

	
	public static void testgetRelation(){
		GoldObjectFactory f = GoldObjectFactory.buildDefaultFactory();
		
		String id = "TODO_part:of";
		
		Relation r = f.getRelation(id);

		System.out.println(r.getId());
	}
	
	public static void testDeltaClsObjects() throws Exception{
		GoldDeltaFactory gdf = new GoldDeltaFactory();
		List<Cls> list = gdf.buildClsDelta();
	//	gdf.getSession().getTransaction().commit();
		GoldObjectFactory gof = GoldObjectFactory.buildDefaultFactory();
		Session s = gof.getSession();
		for(Cls c:list){
			s.merge(c);
			//s.saveOrUpdate(c);
			System.out.println("c:************ " + c.getId());
		}
		s.getTransaction().commit();
	}
	
}

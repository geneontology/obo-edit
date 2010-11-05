package org.geneontology.gold.hibernate.test;

import java.util.List;

import org.geneontology.gold.hibernate.factory.GoldObjectFactory;
import org.geneontology.gold.hibernate.model.AllSomeRelationship;
import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.ObjAlternateLabel;
import org.geneontology.gold.hibernate.model.Relation;
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
		
		//System.out.println(list);
		
		for(SubclassOf sub: list){
			System.out.println(sub.getCls().getId() + ", " + sub.getSuperCls().getId() + ", " + sub.getOntology().getId());
		}
	}
	
	
	public static void testGetObjAlternateLabel(){
		GoldObjectFactory f = new GoldObjectFactory();

		
		String obj = "CARO:0000045";
		
		List<ObjAlternateLabel> list =  f.getObjAlternateLabel(obj);
		
		//System.out.println(list);
		
		for(ObjAlternateLabel label: list){
			System.out.println(label.getObj() + ", " + label.getLabel());
		}
		
	}

	public static void testGetAllSomeRelationship(){
		GoldObjectFactory f = new GoldObjectFactory();

		
		String cls = "CARO:0000014";
		
		List<AllSomeRelationship> list =  f.getAllSomeRelationship(cls);
		
		//System.out.println(list);
		
		for(AllSomeRelationship rel: list){
			System.out.println(rel.getCls().getId() + ", " + rel.getRelation().getId() + ", " + rel.getTargetCls().getId());
		}
		
	}

	
	public static void testgetRelation(){
		GoldObjectFactory f = new GoldObjectFactory();
		
		String id = "TODO_part:of";
		
		Relation r = f.getRelation(id);

		System.out.println(r.getId());
	}
	
	
	
}

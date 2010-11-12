package org.geneontology.gold.hibernate.factory;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.Vector;

import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.hibernate.model.AllSomeRelationship;
import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.GOModel;
import org.geneontology.gold.hibernate.model.ObjAlternateLabel;
import org.geneontology.gold.hibernate.model.Relation;
import org.geneontology.gold.hibernate.model.SubclassOf;
import org.geneontology.gold.io.DatabaseDialect;
import org.geneontology.gold.io.postgres.DeltaQueryInterceptor;
import org.geneontology.gold.io.postgres.PostgresDialect;
import org.hibernate.Session;

/**
 * This class builds delta from two databases of GOLD
 * ,i.e. gold.dbname and gold.deltatableprefix (see gold.properties file). 
 * It returns hibernate objects populated from delta tables (tables prefixed with delta word). 
 * The are need be merged in the gold.dbname database.
 * @see {@link PostgresDialect}, {@link DeltaQueryInterceptor}, 
 * @author Shahid Manzoor
 *
 */
public class GoldDeltaFactory {

	private DatabaseDialect db;
	
	//this session is created with DeltaQueryInterceptor object.
	//private Session session;
	
	private GoldObjectFactory goldObjFactory;
	
	
	public Session getSession(){
	//	session.beginTransaction();
		//return session;
		return goldObjFactory.getSession();
	}
	
	public GoldDeltaFactory()throws Exception{
		db = GeneOntologyManager.getInstance().buildDatabaseDialect();
		//this.session = new GoldObjectFactory().getDeltaInterceptorSession();
		goldObjFactory = GoldObjectFactory.buildDeltaObjectFactory();
	}
	
	public List<Cls> buildClsDelta() throws SQLException{
		Vector<Cls> list = new Vector<Cls>();

		Session session = goldObjFactory.getSession();
		
		ResultSet rs = db.getDelaData("cls");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			String id = rs.getString("id");
			list.add(goldObjFactory.getClassById(id) );
		}
		
		session.getTransaction().commit();
		session.close();
		return list;
	}

	public List<Relation> buildRelationDelta() throws SQLException{
		Vector<Relation> list = new Vector<Relation>();

		Session session = goldObjFactory.getSession();
		
		ResultSet rs = db.getDelaData("relation");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			String id = rs.getString("id");
			//list.add(goldObjFactory.getRelation(id, session) );
			Relation r = (Relation) session.load(Relation.class, id);
			list.add(r);
		}
		
		
	//	session.getTransaction().commit();
		//session.close();
	//	session.flush();
		return list;
	}
	

	public List<SubclassOf> buildSubclassOfDelta() throws SQLException{
		Vector<SubclassOf> list = new Vector<SubclassOf>();

		Session session = goldObjFactory.getSession();
		
		ResultSet rs = db.getDelaData("subclass_of");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			SubclassOf subc= goldObjFactory.getSubClassOfAssertion(rs.getString("ontology"), 
					rs.getString("super_cls"), rs.getString("cls"));
			list.add(subc);
		}
		
		
	//	session.getTransaction().commit();
		//session.close();
	//	session.flush();
		return list;
	}
	
	public List<ObjAlternateLabel> buildObjAlternateLabels() throws SQLException{
		Vector<ObjAlternateLabel> list = new Vector<ObjAlternateLabel>();

		Session session = goldObjFactory.getSession();
		
		ResultSet rs = db.getDelaData("obj_alternate_label");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			ObjAlternateLabel al = goldObjFactory.getObjAlternateLabelByPk(rs.getString("obj"), 
					rs.getString("label"));
			list.add(al);
		}
		
		
	//	session.getTransaction().commit();
	//	session.close();
	//	session.flush();
		//session.clear();
		//session.close();
		//session.flush();
		return list;

		
	}
	

	public List<AllSomeRelationship> buildAllSomeRelationships() throws SQLException{
		Vector<AllSomeRelationship> list = new Vector<AllSomeRelationship>();

		Session session = goldObjFactory.getSession();
		
		ResultSet rs = db.getDelaData("all_some_relationship");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			AllSomeRelationship asr = goldObjFactory.getAllSomeRelationshipByPk(rs.getString("ontology"), 
					rs.getString("target_cl"), rs.getString("cls"), rs.getString("relation"));
			list.add(asr);
		}
		
		
//		session.getTransaction().commit();
		//session.flush();
		return list;

		
	}
	
	
	
}

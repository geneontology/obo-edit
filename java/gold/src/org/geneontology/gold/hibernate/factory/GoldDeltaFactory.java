package org.geneontology.gold.hibernate.factory;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.Vector;

import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.hibernate.model.Cls;
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
	private Session session;
	
	private GoldObjectFactory goldObjFactory;
	
	
	public Session getSession(){
		session.beginTransaction();
		return session;
	}
	
	public GoldDeltaFactory()throws Exception{
		db = GeneOntologyManager.getInstance().buildDatabaseDialect();
		this.session = new GoldObjectFactory().getDeltaInterceptorSession();
		goldObjFactory = new GoldObjectFactory();
	}
	
	public List<Cls> buildClsDelta() throws SQLException{
		Vector<Cls> list = new Vector<Cls>();

		Session session = getSession();
		
		ResultSet rs = db.getDelaData("cls");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			String id = rs.getString("id");
			list.add(goldObjFactory.getClassById(id, session) );
		}
		
		
		session.getTransaction().commit();
		session.flush();
		return list;
	}
	
}

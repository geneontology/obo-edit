package org.geneontology.gold.io.postgres.test;

import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

import org.geneontology.gold.io.postgres.TsvFilelLoader;
import org.postgresql.core.BaseConnection;

import junit.framework.TestCase;

public class TsvFilelLoaderTest extends TestCase {

	
	public static void testLoadTables() throws ClassNotFoundException, SQLException, IOException{
	//	BaseConnection bc = (BaseConnection)getConnection();
		
		TsvFilelLoader loader = new TsvFilelLoader("postgres", "postgres", "localhost", "temp123");
		
		System.out.println("loading...");
		loader.loadTables("data");
	}
	
	
/*	private static Connection getConnection() throws ClassNotFoundException, SQLException{
		Class.forName("org.postgresql.Driver");
		System.out.println("connecting...");
		return  DriverManager.getConnection("jdbc:postgresql://localhost/temp12", "postgres", "postgres");
		
		
	}
*/	
	
}

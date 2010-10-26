package org.geneontology.gold.io.postgres.test;

import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

import org.geneontology.gold.io.postgres.TsvFilelLoader;
import org.postgresql.core.BaseConnection;

import junit.framework.TestCase;

public class TsvFilelLoaderTest extends TestCase {

	
	public static void testLoadTabes() throws ClassNotFoundException, SQLException, IOException{
		BaseConnection bc = (BaseConnection)getConnection();
		
		TsvFilelLoader loader = new TsvFilelLoader(bc);
		
		loader.loadTables();
	}
	
	private static Connection getConnection() throws ClassNotFoundException, SQLException{
		Class.forName("org.postgresql.Driver");
		
		return  DriverManager.getConnection("jdbc:postgresql://localhost/temp12", "postgres", "postgres");
		
		
	}
	
	
}

package org.geneontology.gold.io.postgres;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.Reader;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;


/**
 * 
 * @author Shahid Manzoor
 * This class executes the script through JDBC. This purpose of this class to automate
 * the database creation and its tables. 
 */
public class SchemaManager {

	
	/**
	 * 
	 * @param host
	 * @param username
	 * @param password
	 * @param db refers to the database name. If the database does not exist then
	 * 	the method creates new one of the name 'db'
	 * @param file refers to sql file which contains the create tables statements.
	 * @throws ClassNotFoundException
	 * @throws SQLException
	 * @throws FileNotFoundException
	 *
	 */
	public void loadSchemaSQL(String host, String username, String password, String db, String file) throws ClassNotFoundException, SQLException, FileNotFoundException{
		Class.forName("org.postgresql.Driver");
		
		
		Connection connection = null;
		//try to connect with database with all parameters. 
		try{
			connection = DriverManager.getConnection("jdbc:postgresql://localhost/"+db, username, password);
		}catch(Exception ex){
			
		}
		
		//if connection is null then try to create database.
		if(connection == null){
			connection = DriverManager.getConnection("jdbc:postgresql://localhost/", username, password);
			
			connection.createStatement().executeUpdate("CREATE DATABASE " + db);
			
			connection.close();
			
			
			connection = DriverManager.getConnection("jdbc:postgresql://localhost/"+db, username, password);
			
			
		}
		
		loadSchemaSQL(connection, file);
		
	}
	
	public void loadSchemaSQL(Connection connection, String file) throws FileNotFoundException{
		executeScript(new FileReader(file), connection);
	}
	
	
	private void executeScript(Reader r, Connection connection){
		if(connection == null)
			throw new RuntimeException("Can not perform operation as connection is establed with the RDBMS");

		try{
			//BufferedReader reader =new BufferedReader( new FileReader(new File(scriptFile)) );
                        BufferedReader reader =new BufferedReader(r);
			String line = null;

			StringBuffer buf = new StringBuffer();
			while((line = reader.readLine()) != null){
				
				
				line = line.trim();

				//if(line.startsWith("--"))
					//continue;
				
				int index = line.indexOf("--");
				if(index==0){//start of line
					continue;
				}else if (line.indexOf("--")>0){
					line = line.substring(0, index);
				}
				
				buf.append(line);

				if(line.endsWith(";")){

					try{
						System.out.println(buf.toString());
						Statement stmt = connection.createStatement();
						stmt.executeUpdate(buf.toString());
					}catch(Exception ex){
						ex.printStackTrace();
					}finally{
						buf = new StringBuffer();
					}
				}
				//stmt.executeUpdate("SCRIPT '" + file + "'");
				//return true;
			}

		}catch(Exception e){
			System.out.println("Warning:*******************");
			e.printStackTrace();
		}


	}
	
	
	
}

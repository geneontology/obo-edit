package org.geneontology.gold.io.postgres;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.Reader;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


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
	public void loadSchemaSQL(String host, String username, String password, String db, String file, String tablePrefix, boolean force) throws ClassNotFoundException, SQLException, FileNotFoundException{
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
		
		loadSchemaSQL(connection, file, tablePrefix, force);
		
	}
	
	public void loadSchemaSQL(Connection connection, String file, String tablePrefix, boolean force) throws FileNotFoundException{
		executeScript(new FileReader(file), connection, tablePrefix, force);
	}
	
	
	
	private void executeScript(Reader r, Connection connection, String tablePrefix, boolean force){
		if(connection == null)
			throw new RuntimeException("Can not perform operation as connection is establed with the RDBMS");

		tablePrefix = tablePrefix == null ? "" : tablePrefix.trim();
		
		try{
			//BufferedReader reader =new BufferedReader( new FileReader(new File(scriptFile)) );
                        BufferedReader reader =new BufferedReader(r);
			String line = null;

			StringBuffer buf = new StringBuffer();
			Pattern pattern = Pattern.compile("CREATE\\s*TABLE\\s*\\w+", Pattern.CASE_INSENSITIVE);
			Pattern Refspattern = Pattern.compile("REFERENCES\\s*\\w+", Pattern.CASE_INSENSITIVE);
			
			//this list maintains all sql statements to be executed
			//extracted from  the Reader r
	//		ArrayList<String> listSQLStatements = new ArrayList<String>();
			//if force is true then it contains drop tables statements
		//	ArrayList<String> listDelete = new ArrayList<String>();
			
			while((line = reader.readLine()) != null){
				
				line = line.trim();

				int index = line.indexOf("--");
				if(index==0){//start of line
					continue;
				}else if (line.indexOf("--")>0){
					line = line.substring(0, index);
				}
				
				buf.append(line);

				if(line.endsWith(";")){

					try{
						Statement stmt = connection.createStatement();
						
						String sql = buf.toString();
						
						if(tablePrefix.length()>0 || force){
							Matcher matcher = pattern.matcher(sql);
							if(matcher.find()){
								String tableName = matcher.group().trim();
								String s[] = tableName.split(" ");
								
								if(tablePrefix.length()>0){
									String repacelement = "CREATE TABLE " + tablePrefix +s[s.length-1];
									sql = sql.replace(tableName, repacelement);
									
									matcher = Refspattern.matcher(sql);
									
									while(matcher.find()){
										tableName = matcher.group().trim();
										s = tableName.split(" ");
										String replacement = " REFERENCES "+ tablePrefix + s[s.length-1];
										sql = sql.replace(tableName, replacement);
									}									
								}
								
								if(force){
									try{
										String drop = "DROP TABLE IF EXISTS " + tablePrefix+ s[s.length-1] +"  CASCADE";
										stmt.executeUpdate(drop);
									}catch(Exception ex){
										//ignore this
									}
								}
							}
							
							
						}

						System.out.println(sql);
						stmt.executeUpdate(sql);
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

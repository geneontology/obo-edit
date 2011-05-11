package org.geneontology.gold.io.postgres;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.Reader;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;


/**
 * 
 * @author Shahid Manzoor
 * This class executes the script through JDBC. This purpose of this class to automate
 * the database creation and its tables. 
 */
public class SchemaManager {

	private static Logger LOG = Logger.getLogger(SchemaManager.class);
	
	private static boolean DEBUG = LOG.isDebugEnabled();
	
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
		if(DEBUG)
			LOG.debug("--");
		
		Connection connection = getConnection(host, username, password, db);
		
		loadSchemaSQL(connection, file, tablePrefix, force);
		
	}
	
	private Connection getConnection(String host, String username, String password, String db) throws ClassNotFoundException, SQLException{
		
		Class.forName("org.postgresql.Driver");
		
		
		Connection connection = null;
		//try to connect with database with all parameters. 
		try{
			connection = DriverManager.getConnection("jdbc:postgresql://localhost/"+db, username, password);
		}catch(Exception ex){
			
		}
		
		//if connection is null then try to create database.
		if(connection == null){
			connection = DriverManager.getConnection("jdbc:postgresql://localhost/postgres", username, password);
			
			connection.createStatement().executeUpdate("CREATE DATABASE " + db);
			
			connection.close();
			
			
			connection = DriverManager.getConnection("jdbc:postgresql://localhost/"+db, username, password);
			
			
		}
		
		return connection;
	}

	private Connection getConnection() throws ClassNotFoundException, SQLException{
		
		GeneOntologyManager manager = GeneOntologyManager.getInstance();
		String dbHostName = manager.getGolddbHostName();
		String dbUserName = 		manager.getGolddbUserName();
		String dbUserPassword =		manager.getGolddbUserPassword();
		String dbName = manager.getGolddbName();
		
		Connection connection = getConnection(dbHostName, dbUserName, dbUserPassword, dbName);
		
		return connection;
	}
	
	public boolean isDatabaseInitialed() throws ClassNotFoundException, SQLException{
		Connection connection = getConnection();
		
		ResultSet rs = connection.getMetaData().getTables(null, "public", "%", new String[]{"TABLE"});
		
		boolean found = false;
		while(rs.next()){
			String table = rs.getString(3);
			if("cls".equals(table)){
				found = true;
				break;
			}
		}
		
		connection.close();
		
		return found;
	}
	
	public void loadSchemaSQL() throws ClassNotFoundException, SQLException, FileNotFoundException{
		Connection connection = getConnection();		

		GeneOntologyManager manager = GeneOntologyManager.getInstance();
		String sqlFile = manager.getOntSqlSchemaFileLocation();
		
		if(sqlFile == null){
			throw new FileNotFoundException("The sql file location is not configured (geneontology.gold.schemalocation) in the gold.properties file");
		}
		
		File f = new File(sqlFile);
		
		if(!f.exists()){
			throw new FileNotFoundException("The sql file location " + sqlFile + " is not found.");
		}

		loadSchemaSQL(connection, manager.getOntSqlSchemaFileLocation(), "", false);
		
		loadSchemaSQL(connection, manager.getGafSqlSchemaFileLocation(), "", false);

		
		f = f.getParentFile();
		
		for(File sql: f.listFiles()){
			if(manager.getGafSqlSchemaFileLocation().endsWith(sql.getName()) || manager.getOntSqlSchemaFileLocation().endsWith(sql.getName()))
				continue;
			
			if(sql.getName().endsWith(".sql")){
				
				
				
				loadSchemaSQL(connection, sql.getAbsolutePath(), "", false);
			}
		}
		
		
	}
	
	public void loadSchemaSQL(Connection connection, String file, String tablePrefix, boolean force) throws FileNotFoundException{
		LOG.info("Loading Schema file: " + file);
		
		
		executeScript(new FileReader(file), connection, tablePrefix, force);
	}
	
	
	
	private void executeScript(Reader r, Connection connection, String tablePrefix, boolean force){
		if(DEBUG)
			LOG.debug("--");
		
		
		if(connection == null)
			throw new RuntimeException("Can not perform operation as connection is establed with the RDBMS");

		tablePrefix = tablePrefix == null ? "" : tablePrefix.trim();
		String sql = "";
		try{
			//BufferedReader reader =new BufferedReader( new FileReader(new File(scriptFile)) );
                        BufferedReader reader =new BufferedReader(r);
			String line = null;

			StringBuffer buf = new StringBuffer();
			Pattern pattern = Pattern.compile("CREATE\\s*TABLE\\s*\\w+", Pattern.CASE_INSENSITIVE);
			Pattern Refspattern = Pattern.compile("REFERENCES\\s*\\w+", Pattern.CASE_INSENSITIVE);
			
			
			while((line = reader.readLine()) != null){
				
				line = line.trim();

				int index = line.indexOf("--");
				if(index==0){//start of line
					continue;
				}else if (line.indexOf("--")>0){
					line = line.substring(0, index);
				}
				
				buf.append(" ");
				buf.append(line);

				if(line.endsWith(";")){

					try{
						Statement stmt = connection.createStatement();
						
						sql = buf.toString();
						
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
										
										if(DEBUG)
											LOG.debug(drop);
										
										stmt.executeUpdate(drop);
									}catch(Exception ex){
										//ignore this
									}
								}
							}
							
							
						}

						if(DEBUG)
							LOG.debug(sql);
						
						
						
						stmt.executeUpdate(sql);
					}catch(Exception ex){
						LOG.error("Error occured in the " + sql + "sql statement.\n " + ex.getMessage(), ex);
						//ex.printStackTrace();
					}finally{
						buf = new StringBuffer();
					}
					
					
				}
				//stmt.executeUpdate("SCRIPT '" + file + "'");
				//return true;
			}

		}catch(Exception e){
			LOG.error("An Error occured while creating database schema", e);
		}

		LOG.info("DB schema created successfully");

	}
	
	
	
}

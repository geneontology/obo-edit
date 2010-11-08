package org.geneontology.gold.io.postgres;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.io.DatabaseDialect;

public class PostgresDialect implements DatabaseDialect {

	
	private Connection connection;
	
	
	
	@Override
	public Connection getConnect() {
		
		if(connection == null){
			try{
				Class.forName("org.postgresql.Driver");
				GeneOntologyManager manager = GeneOntologyManager.getInstance();
				connection = DriverManager.getConnection("jdbc:postgresql://"+ manager.getGolddbHostName() +"/"+manager.getGolddbName(), 
						manager.getGolddbUserName(), manager.getGolddbUserPassword());
			}catch(Exception ex){
				
			}
		}
		
		return connection;
	}
	
	@Override
	public String getDeltaQuery(String tableName) {
		if("cls".equals(tableName)){
			return "SELECT * from " + GeneOntologyManager.getInstance().getGoldDetlaTablePrefix() + tableName +" EXCEPT SELECT * from "+tableName;
		}
		
		return null;
	}

	@Override
	public ResultSet getDelaData(String tableName)  throws SQLException{
		String query =  getDeltaQuery(tableName);
		if(query == null)
			return null;

		return executeQuery(query);
	
	}
	
	
	private ResultSet executeQuery(String query) throws SQLException{
		getConnect();
		return connection.createStatement().executeQuery(query);
		
	}
	

}

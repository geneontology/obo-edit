package org.geneontology.gold.io.postgres;

import java.sql.Connection;
import java.sql.DriverManager;

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
				connection = DriverManager.getConnection("jdbc:postgresql://"+ manager.getGolddbHostName() +"/"+manager.getGoldDetlaDb(), 
						manager.getGolddbUserName(), manager.getGolddbUserPassword());
			}catch(Exception ex){
				
			}
		}
		
		return connection;
	}
	
	@Override
	public String getDeltaQuery(String tableName) {
		if("cls".equals(tableName)){
			return "SELECT * from cls1 EXCEPT SELECT * form cls";
		}
		
		return null;
	}

}

package org.geneontology.gold.io;

import java.sql.Connection;

public interface DatabaseDialect {

	public Connection getConnect();
	
	public String getDeltaQuery(String tableName);
	
	
}

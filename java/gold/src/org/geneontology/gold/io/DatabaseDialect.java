package org.geneontology.gold.io;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;

public interface DatabaseDialect {

	public Connection getConnect();
	
	public String getDeltaQuery(String tableName);
	
	public ResultSet getDelaData(String tableName) throws SQLException;
	
}

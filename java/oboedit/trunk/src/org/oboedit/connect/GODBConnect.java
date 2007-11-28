package org.oboedit.connect;

import java.sql.*;
import java.util.regex.*;

import org.obo.datamodel.LinkedObject;
import org.oboedit.graph.OENode;


public class GODBConnect {

	// Driver.
	private String DRIVER = "com.mysql.jdbc.Driver";
	
	// Connect.
	private String JDBC = "jdbc:mysql://";
	private String HOST = "spitz";
	private String DB = "go_latest_lite";
	private String USER = "";
	private String PASSWORD = "";
	
	// SQL.
	private String sqlFront = "SELECT count(*) AS count FROM term INNER JOIN graph_path ON (term.id = graph_path.term1_id) INNER JOIN association ON ( graph_path.term2_id = association.term_id) WHERE term.acc = '";
	private String sqlBack = "'";
	private String variable = "$$$";
	
	private Connection connector;

	
	// 
	public GODBConnect() {

		// Class.forName(xxx) loads the jdbc classes and creates a drivermanager class factory.
		try {
			Class.forName(DRIVER);
		} catch (ClassNotFoundException e) {
			System.err.println("ERROR___: " + e.getMessage());
		}
	}
	
	
	//
	public void connect() {

		// Assemble connection string and SQL string.
		String connection = JDBC + HOST + "/" + DB;

		// Try to connect.
		try {
			connector = DriverManager.getConnection(connection);
		} catch (SQLException e) {
			System.err.println("___ERROR (" + connection + "): " + e.getMessage());
		}
	}
	
	
	//
	public void resetConnection() {

		// Try to connect.
		try {
			connector.close();
		} catch (SQLException e) {
			System.err.println("___ERROR: " + e.getMessage());
		}

		// Assemble connection string and SQL string.
		String connection = JDBC + HOST + "/" + DB;

		// Try to connect again.
		try {
			connector = DriverManager.getConnection(connection);
		} catch (SQLException e) {
			System.err.println("___ERROR (" + connection + "): " + e.getMessage());
		}
	}

	
	//
	public int getCount( String sqlKey ) {

		int number = -1;
		ResultSet rs;

		//this.connect();
	
		try {
			String sql = sqlFront + sqlKey + sqlBack;
			Statement stmt = connector.createStatement();
			rs = stmt.executeQuery(sql);
			
			System.err.println("SQL: " + sql);
			while( rs.next() ){
				number = rs.getInt("count");
				System.err.println("SQL (RESULT): " + number);
			}
		} catch (SQLException e) {
			System.err.println("ERROR___: " + e.getMessage());
		}
		
		//this.disconnect();
		
		return number;
	}
	
	
	//
	public String getDB() { return DB; }
	public void setDB(String newDB) { DB = newDB; }
	
	
	//
	public String getHost() { return HOST; }
	public void setHost(String newHost) { HOST = newHost; }
	
	
	//
	public String getSQL() { return sqlFront + variable + sqlBack; }
	public void setSQL(String newSQL) {
		
		int stringLength = newSQL.length();
		int variableLength = variable.length();
		int index = newSQL.indexOf(variable);
		String foo = newSQL.substring(0, index);
		String bar = newSQL.substring(index + variableLength, stringLength);

		//System.err.println("___FRONT: " + foo);
		//System.err.println("___BACK: " + bar);
		
		sqlFront = foo;
		sqlBack = bar;
	}
	
	
	//
	public String getUser() { return USER; }
	public void setUser(String newUser) { USER = newUser; }
	
	
	//
	public String getPassword() { return PASSWORD; }
	public void setPassword(String newPassword) { PASSWORD = newPassword; }
}

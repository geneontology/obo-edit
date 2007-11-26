package org.oboedit.connect;

import java.sql.*;
import org.obo.datamodel.LinkedObject;
import org.oboedit.graph.OENode;

public class GODBConnect {

	// Connector and DB.
	private static final String DRIVER = "com.mysql.jdbc.Driver";
	private static final String DB = "jdbc:mysql://spitz/go_latest_lite";
	
	private Connection connector;

	public GODBConnect() {

		// Class.forName(xxx) loads the jdbc classes and creates a drivermanager class factory
		try {
			Class.forName(DRIVER);
		} catch (ClassNotFoundException e) {
			System.err.println("ERROR___: " + e.getMessage());
		}
	}
	
	//
	public void connect() {

		// Try to connect.
		try {
			connector = DriverManager.getConnection(DB);
		} catch (SQLException e) {
			System.err.println("ERROR___: " + e.getMessage());
		}
	}
	
	//
	public void disconnect() {
		try {
			connector.close();
		} catch (SQLException e) {
			System.err.println("ERROR___: " + e.getMessage());
		}
	}

	//
	public int getAssociationCount( String str ) {

		int number = -1;
		ResultSet rs;

		//System.err.println("___SETH___ into getData(" + str + ")");
	
		try {
			String front = "SELECT count(*) AS count FROM term INNER JOIN graph_path ON (term.id = graph_path.term1_id) INNER JOIN association ON ( graph_path.term2_id = association.term_id) WHERE term.acc = '";
			String back = "'";
			String sql = front + str + back;
			Statement stmt = connector.createStatement();
			rs = stmt.executeQuery(sql);
			
			//System.err.println("___SETH___ Checking counts...");
			System.err.println("SQL: " + sql);
			while( rs.next() ){
				number = rs.getInt("count");
				System.err.println("SQL (RESULT): " + number);
			}
		} catch (SQLException e) {
			System.err.println("ERROR___: " + e.getMessage());
		}

		return number;
	}
	
}

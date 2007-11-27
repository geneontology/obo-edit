package org.oboedit.connect;

import java.sql.*;
import org.obo.datamodel.LinkedObject;
import org.oboedit.graph.OENode;

public class OBDConnect {

	// Connector and DB.
	private static final String DRIVER = "org.postgresql.Driver";
	private static final String DB = "jdbc:postgresql://spitz.lbl.gov:5432/obd_phenotype_full";
	
	private Connection connector;

	public OBDConnect() {

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
			connector = DriverManager.getConnection(DB,"cjm","");
			System.out.println("connected: "+connector);
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
			String front = "SELECT annotation_count FROM annotation_count_by_class INNER JOIN node USING (node_id) WHERE uid = '";
			String back = "'";
			String sql = front + str + back;
			Statement stmt = connector.createStatement();
			rs = stmt.executeQuery(sql);
			
			//System.err.println("___SETH___ Checking counts...");
			System.err.println("SQL: " + sql);
			while( rs.next() ){
				number = rs.getInt("annotation_count");
				System.err.println("SQL (RESULT): " + number);
			}
		} catch (SQLException e) {
			System.err.println("ERROR___: " + e.getMessage());
		}

		return number;
	}
	
}

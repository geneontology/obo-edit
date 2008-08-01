package org.bbop.server;

import java.sql.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.bbop.client.model.GOGeneProduct;

public class GOConnect {

    // Driver.
    private String DRIVER = "com.mysql.jdbc.Driver";
    
    // Connect.
    private String JDBC = "jdbc:mysql://";
    //private String HOST = "localhost";
    private String HOST = "spitz";
    private String DB = "go_latest_lite";
    private String USER = "";
    private String PASSWORD = "";
    
    private Connection connector;
    
    private String SNIFF = "";
    
    // 
    public GOConnect() {

        // Class.forName(xxx) loads the jdbc classes and creates a drivermanager class factory.
        try {
                Class.forName(DRIVER);
        } catch (ClassNotFoundException e) {
                System.err.println("ERROR___: " + e.getMessage());
        }
        
    	// Assemble connection string and SQL string.
    	String connection = JDBC + HOST + "/" + DB;

    	// Try to connect.
    	try {
    		connector = DriverManager.getConnection(connection);
    	} catch (SQLException e) {
    		System.err.println("___ERROR (" + connection + "): " + e.getMessage());
    	}
    	
    	SNIFF = Double.toString(Math.random());
    	System.err.println("___Connection: (" + connector + "): " + SNIFF);
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
    public String[][] makeQuery( String sql ) {
       	
    	ResultSet rs;
		LinkedList<String[]> list = new LinkedList<String[]>();
		
    	try {
    		
    		//
    		Statement stmt = connector.createStatement();
    		rs = stmt.executeQuery(sql);
    		ResultSetMetaData rsmd = rs.getMetaData();
    		
    		System.err.println("SQL: " + sql);
    		System.err.println("\tDB: " + DB);
    		System.err.println("\tHOST: " + HOST);
    		System.err.println("\tUSER: " + USER);
    		System.err.println("\tPASSWORD: " + PASSWORD);

    		int numCols = rsmd.getColumnCount();
    		System.err.println("\tRESULT COUNT: " + numCols);
    		while( rs.next() ){
    			
				System.err.println("SQL RESULT NUMBER: " + rs.getRow());
    			String[] info = new String[numCols];
    			for( int i = 1; i <= numCols; i++ ){
    				info[i-1] = rs.getString(i);
    				System.err.println("SQL (RESULT " + i + "): " + info[i-1]);
    			}

       			list.add((String[])info);
    		}    		
    		
    	} catch (SQLException e) {
    		System.err.println("ERROR___: " + e.getMessage());
    	}
     	
    	return (String[][]) list.toArray(new String[0][0]);
    }
    
    //
    public String getDB() { return DB; }
    public void setDB(String newDB) { DB = newDB; }
    
    //
    public String getHost() { return HOST; }
    public void setHost(String newHost) { HOST = newHost; }
        
    //
    public String getUser() { return USER; }
    public void setUser(String newUser) { USER = newUser; }

    //
    public String getPassword() { return PASSWORD; }
    public void setPassword(String newPassword) { PASSWORD = newPassword; }
}

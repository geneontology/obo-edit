package org.bbop.server;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class GODBConnect {

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
    public GODBConnect() {

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
    public Integer getGPCount( String sqlLever ) {

    	// SQL.
        String sqlFront = "SELECT count(*) AS count FROM association INNER JOIN term ON (association.term_id = term.id) WHERE term.acc = '";
        String sqlBack = "'";
    	
    	int number = -1;
    	ResultSet rs;
    	
    	try {
    		String sql = sqlFront + sqlLever + sqlBack;
    		Statement stmt = connector.createStatement();
    		rs = stmt.executeQuery(sql);
                    
    		System.err.println("SQL: " + sql);
    		System.err.println("\tDB: " + DB);
    		System.err.println("\tHOST: " + HOST);
    		System.err.println("\tUSER: " + USER);
    		System.err.println("\tPASSWORD: " + PASSWORD);
    		while( rs.next() ){
    			number = rs.getInt("count");
    			System.err.println("SQL (RESULT): " + number);
    		}

    	} catch (SQLException e) {
    		System.err.println("ERROR___: " + e.getMessage());
    	}
    	
        return new Integer(number);
    }
  
    //
    public String[] getGPs( String sqlLever, int limit, int offset ) {

    	// SQL.
        String sqlFront =  "SELECT SQL_CALC_FOUND_ROWS gene_product.id AS gp FROM term INNER JOIN association ON (association.term_id = term.id) INNER JOIN gene_product ON (gene_product.id = association.gene_product_id) WHERE term.acc = '";
       	String sqlBack = "' LIMIT " + limit + " OFFSET " + offset;
       	
    	ArrayList gps = new ArrayList();
    	ResultSet rs;

    	String count = new String();
    	ResultSet cs;
    	
    	try {
    		
    		// Get GPs.
    		String sql = sqlFront + sqlLever + sqlBack;
    		Statement stmt = connector.createStatement();
    		rs = stmt.executeQuery(sql);
                    
    		System.err.println("SQL: " + sql);
    		System.err.println("\tDB: " + DB);
    		System.err.println("\tHOST: " + HOST);
    		System.err.println("\tUSER: " + USER);
    		System.err.println("\tPASSWORD: " + PASSWORD);
    		while( rs.next() ){
    			String gp = rs.getString("gp");
    			gps.add(gp);
    			System.err.println("SQL (RESULT): " + gp);
    		}    		
    		
    	} catch (SQLException e) {
    		System.err.println("ERROR___: " + e.getMessage());
    	}
     
    	try {
		
    		// Get GPs.
    		String sql = "SELECT FOUND_ROWS() AS tcount";
    		Statement stmt = connector.createStatement();
    		cs = stmt.executeQuery(sql);
                
    		System.err.println("SQL: " + sql);
    		System.err.println("\tDB: " + DB);
    		System.err.println("\tHOST: " + HOST);
    		System.err.println("\tUSER: " + USER);
    		System.err.println("\tPASSWORD: " + PASSWORD);
    		while( cs.next() ){
    			count = Integer.toString( cs.getInt("tcount") );
    			gps.add(count);
    			System.err.println("SQL (RESULT): " + count);
    		}
				
    	} catch (SQLException e) {
    		System.err.println("ERROR___: " + e.getMessage());
    	}
    
    	String[] out_gps = new String[gps.size()];
    	//System.err.println("array size: " + out_gps.length);
    	//System.err.println("list size: " + gps.size() );
    	
    	// Add count.
    	//out_gps[0] = count;
	
    	// Add GPs.
    	for(int i = 0; i < out_gps.length; i++ ){
    		out_gps[i] = (String) gps.get(i);
    		//System.err.println("index: " + i + ", " + (i -1));
    	}	
	
    	return out_gps;
    }
    
    //
    public Boolean isTerm( String sqlLever ) {

    	// SQL.
    	String sqlFront = "SELECT count(*) AS count FROM term WHERE term.acc = '";
   		String sqlBack = "'";
    	
    	boolean is_a_term_p = false;
    	ResultSet rs;
    	
    	try {
    		String sql = sqlFront + sqlLever + sqlBack;
    		Statement stmt = connector.createStatement();
    		rs = stmt.executeQuery(sql);
                    
    		System.err.println("SQL: " + sql);
    		System.err.println("\tDB: " + DB);
    		System.err.println("\tHOST: " + HOST);
    		System.err.println("\tUSER: " + USER);
    		System.err.println("\tPASSWORD: " + PASSWORD);
    		while( rs.next() ){
    			int number = rs.getInt("count");
    			System.err.println("SQL (RESULT): " + number);
    			if( number == 1 ){
    				is_a_term_p = true;
    			}
    		}

    	} catch (SQLException e) {
    		System.err.println("ERROR___: " + e.getMessage());
    	}
        
        return new Boolean(is_a_term_p);
    }
    
    //
    public String[] getTermInfo( String acc ) {

    	// SQL.
        String sqlFront =  "SELECT * FROM term WHERE acc = '";
       	String sqlBack = "'";
       	
    	ResultSet rs;
    	String[] info = new String[6];

    	try {
    		
    		// Get GPs.
    		String sql = sqlFront + acc + sqlBack;
    		Statement stmt = connector.createStatement();
    		rs = stmt.executeQuery(sql);
                    
    		System.err.println("SQL: " + sql);
    		System.err.println("\tDB: " + DB);
    		System.err.println("\tHOST: " + HOST);
    		System.err.println("\tUSER: " + USER);
    		System.err.println("\tPASSWORD: " + PASSWORD);
    		while( rs.next() ){
    			info[0] = rs.getString("id");
      			info[1] = rs.getString("name");
      			info[2] = rs.getString("term_type");
      			info[3] = rs.getString("acc");
      			info[4] = rs.getString("is_obsolete");
      			info[5] = rs.getString("is_root");
    			System.err.println("SQL (RESULT 0): " + info[0]);
      			System.err.println("SQL (RESULT 1): " + info[1]);
       			System.err.println("SQL (RESULT 2): " + info[2]);
      			System.err.println("SQL (RESULT 3): " + info[3]);
       			System.err.println("SQL (RESULT 4): " + info[4]);
      			System.err.println("SQL (RESULT 5): " + info[5]);
    		}    		
    		
    	} catch (SQLException e) {
    		System.err.println("ERROR___: " + e.getMessage());
    	}
     	
    	return info;
    }
 
    //
    public String[] getGPInfo( String id ) {

    	// SQL.
        String sqlFront =  "SELECT * FROM gene_product WHERE id = '";
       	String sqlBack = "'";
       	
    	ResultSet rs;
    	String[] info = new String[6];

    	try {
    		
    		// Get GPs.
    		String sql = sqlFront + id + sqlBack;
    		Statement stmt = connector.createStatement();
    		rs = stmt.executeQuery(sql);
                    
    		System.err.println("SQL: " + sql);
    		System.err.println("\tDB: " + DB);
    		System.err.println("\tHOST: " + HOST);
    		System.err.println("\tUSER: " + USER);
    		System.err.println("\tPASSWORD: " + PASSWORD);
    		while( rs.next() ){
    			info[0] = rs.getString("id");
      			info[1] = rs.getString("symbol");
      			info[2] = rs.getString("dbxref_id");
      			info[3] = rs.getString("species_id");
      			info[4] = rs.getString("type_id");
      			info[5] = rs.getString("full_name");
    			System.err.println("SQL (RESULT 0): " + info[0]);
      			System.err.println("SQL (RESULT 1): " + info[1]);
       			System.err.println("SQL (RESULT 2): " + info[2]);
      			System.err.println("SQL (RESULT 3): " + info[3]);
       			System.err.println("SQL (RESULT 4): " + info[4]);
      			System.err.println("SQL (RESULT 5): " + info[5]);
    		}    		
    		
    	} catch (SQLException e) {
    		System.err.println("ERROR___: " + e.getMessage());
    	}
     	
    	return info;
    }
    
    //
    public String getSniff() { return SNIFF; }
    
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

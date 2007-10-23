package org.bbop.dataadapter;

import java.util.*;
import java.sql.*;

/**
 *
 * An implementation of AdapterConfiguration for adapters that reads
 * one or more files and write a single file
 * 
 */
public class JDBCAdapterConfiguration implements AdapterConfiguration {

    protected int maxReadHistorySize;
    protected int maxWriteHistorySize;
    protected Collection<String> readPaths = new Vector<String>();
    protected String writePath;
	protected Connection conn;

    // example: jdbc:postgresql://foo.com:5432/dbname
    public JDBCAdapterConfiguration(String jdbcPath) {
    	this();
    	readPaths.add(jdbcPath);
    }

    public JDBCAdapterConfiguration() {
    }

    public Collection<String> getReadPaths() {
	return readPaths;
    }

    public void setReadPaths(Collection<String> c) {
	this.readPaths = c;
    }

    public void setWritePath(String path) {
	this.writePath = path;
    }

    public String getWritePath() {
	return writePath;
    }

    public String toString() {
	return "readPaths = "+readPaths;
    }
    
	public Connection getConnection(String driverstring, String user, String passwd) throws SQLException, ClassNotFoundException {
		Class.forName("org.postgresql.Driver");
		return DriverManager.getConnection(driverstring, user, passwd);
	} 
	
	// TODO: DRY
	public void getConnection(String driverstring, String host, String db, String port, String user, String password) {
		if(host==null) host = "genome-venus.ad.uab.edu";
		if(port==null) port = "3508"; //3906
		if(db==null) db = "ozborn_vgd_common_61";
		if(driverstring==null) driverstring = "jdbc:jtds:sqlserver://"+host+":"+port+"/"+db;
		else driverstring = driverstring+host+":"+port+"/"+db;
		Properties prop = new Properties();
		prop.setProperty("user", user);
		prop.setProperty("password", password);
		prop.setProperty("ssl","request");
		try {
			Class.forName("net.sourceforge.jtds.jdbc.Driver");
			conn = DriverManager.getConnection
			(driverstring,prop);

		} catch (SQLException se){ 
			System.out.println("Error code:"+se.getErrorCode());
			System.out.println("Local Message :"+se.getLocalizedMessage());
			System.out.println("Cause :"+se.getCause());
		}
		catch (Exception e) { e.printStackTrace(); }
	} 

}

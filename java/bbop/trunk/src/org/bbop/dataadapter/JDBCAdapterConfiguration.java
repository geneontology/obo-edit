package org.bbop.dataadapter;

import java.util.*;
import java.sql.*;

/**
 *
 * An implementation of AdapterConfiguration for adapters that reads
 * one or more files and write a single file
 * 
 */
import org.apache.log4j.*;

public class JDBCAdapterConfiguration implements AdapterConfiguration {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(JDBCAdapterConfiguration.class);

    protected int maxReadHistorySize;
    protected int maxWriteHistorySize;
    protected String readPath;
    protected String dbUsername;
    protected String dbPassword = "";
    protected String writePath;
	protected Connection conn;

    // example: jdbc:postgresql://foo.com:5432/dbname
    public JDBCAdapterConfiguration(String jdbcPath) {
    	this();
    	this.readPath = jdbcPath;
    }

    public JDBCAdapterConfiguration() {
    }

    public String getReadPath() {
    	return readPath;
    }

    public void setReadPath(String readPath) {
    	this.readPath = readPath;
    }

    public void setWritePath(String path) {
	this.writePath = path;
    }

    public String getWritePath() {
	return writePath;
    }

    public String toString() {
    	return "readPaths = " + readPath;
    }
    
	public Connection getConnection() throws SQLException, ClassNotFoundException {
		Class.forName("org.postgresql.Driver");
		System.out.println("connecting to " + this.getReadPath());
		
		String username = this.getDbUsername();
		
		if (username==null){
			String systemUser = System.getenv("USER");
			if (systemUser==null){
				logger.info("WARN: No username specified for database connection. Attempting to connect without a user.");
				username = "";
			} else {
				logger.info("WARN: No username specified for database connection. Using environmental username " + systemUser);
				username = systemUser;
			}
		}
		
		String password = this.getDbPassword();
		if (password == null){
			logger.info("WARN: No password specified for database connection.");
			password ="";
		}
		
		return DriverManager.getConnection(this.readPath, username, password);
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

	public String getDbUsername() {
		return dbUsername;
	}

	public void setDbUsername(String dbUsername) {
		this.dbUsername = dbUsername;
	}

	public String getDbPassword() {
		return dbPassword;
	}

	public void setDbPassword(String dbPassword) {
		this.dbPassword = dbPassword;
	} 

}

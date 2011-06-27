package org.geneontology.gold.io.postgres;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.IOException;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.List;

import org.postgresql.copy.CopyManager;
import org.postgresql.core.BaseConnection;


/**
 * 
 * @author Shahid Manzoor
 * This class implements the bulk data load methodology of GOLD database.
 * It loads comma separated sql data files into the GOLD database. 
 *
 */
public class TsvFileLoader {

	
	//private BaseConnection connection;
	
	private CopyManager copyManager;

	public TsvFileLoader(String username, String password, String host, String dbName) throws SQLException, ClassNotFoundException{
	
	//public TsvFilelLoader(BaseConnection connection) throws SQLException{
	//	this.connection = connection;
		Class.forName("org.postgresql.Driver");
		
		
		this.copyManager = new CopyManager((BaseConnection) DriverManager.getConnection("jdbc:postgresql://"+host+"/"+dbName, username, password));
	}
	
	
	/**
	 * 
	 * @param The file to be loaded into the table of same name as data of file name.
	 * 			The must exist in the ./data folder.
	 * @throws IOException
	 * @throws SQLException
	 */
	public void loadTable(File file) throws IOException, SQLException{
		
		//File file = new File("data/"+fileName);
		
		if(!file.exists())
			throw new IOException("The data file at '"+ file.getPath() + "' does not exist" );
		
		
		String table = file.getName().substring(0, file.getName().indexOf('.'));
		
		copyManager.copyIn("COPY "+ table + " FROM STDIN WITH DELIMITER AS '\t'", new FileInputStream(file));
	}
	
	
	/**
	 * This method scan the './data' folder to load the the sql CVS files to be
	 * loaded. It also treats file name as table name. The file seperator should be tab.
	 * The null value should be encoded as empty string
	 * @throws IOException
	 * @throws SQLException
	 */
	public void loadTables(String tsvFilesDir) throws IOException, SQLException{
		
		File data = new File(tsvFilesDir);
		
		File[] tables = data.listFiles(new FileFilter() {
			
			public boolean accept(File pathname) {
				return !pathname.isDirectory() && pathname.getName().endsWith(".txt");
			}
		});
		
		for(File table: tables){
			loadTable(table);
		}
	}
	

	public void loadTables(String tsvFilesDir, List<String> list) throws IOException, SQLException{
		File dir = new File(tsvFilesDir);
		for(String table: list){
			loadTable(new File(dir, table + ".txt"));
		}
	}

	/**
	 * 
	 * @param files Calls loadTable(File file) on each element in this list.
	 * @throws IOException
	 * @throws SQLException
	 */
	public void loadTables(List<File> files) throws IOException, SQLException {
		for (File file : files) {
			//System.err.println(file);
			loadTable(file);
		}
	}
	
}

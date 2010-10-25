package org.geneontology.gold.io.postgres;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.sql.SQLException;

import org.postgresql.copy.CopyManager;
import org.postgresql.core.BaseConnection;


/**
 * 
 * @author Shahid Manzoor
 * This class implements the bulk data load methodology of GOLD database.
 * It loads comma separated sql data files into the GOLD database. 
 *
 */
public class CvsFilelLoader {

	
	private BaseConnection connection;
	
	private CopyManager copyManager;
	
	public CvsFilelLoader(BaseConnection connection) throws SQLException{
		this.connection = connection;
		
		this.copyManager = new CopyManager(connection);
	}
	
	
	/**
	 * 
	 * @param The file to be loaded into the table of same name as data of file name.
	 * 			The must exist in the ./data folder.
	 * @throws IOException
	 * @throws SQLException
	 */
	public void loadTable(String fileName) throws IOException, SQLException{
		
		File file = new File("data/"+fileName);
		
		if(!file.exists())
			throw new IOException("The data file at 'data/"+ fileName + "' does not exist" );
		
		
		String table = fileName.substring(0, fileName.indexOf('.'));
		
		copyManager.copyIn("COPY "+ table + " FROM STDIN WITH DELIMITER AS '\t' CSV", new FileInputStream(file));
	}
	
	
	/**
	 * This method scan the './data' folder to load the the sql CVS files to be
	 * loaded. It also treats file name as table name. The file seperator should be tab.
	 * The null value should be encoded as empty string
	 * @throws IOException
	 * @throws SQLException
	 */
	public void loadTables() throws IOException, SQLException{
		
		File data = new File("data");
		
		String[] tables = data.list();
		
		for(String table: tables){
			loadTable(table);
		}
	}
	
}

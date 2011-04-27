package org.geneontology.gold.io;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.apache.log4j.Logger;

/**
 * TO BE IMPLEMENTED
 * 
 * generic class for generating tabular files that can be imported (bulk
 * loaded) directly into a SQL database.
 * 
 * Typically these will go into tables called "bulk_<X>". these can
 * then be used to synchronize with tables <X>.
 *
 * TODO: performance comparison with
 * http://docs.jboss.org/hibernate/core/3.3/reference/en/html/batch.html
 */
public class TableDumper {
	
	private static Logger LOG = Logger.getLogger(TableDumper.class);
	
	private static boolean DEBUG = LOG.isDebugEnabled();
	
	private String table;
	
	private FileWriter writer;
	
	private String nullString;
	
	private final static String defaultNullString = "\\N";
	private final static String suffix = ".txt";
	
	public TableDumper(String table) throws IOException {
		this(table, ".");
	}
	
	public TableDumper(String table, String path) throws IOException{
		this(table, path, defaultNullString);
	}
	
	public TableDumper(String table, String path, String nullString) throws IOException{
		this.table = table;
		this.nullString = nullString;
		writer = new FileWriter(new File(path,this.table + suffix));
	}

	/**
	 * Calls TableDumper(file,nullString) with the defaultNullString.
	 * @param file
	 * @throws IOException
	 * @author Sven Heinicke
	 */
	public TableDumper(File file) throws IOException {
		this(file, defaultNullString);
	}
	
	/**
	 * Constructs on object from a File rather then a String holding a path.  The table variable is filled with everything before the first '.' in file.getName().
	 * @param file
	 * @param nullString
	 * @throws IOException
	 * @author Sven Heinicke
	 */
	public TableDumper(File file, String nullString) throws IOException{
		this.nullString = nullString;

		String fileName = file.getName();
		table = fileName.substring(0, fileName.indexOf('.')); // like TsvFileLoader does it
		
		writer = new FileWriter(file);
	}
	
	public void dumpRow(String v1) throws IOException {
		if(v1 == null)
			v1 = nullString;
		write(v1);
	}
	public void dumpRow(String v1, String v2) throws IOException {
		if(v1 == null)
			v1 = nullString;
		
		if(v2 == null)
			v2 = nullString;
		
		write(v1+"\t"+v2);		
	}
	public void dumpRow(String v1, String v2, String v3) throws IOException {
		if(v1 == null)
			v1 = nullString;
		
		if(v2 == null)
			v2 = nullString;
		
		if(v3 == null)
			v3 = nullString;
		
		write(v1+"\t"+v2+"\t"+v3);					
	}
	
	public void dumpRow(String... args) throws IOException {
		StringBuffer b = null;
		for (String v : args) {
			if(v == null)
				v = nullString;
			
			if (b==null)
				b = new StringBuffer(v);
			else
				b.append("\t"+v);
		}
		write(b.toString());					
	}
	// TODO
	
	private void write(String s) throws IOException {
		if(DEBUG)
			LOG.debug(s);
		
		
		writer.write(s+ "\n");
	}
	
	public void close() throws IOException{
		writer.flush();
		writer.close();
	}
	
	public String getTable(){
		return this.table;
	}

}

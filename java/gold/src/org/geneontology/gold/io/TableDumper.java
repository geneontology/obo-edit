package org.geneontology.gold.io;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

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
	private String table;
	
	private FileWriter writer;
	
	private String nullString;
	
	public TableDumper(String table) throws IOException {
		this(table, ".");
	}
	
	public TableDumper(String table, String path) throws IOException{
		this(table, path, "\\N");
	}
	
	public TableDumper(String table, String path, String nullString) throws IOException{
		this.table = table;
		this.nullString = nullString;
		writer = new FileWriter(new File(path, table+".txt"));
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
		System.out.println(s+"\n");
		writer.write(s+ "\n");
	}
	
	public void close() throws IOException{
		writer.flush();
		writer.close();
	}

}

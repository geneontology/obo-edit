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
	public TableDumper(String table) throws IOException {
		this(table, ".");
	}
	
	public TableDumper(String table, String path) throws IOException{
		this.table = table;
		
		writer = new FileWriter(new File(path, table+".txt"));
	}
	
	public void dumpRow(String v1) throws IOException {
		write(v1);
	}
	public void dumpRow(String v1, String v2) throws IOException {
		write(v1+"\t"+v2);		
	}
	public void dumpRow(String v1, String v2, String v3) throws IOException {
		write(v1+"\t"+v2+"\t"+v3);					
	}
	
	public void dumpRow(String... args) throws IOException {
		StringBuffer b = null;
		for (String v : args) {
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

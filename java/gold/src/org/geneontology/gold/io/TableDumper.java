package org.geneontology.gold.io;

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
	
	
	public TableDumper(String table) {
		super();
		this.table = table;
	}
	
	public void dumpRow(String v1) {
		write(v1);
	}
	public void dumpRow(String v1, String v2) {
		write(v1+"\t"+v2);		
	}
	public void dumpRow(String v1, String v2, String v3) {
		write(v1+"\t"+v2+"\t"+v3);					
	}
	
	public void dumpRow(String... args) {
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
	
	private void write(String s) {
		System.out.println(s+"\n");
	}

}

package org.geneontology.gold.io;

import java.io.IOException;
import java.util.Hashtable;
import java.util.List;
import org.semanticweb.owlapi.model.OWLOntology;
import owltools.graph.OWLGraphWrapper;

/**
 * This class is a base class of the classes which generate Tsv files
 * @author Shahid Manzoor
 *
 */
public abstract class AbstractBulkLoader {

	/**
	 * Ontologies are accessed via GraphWrappers
	 */
	protected OWLGraphWrapper graphWrapper;

	//This variable is used as prefix of each dump file created through this loader
	protected String dumpFilePrefix;
	
	
	/**
	 * the default path where the Tsv file is written
	 */
	protected String path;
	
	protected Hashtable<String, TableDumper> tables;
	
	// TODO - abstract this
	public OWLOntology getOwlOntology() {
		return graphWrapper.getSourceOntology();
	}
	
	protected void flushTables() throws IOException{
		for(TableDumper table: tables.values()){
			table.close();
		}
	}
	
	public AbstractBulkLoader(OWLGraphWrapper wrapper){
		this(wrapper, "./data");
	}
	
	
	public AbstractBulkLoader(OWLGraphWrapper wrapper, String path){
		this(wrapper, path, "");
	}
	
	public AbstractBulkLoader(OWLGraphWrapper wrapper, String path, String dumpFilePrefix){
		this.graphWrapper = wrapper;
		this.path = path;
		this.dumpFilePrefix = dumpFilePrefix == null ? "" : dumpFilePrefix.trim();
		tables = new Hashtable<String, TableDumper>();
	}

	/**
	 * It returns the array of the table names dumped into TSV files
	 * @return
	 * @throws IOException
	 */
	public abstract List<String> dumpBulkLoadTables() throws IOException;	
	
}

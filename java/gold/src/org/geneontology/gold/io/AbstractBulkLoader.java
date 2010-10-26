package org.geneontology.gold.io;

import java.io.IOException;

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

	/**
	 * the default path where the Tsv file is written
	 */
	protected String path;
	
	// TODO - abstract this
	public OWLOntology getOwlOntology() {
		return graphWrapper.getOntology();
	}
	
	
	public AbstractBulkLoader(OWLGraphWrapper wrapper){
		this(wrapper, "./data");
	}
	
	
	public AbstractBulkLoader(OWLGraphWrapper wrapper, String path){
		this.graphWrapper = wrapper;
		this.path = path;
	}
	
	
	public abstract void dumpBulkLoadTables() throws IOException;	
	
}

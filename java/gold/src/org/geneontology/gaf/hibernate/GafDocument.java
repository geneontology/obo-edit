package org.geneontology.gaf.hibernate;

import java.io.Serializable;
import org.apache.log4j.Logger;

/**
 * The class represents the gaf_document table in the database. Please see GafDocument.hbm.xml file in this 
 * for the mapping detail 
 * @author Shahid Manzoor
 *
 */

public class GafDocument extends owltools.gaf.GafDocument implements Serializable {

	private static Logger LOG = Logger.getLogger(GafDocument.class);
	
	
	private boolean hibernateLoad;
	
	void setHibernateLoad(){
		hibernateLoad = true;
	}

	public GafDocument() {
		super();
	}

	public GafDocument(String id, String documentPath) {
		super(id, documentPath);
	}

	
	
}

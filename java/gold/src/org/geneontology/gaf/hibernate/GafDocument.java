package org.geneontology.gaf.hibernate;

import java.io.Serializable;
import org.apache.log4j.Logger;

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

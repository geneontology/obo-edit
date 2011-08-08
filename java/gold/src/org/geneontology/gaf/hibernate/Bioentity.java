package org.geneontology.gaf.hibernate;

import java.io.Serializable;
import org.geneontology.gold.hibernate.model.Cls;

public class Bioentity extends owltools.gaf.Bioentity implements Serializable {

	
	private Cls ncbiTaxonIdObject;
	
	public Bioentity() {
		super();
	}

	public Bioentity(String id, String symbol, String fullName, String typeCls,
			String ncbiTaxonId, String db, String gafDocument) {
		super(id, symbol, fullName, typeCls, ncbiTaxonId, db, gafDocument);
	}

	public Cls getNcbiTaxonIdObject() {
		if(this.ncbiTaxonIdObject == null){
		//	ncbiTaxonIdObject = (Cls) getHibernateObject(Cls.class, "id", getNcbiTaxonId());
		}
		
		
		return ncbiTaxonIdObject;
	}

	
	
	
}

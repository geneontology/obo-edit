package org.geneontology.gaf.hibernate;

import java.io.Serializable;
import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.GOModel;

public class Bioentity extends owltools.gaf.Bioentity implements Serializable {

	
	private Cls ncbiTaxonIdObject;
	private boolean isNcbiTaxonIdObjectLoaded;
	
	public Bioentity() {
		super();
	}
	

	public Bioentity(String id, String symbol, String fullName, String typeCls,
			String ncbiTaxonId, String db, String gafDocument) {
		super(id, symbol, fullName, typeCls, ncbiTaxonId, db, gafDocument);
	}

	public Cls getNcbiTaxonIdObject() {
		if(this.ncbiTaxonIdObject == null && this.ncbiTaxonId != null && !this.isNcbiTaxonIdObjectLoaded){
			isNcbiTaxonIdObjectLoaded = true;
			ncbiTaxonIdObject = (Cls) GOModel.getHibernateObject(Cls.class, "id", getNcbiTaxonId());
		}
		
		return ncbiTaxonIdObject;
	}


	@Override
	public String getId() {
		// TODO Auto-generated method stub
		return super.getId();
	}


	@Override
	public void setId(String id) {
		// TODO Auto-generated method stub
		super.setId(id);
	}

	
	
	
}

package org.geneontology.gaf.hibernate;

import java.io.Serializable;

import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.GOModel;

public class Bioentity extends GOModel implements Serializable {

	private String id;
	private String symbol;
	private String fullName;
	private String typeCls;
	private String ncbiTaxonId;
	private String db;
	private String gafDocument;
	
	private Cls ncbiTaxonIdObject;
	
	 public Bioentity(){
		String[] keys = {"id"};
		this.initUniqueConstraintFields(Bioentity.class, keys);
	}
	
	public Bioentity(String id, String symbol, String fullName, String typeCls,
			String ncbiTaxonId, String db, String gafDocument) {
		this();
		this.id = id;
		this.symbol = symbol;
		this.fullName = fullName;
		this.typeCls = typeCls;
		this.ncbiTaxonId = ncbiTaxonId;
		this.db = db;
		this.gafDocument = gafDocument;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getSymbol() {
		return symbol;
	}

	public void setSymbol(String symbol) {
		this.symbol = symbol;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getTypeCls() {
		return typeCls;
	}

	public void setTypeCls(String typeCls) {
		this.typeCls = typeCls;
	}

	public String getNcbiTaxonId() {
		return ncbiTaxonId;
	}

	public void setNcbiTaxonId(String ncbiTaxonId) {
		this.ncbiTaxonId = ncbiTaxonId;
	}

	public String getDb() {
		return db;
	}

	public void setDb(String db) {
		this.db = db;
	}

	public String getGafDocument() {
		return gafDocument;
	}

	public void setGafDocument(String gafDocument) {
		this.gafDocument = gafDocument;
	}

	public Cls getNcbiTaxonIdObject() {
		if(this.ncbiTaxonIdObject == null){
			ncbiTaxonIdObject = (Cls) getHibernateObject(Cls.class, "id", getNcbiTaxonId());
		}
		
		
		return ncbiTaxonIdObject;
	}

	
	
	
}

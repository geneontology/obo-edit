package org.geneontology.gaf.parser;

import java.util.ArrayList;
import java.util.List;

import org.geneontology.gaf.hibernate.Bioentity;
import org.geneontology.gold.hibernate.model.GOModel;


/**
 * This class builds hibernate objects for GAF db during the 
 * parsing process of a GAF file
 * @author Shahid Manzoor
 *
 */
public class GAFParserHandlerForHibernate implements GAFParserHandler {

	private List<GOModel> list;
	
	public GAFParserHandlerForHibernate(){
		list = new ArrayList<GOModel>();
	}
	
	
	public List<GOModel> getHibernateObjects(){
		return list;
	}
	
	
	public void startDocument() {
		list = new ArrayList<GOModel>();
	}

	public void endDocument() {

	}


	public void handleColumns(String[] cols) {
		addBioEntity(cols);
		
	}
	
	
	private void addBioEntity(String[] cols){
		String id = cols[1];
		String symbol = cols[2];
		String fullName = cols[9];
		String typeCls = cols[11];
		int ncbiTaxonId = Integer.parseInt( cols[12] );
		String db = cols[10];
		
		list.add(new Bioentity(id, symbol, fullName, typeCls, ncbiTaxonId, db));
	}

	private void addGeneAnnotation(String cols[]){
		String bioentity = cols[1];
		String qualifierExpression = cols[3].trim();
	
		
		boolean toContributesTo = false;
		boolean toIntegeralTo = false;
		
		/*
		 TODO: TBD
		if(qualifierExpression.length()>0){
			
		}*/
		
	}
	

}

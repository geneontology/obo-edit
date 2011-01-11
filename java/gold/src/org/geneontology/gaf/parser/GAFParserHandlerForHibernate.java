package org.geneontology.gaf.parser;

import java.util.ArrayList;
import java.util.List;

import org.geneontology.gaf.hibernate.Bioentity;
import org.geneontology.gaf.hibernate.GeneAnnotation;
import org.geneontology.gold.hibernate.model.GOModel;

import sun.nio.cs.ext.ISCII91;


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
		Bioentity entity = addBioEntity(cols);
		GeneAnnotation ga = addGeneAnnotation(cols, entity);
		
	}
	
	
	private Bioentity addBioEntity(String[] cols){
		String id = cols[1];
		String symbol = cols[2];
		String fullName = cols[9];
		String typeCls = cols[11];
		int ncbiTaxonId = Integer.parseInt( cols[12] );
		String db = cols[10];
		
		Bioentity entity = new Bioentity(id, symbol, fullName, typeCls, ncbiTaxonId, db);
		
		list.add(entity);

		return entity;
	}

	private GeneAnnotation addGeneAnnotation(String cols[], Bioentity entity){
		String qualifierExpression = cols[3].trim();
	
		
		boolean isContributesTo = false;
		boolean isIntegeralTo = false;
		
	
		String cls = cols[4];
		String evidenceCls = cols[6];
		String withExpression = cols[7];

		int actsOnTaxonId = Integer.parseInt(cols[12]);
		
		String assignedBy = cols[14];
		String extensionExpression = cols[15];
		String geneProductForm = cols[16];
		String lastUpdateDate = cols[13];
		
		GeneAnnotation ga = new GeneAnnotation(entity, qualifierExpression,
				isContributesTo, isIntegeralTo, 
				cls, null, evidenceCls, withExpression, actsOnTaxonId, 
				lastUpdateDate, assignedBy, extensionExpression, geneProductForm);

		list.add(ga);
		
		return ga;
	}
	

}

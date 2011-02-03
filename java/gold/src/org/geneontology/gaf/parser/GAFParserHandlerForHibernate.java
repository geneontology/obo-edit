package org.geneontology.gaf.parser;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.geneontology.gaf.hibernate.Bioentity;
import org.geneontology.gaf.hibernate.CompositeQualifier;
import org.geneontology.gaf.hibernate.ExtensionExpression;
import org.geneontology.gaf.hibernate.GafDocument;
import org.geneontology.gaf.hibernate.GeneAnnotation;
import org.geneontology.gaf.hibernate.WithInfo;
import org.geneontology.gold.hibernate.model.Cls;
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
	
	private GafDocument gafDocument;
	
	public GAFParserHandlerForHibernate(){
		list = new ArrayList<GOModel>();

		gafDocument = new GafDocument();
	}
	
	
	public List<GOModel> getHibernateObjects(){
		return list;
	}
	
	
	public void startDocument(File gafFile) {
		list = new ArrayList<GOModel>();
		
		gafDocument.setDocumentPath(gafFile.getAbsolutePath());
		gafDocument.setId(gafFile.getName());
		
	}

	public void endDocument() {

	}


	public void handleColumns(String[] cols) {
		Bioentity entity = addBioEntity(cols);
		addGeneAnnotation(cols, entity);
		addWithInfo(cols);
		addCompositeQualifier(cols);
		addExtensionExpression(cols);
	}
	
	
	private Bioentity addBioEntity(String[] cols){
		String id = cols[1];
		String symbol = cols[2];
		String fullName = cols[9];
		String typeCls = cols[11];
		int ncbiTaxonId =-1;
		String taxons[] = cols[12].split("|");
		taxons = taxons[0].split(":");
		ncbiTaxonId = Integer.parseInt(taxons[1]);
		
		String db = cols[10];
		
		
		
		Bioentity entity = new Bioentity(id, symbol, fullName, typeCls, ncbiTaxonId, db);
		
		gafDocument.addBioentity(entity);
		

		return entity;
	}
	
	
	private void addWithInfo(String cols[]){
		if(cols[7].length()>0){
			String tokens[] = cols[7].split("|");
			for(String token: tokens){
				gafDocument.addWithInfo(new WithInfo(cols[7], token));
			}
		}
	}
	
	private void addCompositeQualifier(String cols[]){
		if(cols[3].length()>0){
			String tokens[] = cols[3].split("|");
			for(String token: tokens){
				gafDocument.addCompositeQualifier(new CompositeQualifier(cols[3], token));
			}
		}
	}

	private void addExtensionExpression(String cols[]){
		if(cols.length==17){
			if(cols[16].length()>0){
				String tokens[] = cols[16].split("|");
				for(String token: tokens){
					
					int index = token.indexOf("(");
					
					if(index>0){
						String relation = token.substring(0, index);
						String cls = token.substring(index+1, token.length()-1);
						gafDocument.addExtensionExpression(new ExtensionExpression(cols[16], relation, cls));
					}
					
				}
			}
			
		}
	}
	
	private void addGeneAnnotation(String cols[], Bioentity entity){
		String compositeQualifier = cols[3];
	
		
		boolean isContributesTo = cols[3].contains("contributes_to");
		boolean isIntegeralTo = cols[3].contains("integral_to");
		
		String clsId = cols[4];

		String referenceId = cols[5];
		
		String evidenceCls = cols[6];
		String withExpression = cols[7];

		int actsOnTaxonId =-1;
		
		String taxons[] = cols[12].split("|");
		if(taxons.length>1){
			taxons = taxons[1].split(":");
			actsOnTaxonId = Integer.parseInt(taxons[1]);
		}
		
		String lastUpdateDate = cols[13];
		
		String assignedBy = cols[14];

		String extensionExpression = null;
		String geneProductForm = null;

		if(cols.length==17){
		
			extensionExpression = cols[15];
			geneProductForm = cols[16];
		}
		
		GeneAnnotation ga = new GeneAnnotation();
		ga.setBioentity(entity.getId());
		ga.setIsContributesTo(isContributesTo);
		ga.setIsIntegralTo(isIntegeralTo);
		ga.setCls(clsId);
		ga.setReferenceId(referenceId);
		ga.setEvidenceCls(evidenceCls);
		ga.setWithExpression(withExpression);
		ga.setActsOnTaxonId(actsOnTaxonId);
		ga.setLastUpdateDate(lastUpdateDate);
		ga.setAssignedBy(assignedBy);
		ga.setExtensionExpression(extensionExpression);
		ga.setGeneProductForm(geneProductForm);
		ga.setCompositeQualifier(compositeQualifier);
		ga.setGafDocument(gafDocument.getId());
		
		gafDocument.addGeneAnnotation(ga);
		
	}
	
	
	

}

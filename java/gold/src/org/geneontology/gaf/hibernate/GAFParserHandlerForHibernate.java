package org.geneontology.gaf.hibernate;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gaf.hibernate.GafConstants.EvidenceCode;
import org.geneontology.gaf.hibernate.GafConstants.Qualifier;
import org.geneontology.gaf.parser.GAFParserHandler;
import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.GOModel;
import org.geneontology.gold.rules.AnnotationRuleViolation;
import org.hibernate.exception.ViolatedConstraintNameExtracter;

import sun.nio.cs.ext.ISCII91;


/**
 * This class builds hibernate objects for GAF db during the 
 * parsing process of a GAF file
 * @author Shahid Manzoor
 *
 */
@Deprecated
public class GAFParserHandlerForHibernate implements GAFParserHandler {

	private final static Logger LOG = Logger.getLogger(GAFParserHandlerForHibernate.class);
	
	private GafDocument gafDocument;
	
	private double version;
	
	private List<AnnotationRuleViolation> voilations;
	
	private SimpleDateFormat dtFormat;
	
	private static final HashSet<String> db_abbreviations = buildAbbreviations();
	
	private static HashSet<String> buildAbbreviations(){
		HashSet<String> set = new HashSet<String>();
		
		try{
			
			BufferedReader reader = new BufferedReader(new FileReader(new File(GeneOntologyManager.getInstance().getGoXrfAbbsLocation())));
			
			String line = null;
			while ((line =reader.readLine()) != null) {
	
				if(line.startsWith("!"))
					continue;
					
				String data[] = line.split(":");
				
				if(data.length==2 && "abbreviation".equals(data[0]) ){
					set.add(data[1].trim());
				}
				
			}			
			
		}catch(Exception ex){
			LOG.error("Cann't read Go.xrf_abbs file at the location " + GeneOntologyManager.getInstance().getGoXrfAbbsLocation(),
					ex);
		}
		
		return set;
		
	}
	
	
	public GAFParserHandlerForHibernate(){
		gafDocument = new GafDocument();
		voilations = new ArrayList<AnnotationRuleViolation>();
		dtFormat = new SimpleDateFormat("yyyyMMdd");
	}
	
	public GafDocument getGafDocument(){
		return gafDocument;
	}
	
	public List<AnnotationRuleViolation> getAnnotationRuleViolations(){
		return voilations;
	}
	
	public void startDocument(File gafFile) {
		gafDocument.setDocumentPath(gafFile.getAbsolutePath());
		gafDocument.setId(gafFile.getName());
		voilations = new ArrayList<AnnotationRuleViolation>();
		
	}

	public void setVersion(double version) {
		this.version = version;
	}
	
	
	public void endDocument() {

	}

	private String getRow(String[] cols){
		String row = "";
		for(String col: cols){
			row += col + "\tt";
		}
		
		return row;
		
	}
	
	private void performBasicChecks(String cols[]){

		String row = getRow(cols);
		
		if(version == 2.0){
			if(cols.length != 17){
				voilations.add(new AnnotationRuleViolation(" The row '"+ row + "' does not contain required columns number"));
			}
		}else{
			if(cols.length != 15){
				voilations.add(new AnnotationRuleViolation(" The row '"+ row + "' does not contain required columns number"));
			}
		}

		//cardinality checks
		checkCardinality(cols[0], "Column 1: DB", row,1,1);
		checkCardinality(cols[1], "Column 2: DB Object ID", row,1,1);
		checkCardinality(cols[2], "Column 3: DB Object Symbol", row,1,1);
		checkCardinality(cols[3], "Column 4: Qualifier", row, 0, 3);
		checkCardinality(cols[4], "Column 5: GO ID", row,1,1);
		checkCardinality(cols[5], "Column 6: DB Reference", row,1,3);
		checkCardinality(cols[6], "Column 7: Evidence Code", row,1,3);
		checkCardinality(cols[7], "Column 8: With or From", row,0,3);
		checkCardinality(cols[8], "Column 9: Aspect", row,1,1);
		checkCardinality(cols[9], "Column 10: DB Object Name", row,0,1);
		checkCardinality(cols[10], "Column 11: DB Object Synonym",  row, 0,3);
		checkCardinality(cols[11], "Column 12: DB Object Type", row, 1,1);
		checkCardinality(cols[12], "Column 13: Taxon", row, 1,2);
		checkCardinality(cols[13], "Column 14: Date", row, 1,1);
		checkCardinality(cols[14], "Column 15: DB Object Type", row, 1,1);
		checkCardinality(cols[15], "Column 16: DB Object Type", row, 0,3);
		checkCardinality(cols[16], "Column 17: DB Object Type", row, 0,3);

		//check internal spaces
		
		//check date format
		String dtString = cols[13];
		try{
			dtFormat.parse(dtString);
		}catch(Exception ex){
			voilations.add(new AnnotationRuleViolation("The date in the column 14 is of incorrect format in the row: " + row));
		}
		
		//taxon check
		String[] taxons  = cols[12].split("|");
		checkTaxon(taxons[0], row);
		if(taxons.length>0){
			checkTaxon(taxons[1], row);
		}
		
		
		//check qualifier value
		if(cols[3].length()>0){
			Qualifier qaulifer = Qualifier.valueOf(cols[3]);
			if(qaulifer == null){
				voilations.add(new AnnotationRuleViolation("The qualifier '" + cols[3] + "' in the column 4 is incorrect in the row " + row));
			}
		}
		
		//check evidence code
		EvidenceCode evidenceCode = EvidenceCode.valueOf( cols[6] );
		if(evidenceCode == null){
			voilations.add(new AnnotationRuleViolation("The evidence code '" + cols[6] + "' in the column 7 is incorrect in the row " + row));
		}
		
		//check db abbreviations
		if(!db_abbreviations.contains(cols[0]))
			voilations.add(new AnnotationRuleViolation("The DB '" + cols[0] + "'  referred in the column 1 is incorrect in the row: " + row));
 		
	}
	
	private void checkTaxon(String value, String row){
		if(!value.startsWith("taxon"))
			voilations.add(new AnnotationRuleViolation("The taxon id in the column 13 is of in correct format in the row :" + row));
		
		try{
			Integer.parseInt(value.substring("taxon".length()-1));
		}catch(Exception ex){
			voilations.add(new AnnotationRuleViolation("The taxon id in the column 13 is not an integer value :" + row));
		}
	}
	
	private void checkCardinality(String value, String columnName, String row, int min, int max){

		if(min>0 && value.length() != value.trim().length()){
			voilations.add(new AnnotationRuleViolation("White Spaces are found in the " + columnName+ " column in the row: " + row));
		}

		if(min==0 && value != null && value.length() != value.trim().length()){
			voilations.add(new AnnotationRuleViolation("White Spaces are found in the " + columnName+ " column in the row: " + row));
		}
		
		if(min>=1 && value.length()==0){
			voilations.add(new AnnotationRuleViolation(columnName +" value is not supplied in the row: " + row ));
		}
		
		if(max==1 && value.contains("|")){
			voilations.add(new AnnotationRuleViolation(columnName +" cardinality is found greate than 1 in the row: " + row));
		}
		
		
		if(value != null){
			String tokens[] = value.split("|");
			
			if(max==2 && tokens.length>2){
				voilations.add(new AnnotationRuleViolation(columnName +" cardinality is found greate than 2 in the row: " + row));
			}
			
			if(tokens.length>1){
				for(int i =1;i<tokens.length;i++){
					String token = tokens[i]; 
					checkWhiteSpaces(token, columnName, row);
				}
			}
		}
		
		
	}
	
	private void checkWhiteSpaces(String value, String columnName, String row){

		if(value.length() != value.trim().length()){
			voilations.add(new AnnotationRuleViolation("White Spaces are found in the " + columnName+ " column in the row: " + row));
		}
		
	}
	
	
	public void handleColumns(String[] cols) {
		
		performBasicChecks(cols);
		
		Bioentity entity = addBioEntity(cols);
		addGeneAnnotation(cols, entity);
		addWithInfo(cols);
		addCompositeQualifier(cols);
		addExtensionExpression(cols);
	}
	
	
	private Bioentity addBioEntity(String[] cols){
		String id = cols[0] + ":" + cols[1];
		String symbol = cols[2];
		String fullName = cols[9];
		String typeCls = cols[11];
		int ncbiTaxonId =-1;
		String taxons[] = cols[12].split("\\|");
		taxons = taxons[0].split(":");
		ncbiTaxonId = Integer.parseInt(taxons[1]);
		
		String db = cols[10];
		
		Bioentity entity = new Bioentity(id, symbol, fullName, typeCls, ncbiTaxonId, db, gafDocument.getId());
		
		gafDocument.addBioentity(entity);
		

		return entity;
	}
	
	
	private void addWithInfo(String cols[]){
		if(cols[7].length()>0){
			String tokens[] = cols[7].split("\\|");
			for(String token: tokens){
				gafDocument.addWithInfo(new WithInfo(cols[7], token));
			}
		}
	}
	
	private void addCompositeQualifier(String cols[]){
		if(cols[3].length()>0){
			String tokens[] = cols[3].split("\\|");
			for(String token: tokens){
				gafDocument.addCompositeQualifier(new CompositeQualifier(cols[3], token));
			}
		}
	}

	private void addExtensionExpression(String cols[]){
		if(cols.length==17){
			if(cols[16].length()>0){
				String tokens[] = cols[16].split("\\|");
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
		
		String taxons[] = cols[12].split("\\|");
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

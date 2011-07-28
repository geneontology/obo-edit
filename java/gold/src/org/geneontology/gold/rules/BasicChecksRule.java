package org.geneontology.gold.rules;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.HashSet;
import java.util.Set;
import org.apache.log4j.Logger;
import org.geneontology.conf.GoConfigManager;
import org.geneontology.gaf.hibernate.GeneAnnotation;
import org.geneontology.gaf.parser.GAFParser;

public class BasicChecksRule extends AbstractAnnotationRule {

	private static Logger LOG = Logger.getLogger(BasicChecksRule.class);
	
	private static SimpleDateFormat dtFormat = new SimpleDateFormat("yyyyMMdd");
	
	
	private static final HashSet<String> db_abbreviations = buildAbbreviations();
	
	private static HashSet<String> buildAbbreviations(){
		HashSet<String> set = new HashSet<String>();
		
		try{
			
			InputStream is = null;
			String path = GoConfigManager.getInstance().getGoXrfAbbsLocation();
			
			if(path.startsWith("http://") || path.startsWith("file:/")){
				is = new URL(path).openStream();
			}else
				is = new FileInputStream(new File(path));
			
			BufferedReader reader = new BufferedReader(new InputStreamReader(is));
			
			String line = null;
			while ((line =reader.readLine()) != null) {
	
				if(line.startsWith("!"))
					continue;
					
				String data[] = line.split(":");
				String tag = data[0].trim();
				if(data.length==2 && ("abbreviation".equals(tag) || "synonym".equals(tag) ) ){
					set.add(data[1].trim());
				}
				
			}			
			
		}catch(Exception ex){
			LOG.error("Cann't read Go.xrf_abbs file at the location " + GoConfigManager.getInstance().getGoXrfAbbsLocation(),
					ex);
		}
		
		return set;
		
	}
	
	@Override
	public Set<AnnotationRuleViolation> getRuleViolations(GeneAnnotation a) {

		HashSet<AnnotationRuleViolation> set = new HashSet<AnnotationRuleViolation>();
		
	/*	Bioentity bioentity = a.getBioentityObject();
		
		String dbObjectId = bioentity.getId();
	
		String db = dbObjectId.substring(0, dbObjectId.indexOf(':')-1);

		checkCardinality(dbObjectId, "Column 1: DB", set, a,1,1);
		
		dbObjectId = dbObjectId.substring(db.length()-1);
		
		checkCardinality(dbObjectId, "Column 2: DB Object ID", set, a,1,1);
		
		checkCardinality(bioentity.getSymbol(), "Column 3: DB Object Symbol", set, a,1,1);
		
		checkCardinality(a.getCompositeQualifier(), "Column 4: Qualifier", set, a,0, 3);

		checkCardinality(a.getCls(), "Column 5: GO ID", set, a,1,1);
		
		checkCardinality(a.getEvidenceCls(), "Column 7: Evidence Code", set, a,1,3);
		
		checkCardinality(a.getWithExpression(), "Column 8: With or From", set, a,0,3);

		checkCardinality(bioentity.getFullName(), "Column 10: DB Object Name", set, a,0,1);
		
		checkCardinality(bioentity.getDb(), "Column 11: DB Object Synonym", set, a, 0,3);
		
		checkCardinality(bioentity.getTypeCls(), "Column 12: DB Object Type", set, a, 1,1);
		
		checkCardinality(a.getAssignedBy(), "Column 15: DB Object Type", set, a, 1,1);
		
		checkCardinality(a.getAssignedBy(), "Column 16: DB Object Type", set, a, 0,3);
		
		checkCardinality(a.getAssignedBy(), "Column 17: DB Object Type", set, a, 0,3);
*/
		String row = a.toString();
		
		String cols[] = row.split("\\t", -1);
		//cardinality checks
		checkCardinality(cols[0],0, "Column 1: DB", row,1,1, set,a);
		checkCardinality(cols[1], 1,"Column 2: DB Object ID", row,1,1, set,a);
		checkCardinality(cols[2], 2,"Column 3: DB Object Symbol", row,1,1, set,a);
		checkCardinality(cols[3], 3,"Column 4: Qualifier", row, 0, 3, set,a);
		checkCardinality(cols[4], 4,"Column 5: GO ID", row,1,1, set,a);
		checkCardinality(cols[5], 5,"Column 6: DB Reference", row,1,3, set,a);
		checkCardinality(cols[6], 6,"Column 7: Evidence Code", row,1,3, set,a);
		checkCardinality(cols[7], 7,"Column 8: With or From", row,0,3, set,a);
	//	checkCardinality(cols[8], 8,"Column 9: Aspect", row,1,1, set);
		checkCardinality(cols[9], 9,"Column 10: DB Object Name", row,0,1, set,a);
		checkCardinality(cols[10], 10,"Column 11: DB Object Synonym",  row, 0,3, set,a);
		checkCardinality(cols[11], 11,"Column 12: DB Object Type", row, 1,1, set,a);
		checkCardinality(cols[12], 12,"Column 13: Taxon", row, 1,2, set,a);
		checkCardinality(cols[13], 13,"Column 14: Date", row, 1,1, set,a);
		checkCardinality(cols[14], 14,"Column 15: DB Object Type", row, 1,1, set,a);
		
		if(cols.length>15){
			checkCardinality(cols[15], 15,"Column 16: DB Object Type", row, 0,3, set,a);
			checkCardinality(cols[16], 16,"Column 17: DB Object Type", row, 0,3, set,a);
		}
		
		//check date format
		String dtString = cols[GAFParser.DATE];
		try{
			dtFormat.parse(dtString);
		}catch(Exception ex){
			AnnotationRuleViolation v = new AnnotationRuleViolation("The date in the column 14 is of incorrect format in the row: " , a);
			v.setRuleId(getRuleId());
			set.add(v);
		}
		
		//taxon check
		String[] taxons  = cols[GAFParser.TAXON].split("\\|");
		checkTaxon(taxons[0], row, set,a);
		if(taxons.length>1){
			checkTaxon(taxons[1], row, set,a);
		}
		
		//check db abbreviations
		if(!db_abbreviations.contains(cols[0])){
			AnnotationRuleViolation v= new AnnotationRuleViolation("The DB '" + cols[0] + "'  referred in the column 1 is incorrect in the row: " , a);
			v.setRuleId(getRuleId());
			set.add(v);
		}
		
		
		return set;
	
	}


	
	
	private void checkCardinality(String value,int col, String columnName, String row, int min, int max, HashSet<AnnotationRuleViolation> voilations, GeneAnnotation a){

		//TODO: check white spaces
		/*if(value != null && value.length() != value.trim().length()){
			voilations.add(new AnnotationRuleViolation("White Spaces are found in the " + columnName+ " column in the row: " + row));
		}*/

		/*if(min==0 && value != null && value.length() != value.trim().length()){
			voilations.add(new AnnotationRuleViolation("White Spaces are found in the " + columnName+ " column in the row: " + row));
		}*/
		
		if(min>=1 && value.length()==0){
			AnnotationRuleViolation v = new AnnotationRuleViolation(columnName +" value is not supplied in the row: " ,a );
			v.setRuleId(getRuleId());
			voilations.add(v);
		}
		
		if(max==1 && value.contains("|")){
			AnnotationRuleViolation v = new AnnotationRuleViolation(columnName +" cardinality is found greate than 1 in the row: " ,a);
			v.setRuleId(getRuleId());
			voilations.add(v);
		}
		
		
		if(value != null){
			String tokens[] = value.split("\\|");
			
			if(max==2 && tokens.length>2){
				AnnotationRuleViolation v = new AnnotationRuleViolation(columnName +" cardinality is found greate than 2 in the row: " ,a);
				v.setRuleId(getRuleId());
				voilations.add(v);
			}
			
			if(tokens.length>1){
				for(int i =1;i<tokens.length;i++){
					String token = tokens[i]; 
					checkWhiteSpaces(token, col, columnName, row, voilations,a);
				}
			}
		}
		
		
	}
	
	private void checkWhiteSpaces(String value,int col, String columnName, String row, HashSet<AnnotationRuleViolation> voilations, GeneAnnotation a){

		if(col == GAFParser.DB_OBJECT_NAME || col == GAFParser.DB_OBJECT_SYNONYM || col == GAFParser.DB_OBJECT_SYMBOL)
			return;
		
		if(value.contains(" ")){
			AnnotationRuleViolation v = new AnnotationRuleViolation("White Spaces are found in the " + columnName+ " column in the row: " ,a);
			v.setRuleId(getRuleId());
			voilations.add(v);
		}
		
	}
	
	private void checkTaxon(String value, String row, HashSet<AnnotationRuleViolation> voilations, GeneAnnotation a){
		if(!value.startsWith("taxon")){
			AnnotationRuleViolation v = new AnnotationRuleViolation("The taxon id in the column 13 is of in correct format in the row :" ,a);
			v.setRuleId(getRuleId());
			voilations.add(v);
		}
		
		try{
			String taxon = value.substring("taxon:".length());
			Integer.parseInt(taxon);
		}catch(Exception ex){
			AnnotationRuleViolation v = new AnnotationRuleViolation("The taxon id in the column 13 is not an integer value :" ,a);
			v.setRuleId(getRuleId());
			voilations.add(v);
		}
	}
	
	
	/*
	private void checkWhiteSpaces(String value, String columnName, HashSet<AnnotationRuleViolation> set, GeneAnnotation a){

		if(value.length() != value.trim().length()){
			set.add(new AnnotationRuleViolation("Spaces are not allowed in the " + columnName+ " column", a));
		}
		
	}
	
	
	
	
	
	private void checkCardinality(String value, String columnName, HashSet<AnnotationRuleViolation> set, GeneAnnotation a, int min, int max){

		if(min>0 && value.length() != value.trim().length()){
			set.add(new AnnotationRuleViolation("Spaces are not allowed in the " + columnName+ " column", a));
		}

		if(min==0 && value != null && value.length() != value.trim().length()){
			set.add(new AnnotationRuleViolation("Spaces are not allowed in the " + columnName+ " column", a));
		}
		
		if(min>=1 && value.length()==0){
			set.add(new AnnotationRuleViolation(columnName +" column cannot be empty", a));
		}
		
		if(max==1 && value.contains("|")){
			set.add(new AnnotationRuleViolation(columnName +" colmn cardinality cannt be greater than 1", a));
		}
		
		String tokens[] = value.split("|");
		
		if(tokens.length>1){
			for(String token: tokens){
				checkWhiteSpaces(token, columnName, set, a);
			}
		}
		
		
		
		
	}*/
	
	
}

package org.geneontology.gaf.parser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.GZIPInputStream;
import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.rules.AnnotationRuleViolation;

/**
 * 
 * @author Shahid Manzoor
 * 
 */
public class GAFParser {

	private static final String GAF_COMMENT = "!";
	private static final String GAF_VERSION = GAF_COMMENT + "gaf-version:";
	protected static Logger LOG = Logger.getLogger(GAFParser.class);
	private static boolean DEBUG = LOG.isDebugEnabled();

	public final static int DB = 0;
	public final static int DB_OBJECT_ID = 1;
	public final static int DB_OBJECT_SYMBOL = 2;
	public final static int QUALIFIER = 3;
	public final static int GOID = 4;
	public final static int REFERENCE = 5;
	public final static int EVIDENCE = 6;
	public final static int WITH = 7;
	public final static int ASPECT = 8;
	public final static int DB_OBJECT_NAME = 9;
	public final static int DB_OBJECT_SYNONYM = 10;
	public final static int DB_OBJECT_TYPE = 11;
	public final static int TAXON = 12;
	public final static int DATE = 13;
	public final static int ASSIGNED_BY = 14;
	public final static int ANNOTATION_XP = 15;
	public final static int GENE_PRODUCT_ISOFORM = 16;

	
	private double gafVersion;
	
	private BufferedReader reader;
	
	private String currentRow;
	
	private String currentCols[];
	
	private int expectedNumCols;
	
	private List<String> errors;

	
	private List<AnnotationRuleViolation> voilations;
	
	
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
	
	
	public List<AnnotationRuleViolation> getAnnotationRuleViolations(){
		return this.voilations;
	}
	
	/**
	 * Method declaration
	 * 
	 * @param File
	 *            gaf_file
	 * @throws IOException
	 * 
	 * @see
	 */

	public boolean next() throws IOException{
		if(reader != null){
			currentRow  = reader.readLine();
			if(currentRow == null){
				return false;
			}

			if (this.currentRow.trim().length() == 0) {
				System.err.println("Blank line found.");
			}
			
			if (currentRow.startsWith(GAF_COMMENT)) {
				
				if(gafVersion<1){
				
					if (isFormatDeclaration(currentRow)) {
						gafVersion = parseGafVersion(currentRow);
						if (gafVersion == 2.0) {
							expectedNumCols = 17;
						}
					}
				}
				next();
			}else{
				this.currentCols = this.currentRow.split("\\t", -1);
				if (currentCols.length != expectedNumCols) {
					errors.add("Got invalid number of columns for row (expected "
							+ expectedNumCols
							+ ", got "
							+ currentCols.length
							+ "): " + this.currentRow + "." );
				}/*else{
					performBasicChecks(this.currentCols);
				}*/
		}
			
			return true;
		}
		
		return false;
			
	}
	
	public String getCurrentRow(){
		return this.currentRow;
	}
	
	public GAFParser(){
		init();
	}
	
	private void init(){
		this.gafVersion = 0;
		this.reader = null;
		this.errors = new ArrayList<String>();
		this.expectedNumCols = 15;
		voilations = new ArrayList<AnnotationRuleViolation>();
	}
	

	
	public void parse(Reader reader){
		init();
		if (DEBUG)
			LOG.debug("Parsing Start");
	
		this.reader = new BufferedReader(reader);
	}

	/**
	 * 
	 * @param file is the location of the gaf file. The location
	 *  could be http url, absolute path and uri. The could refer to a gaf file or compressed gaf (gzip fle).
	 * @throws IOException
	 * @throws URISyntaxException
	 */
	public void parse(String file) throws IOException, URISyntaxException{
		if(file == null){
			throw new IOException("File '" + file + "' file not found");
		}
		
		InputStream is = null;
		
		if(file.startsWith("http://")){
			URL url = new URL(file);
			is = url.openStream();
		}else if(file.startsWith("file:/")){
			is = new FileInputStream(new File(new URI(file)));
		}else{
			is = new FileInputStream(file);
		}
		
		if(file.endsWith(".gz")){
			is = new GZIPInputStream(is);
		}
		
		parse(new InputStreamReader(is));
		
	}

	
	public void parse(File gaf_file)
			throws IOException {


		// String message = "Importing GAF data";
		parse(gaf_file.getAbsoluteFile());

	}
	
	private void checkNext(){
		if(this.currentCols == null){
			throw new IllegalStateException("Error occured becuase either there is no further " +
					"record in the file or parse method is not called yet");
		}
	}
	
	public String getDb(){
		checkNext();
		
		return this.currentCols[DB];
	}
	
	public String  getDbObjectId(){
		checkNext();
		
		return this.currentCols[DB_OBJECT_ID];
	}
	
	public String  getDbObjectSymbol(){
		checkNext();
		
		return this.currentCols[DB_OBJECT_SYMBOL];
	}
	
	public String  getQualifier(){
		checkNext();
		
		return this.currentCols[QUALIFIER];
	}

	public String  getGOId(){
		checkNext();
		
		return this.currentCols[GOID];
	}
	public String  getReference(){
		checkNext();
		
		return this.currentCols[REFERENCE];
		
	}
	public String  getEvidence(){
	
		checkNext();
		
		return this.currentCols[EVIDENCE];
		
	}
	public String  getWith(){
		checkNext();
		
		return this.currentCols[WITH];

	}
	public String  getAspect(){
		checkNext();
		
		return this.currentCols[ASPECT];

	}
	public String  getDbObjectName(){
		checkNext();
		
		return this.currentCols[DB_OBJECT_NAME];

	}
	public String  getDbObjectSynonym(){
		checkNext();
		
		return this.currentCols[DB_OBJECT_SYNONYM];

	}
	public String  getDBObjectType(){
		checkNext();
		
		return this.currentCols[DB_OBJECT_TYPE];

	}
	public String  getTaxon(){
		checkNext();
		
		return this.currentCols[TAXON];

	}
	public String  getDate(){
		checkNext();
		
		return this.currentCols[DATE];

	}
	public String  getAssignedBy(){
		checkNext();
		
		return this.currentCols[ASSIGNED_BY];

	}
	public String  getAnnotationExtension(){
		checkNext();

		if(this.currentCols.length>15){
			return this.currentCols[ANNOTATION_XP];
		}
		
		return null;
		
	}
	public String  getGeneProjectFormId(){
		checkNext();

		if(this.currentCols.length>16){
			return this.currentCols[GENE_PRODUCT_ISOFORM];
		}
		
		return null;
	}
	

	public List<String> getErrors(){
		return errors;
	}
	
	/*private boolean isValidPath(String file_name) {
		if (file_name == null)
			return false;

		return new File(file_name).canRead();
	}*/

	private boolean isFormatDeclaration(String line) {
		return line.startsWith(GAF_VERSION);
	}

	private double parseGafVersion(String line) {
		Pattern p = Pattern.compile(GAF_VERSION + "\\s*(\\d+\\.*\\d+)");
		Matcher m = p.matcher(line);
		if (m.matches()) {
			return Double.parseDouble(m.group(1));
		}
		return 0;
	}
	
	
	private void performBasicChecks(String cols[]){

		String row = this.currentRow;
		
		if(this.gafVersion == 2.0){
			if(cols.length != 17){
				voilations.add(new AnnotationRuleViolation(" The row '"+ row + "' does not contain required columns number"));
			}
		}else{
			if(cols.length != 15){
				voilations.add(new AnnotationRuleViolation(" The row '"+ row + "' does not contain required columns number"));
			}
		}
 
		/*//cardinality checks
		checkCardinality(cols[0],0, "Column 1: DB", row,1,1);
		checkCardinality(cols[1], 1,"Column 2: DB Object ID", row,1,1);
		checkCardinality(cols[2], 2,"Column 3: DB Object Symbol", row,1,1);
		checkCardinality(cols[3], 3,"Column 4: Qualifier", row, 0, 3);
		checkCardinality(cols[4], 4,"Column 5: GO ID", row,1,1);
		checkCardinality(cols[5], 5,"Column 6: DB Reference", row,1,3);
		checkCardinality(cols[6], 6,"Column 7: Evidence Code", row,1,3);
		checkCardinality(cols[7], 7,"Column 8: With or From", row,0,3);
		checkCardinality(cols[8], 8,"Column 9: Aspect", row,1,1);
		checkCardinality(cols[9], 9,"Column 10: DB Object Name", row,0,1);
		checkCardinality(cols[10], 10,"Column 11: DB Object Synonym",  row, 0,3);
		checkCardinality(cols[11], 11,"Column 12: DB Object Type", row, 1,1);
		checkCardinality(cols[12], 12,"Column 13: Taxon", row, 1,2);
		checkCardinality(cols[13], 13,"Column 14: Date", row, 1,1);
		checkCardinality(cols[14], 14,"Column 15: DB Object Type", row, 1,1);
		
		if(this.expectedNumCols>15){
			checkCardinality(cols[15], 15,"Column 16: DB Object Type", row, 0,3);
			checkCardinality(cols[16], 16,"Column 17: DB Object Type", row, 0,3);
		}*/

		//check internal spaces
		
		//check date format
		/*String dtString = cols[DATE];
		try{
			dtFormat.parse(dtString);
		}catch(Exception ex){
			voilations.add(new AnnotationRuleViolation("The date in the column 14 is of incorrect format in the row: " + row));
		}
		
		//taxon check
		String[] taxons  = cols[TAXON].split("\\|");
		checkTaxon(taxons[0], row);
		if(taxons.length>1){
			checkTaxon(taxons[1], row);
		}*/
		
		
		//check qualifier value
	/*	if(cols[3].length()>0){
			Qualifier qaulifer = Qualifier.valueOf(cols[3]);
			if(qaulifer == null){
				voilations.add(new AnnotationRuleViolation("The qualifier '" + cols[3] + "' in the column 4 is incorrect in the row " + row));
			}
		}*/
		
		//check evidence code
	/*	EvidenceCode evidenceCode = EvidenceCode.valueOf( cols[6] );
		if(evidenceCode == null){
			voilations.add(new AnnotationRuleViolation("The evidence code '" + cols[6] + "' in the column 7 is incorrect in the row " + row));
		}*/
		
		/*//check db abbreviations
		if(!db_abbreviations.contains(cols[0]))
			voilations.add(new AnnotationRuleViolation("The DB '" + cols[0] + "'  referred in the column 1 is incorrect in the row: " + row));
 		*/
	}
	
	private void checkTaxon(String value, String row){
		if(!value.startsWith("taxon"))
			voilations.add(new AnnotationRuleViolation("The taxon id in the column 13 is of in correct format in the row :" + row));
		
		try{
			String taxon = value.substring("taxon:".length());
			Integer.parseInt(taxon);
		}catch(Exception ex){
			voilations.add(new AnnotationRuleViolation("The taxon id in the column 13 is not an integer value :" + row));
		}
	}
	
	private void checkCardinality(String value,int col, String columnName, String row, int min, int max){

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
			String tokens[] = value.split("\\|");
			
			if(max==2 && tokens.length>2){
				voilations.add(new AnnotationRuleViolation(columnName +" cardinality is found greate than 2 in the row: " + row));
			}
			
			if(tokens.length>1){
				for(int i =1;i<tokens.length;i++){
					String token = tokens[i]; 
					checkWhiteSpaces(token, col, columnName, row);
				}
			}
		}
		
		
	}
	
	private void checkWhiteSpaces(String value,int col, String columnName, String row){

		if(col == DB_OBJECT_NAME || col == DB_OBJECT_SYNONYM || col == DB_OBJECT_SYMBOL)
			return;
		
		if(value.contains(" ")){
			voilations.add(new AnnotationRuleViolation("White Spaces are found in the " + columnName+ " column in the row: " + row));
		}
		
	}
	
	
	
}

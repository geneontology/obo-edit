package org.geneontology.gaf.parser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;

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

	/**
	 * Method declaration
	 * 
	 * @param File
	 *            gaf_file
	 * @throws IOException
	 * 
	 * @see
	 */
	public boolean parse(File gaf_file, GAFParserHandler handler)
			throws IOException {

		StringBuffer errors = new StringBuffer();
		// String message = "Importing GAF data";
		if (DEBUG)
			LOG.debug("Parsing Start");

		handler.startDocument();

		if (null != gaf_file && gaf_file.isFile()) {
			String file_name = gaf_file.getCanonicalPath();
			if (isValidPath(file_name)) {

				double gafVersion = 0;

				int expectedNumCols = 15; // default to GAF version 1.0
				BufferedReader reader = new BufferedReader(new FileReader(
						file_name));
				String line = reader.readLine();
				if ((null != line && line.trim().length() > 0)) {
					while (line != null) {

						if (DEBUG)
							LOG.debug("Processing line " + line);

						String row = line;
						if (row.startsWith(GAF_COMMENT)) {
							if (isFormatDeclaration(row)) {
								gafVersion = parseGafVersion(row);
								if (gafVersion == 2.0) {
									expectedNumCols = 17;
								}
							}
							continue;
						}
						/**
						 * Columns are as follows 1 DB required SGD 2
						 * DB_Object_ID required S000000296 3 DB_Object_Symbol
						 * required PHO3 4 Qualifier optional NOT 5 GO ID
						 * required GO:0003993 6 DB:Reference (|DB:Reference)
						 * required SGD_REF:S000047763|PMID:2676709 7 Evidence
						 * code required IMP 8 With (or) From optional
						 * GO:0000346 9 Aspect required F 10 DB_Object_Name
						 * optional acid phosphatase 11 DB_Object_Synonym
						 * (|Synonym) optional YBR092C 12 DB_Object_Type
						 * required gene 13 taxon(|taxon) required taxon:4932 14
						 * Date required 20010118 15 Assigned_by required SGD 16
						 * Annotation_Extension optional part_of(CL:0000576) 17
						 * Gene_Product_Form_ID optional UniProtKB:P12345-2
						 */
						String[] columns = row.split("\\t", -1);
						if (columns.length != expectedNumCols) {
							errors.append("Got invalid number of columns for row (expected "
									+ expectedNumCols
									+ ", got "
									+ columns.length + "): " + row + "\n");
							continue;
						}

						handler.handleColumns(columns);

						line = reader.readLine();

					} // end for loop going through gaf file contents

				} else {
					errors.append(file_name + " is empty");
				}
			} else {
				errors.append(file_name + " cannot be found");
			}
		} else {
			errors.append(gaf_file + " is not a readable file.");
		}

		boolean success = errors.length() == 0;

		LOG.error(errors);

		return success;
	}

	private boolean isValidPath(String file_name) {
		if (file_name == null)
			return false;

		return new File(file_name).canRead();
	}

	private  boolean isFormatDeclaration(String line) {
		return line.startsWith(GAF_VERSION);
	}

	private  double parseGafVersion(String line) {
		Pattern p = Pattern.compile(GAF_VERSION + "\\s*(\\d+\\.*\\d+)");
		Matcher m = p.matcher(line);
		if (m.matches()) {
			return Double.parseDouble(m.group(1));
		}
		return 0;
	}
}

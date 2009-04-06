package org.oboedit.gui.components.ontologyGeneration;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;

import org.apache.log4j.Logger;
import org.pdfbox.cos.COSDocument;
import org.pdfbox.pdfparser.PDFParser;
import org.pdfbox.pdmodel.PDDocument;
import org.pdfbox.util.PDFTextStripper;

/**
 * A Parser object, which takes a pdf file and extract data as string. 
 * 
 * @author Marcel Hanke
 *
 */
public class PdfToTextExtraction extends DataExtraction{
	
	protected final static Logger logger = Logger.getLogger(PdfToTextExtraction.class);
	
	PDFParser parser;
	PDDocument pdDocument;
	COSDocument cosDocument;
	PDFTextStripper pdfTextStripper;
	String pdfText;
	
	PdfToTextExtraction() {
		pdfText = "";
	}
	
	@Override
	public boolean definitionExtraction(CandidateDefinition definition) {
		if(definition == null || definition.getCachedURL() == null || definition.getCachedURL().isEmpty()
				|| !definition.getCachedURL().get(0).endsWith(".pdf")) {
   			return false;	
   		}
		
		URL url;
		
		try {
			url = new URL(definition.getCachedURL().get(0));
		} catch (MalformedURLException e) {
			return false;
		}
		
		if(!load(url)) {
			return false;
		}
		
		if(!parse()) {
			return false;
		}
		
		definition.setDefinition(searchForDefinition(definition.getDefinition().substring(0, definition.getDefinition().length()-2)));
		super.generateHTMLFormattedDefinition(definition);
		
		return true;
	}
	
	public String fileExtraction(File file) {
		if(!file.isFile() || !file.canRead()) {
			return "";
		}
		
		if(!load(file)) {
			return "";
		}
		
		if(!parse()) {
			return "";
		}
		
		return getText();
	}
	
	private boolean load(File file) {
		
		if(!file.isFile()) {
			logger.debug("File " + file + " does not exist.");
			return false;
		}
		
		if(!file.getAbsolutePath().endsWith(".pdf")) {
			logger.debug("File " + file + " is not a pdf file.");
			return false;
		}
		
		try {
			FileInputStream inStream = new FileInputStream(file);
			parser = new PDFParser(inStream);
			return true;
		} catch (IOException e) {
			logger.debug("Unable to open PDF File.");
			e.printStackTrace();
		}
		return false;
	}
	
	private boolean load(URL url) {
		try {
			URLConnection connection = url.openConnection();
			parser = new PDFParser(connection.getInputStream());
			
		} catch (IOException e) {
			e.printStackTrace();
			return false;
		}
		return true;
	}
	
	private boolean parse() {
		try {
			parser.parse();
			cosDocument = parser.getDocument();
			pdfTextStripper = new PDFTextStripper();

			pdDocument = new PDDocument(cosDocument);
			
			StringWriter sw = new StringWriter();
			pdfTextStripper.writeText(pdDocument, sw);
			
			pdfText = sw.toString();
			pdfText = pdfText.replaceAll("-\n", "");
			pdfText = pdfText.replaceAll("\n", " ");
			pdfText = pdfText.replaceAll("\\.\\ ", "\\.\n");
			sw.close();
			return true;
			
		} catch (IOException e) {
			logger.debug("Error occur during extracting information.");
				try {
					if(pdDocument !=null) {
						pdDocument.close();
					}
					if(cosDocument != null) {
						cosDocument.close();
					}
					return false;
				} catch (IOException e1) {
					e1.printStackTrace();
				}
			e.printStackTrace();
		}
		
		try {
			if(pdDocument !=null) {
				pdDocument.close();
			}
			if(cosDocument != null) {
				cosDocument.close();
			}
			return false;
		} catch (IOException e1) {
			e1.printStackTrace();
		}
		return false;
	}
	
	public String getText() {
		return pdfText;
	}
	
	public String searchForDefinition(String definition) {
		if(pdfText==null || pdfText.length() == 0) {
			return null;
		}
		
		String def = definition.substring(0, definition.length()-2); 
		
		if(pdfText.contains(def)) {
			String fullDef = "";
			int defStart = pdfText.indexOf(def);
			int defOldEnd = defStart +def.length()-1;
			
			fullDef = pdfText.substring(defStart, defOldEnd);
			
			for(int i= defOldEnd ; i<pdfText.length() ; i++) {
				if(pdfText.charAt(i)!='.') {
					fullDef = fullDef + pdfText.charAt(i);
				}
				else {
					
					// remove newLine
					if(fullDef.contains("-\n")) {
						fullDef =  fullDef.replaceAll("-\n", ""); 
					}
					else if(fullDef.contains("\n")) {
						fullDef = fullDef.replaceAll("\n", "");
					}
					return fullDef + pdfText.charAt(i);
				}
			}
			return null;
		}
		return null;
	}

	
}

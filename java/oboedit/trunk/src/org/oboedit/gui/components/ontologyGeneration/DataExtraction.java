package org.oboedit.gui.components.ontologyGeneration;

import java.io.File;

import org.apache.log4j.Logger;

/**
 * Abstract class for different FileTyps, which will be read out.
 *  
 * @author Marcel Hanke
 * @author Goetz Fabian
 *
 */
public abstract class DataExtraction {
	public final static Logger logger = Logger.getLogger(DataExtraction.class);
	
	public abstract boolean definitionExtraction(CandidateDefinition definition);
	public abstract String fileExtraction(File file);
	
	/**
	 * Adds the extended definition to the already existing HTML formatted definition.
	 * 
	 */
	protected void generateHTMLFormattedDefinition(CandidateDefinition definition)
	{
		String htmlFormatted = "";
		
		String originalHTMLDefSubStr = "";
		try {
			// original HTML string without last <html> tag
			String originalHTMLDef = definition.getDefinitionHTMLFormatted().substring(0, definition.getDefinitionHTMLFormatted().length() - 7);
			// find last HTML tag
			originalHTMLDefSubStr = originalHTMLDef.substring(0, originalHTMLDef.lastIndexOf(">") + 1);
		} catch (IndexOutOfBoundsException e) {
			logger.error("HTML formatted Definition could not be generated", e);
			return;
		}
		
		int numberOfCharsRead = 0;
		boolean openedTag = false;
		for (int j = 0; j < originalHTMLDefSubStr.length(); j++) {
			if (!openedTag) {
			if (originalHTMLDefSubStr.charAt(j) == '<') {
				openedTag = true;
			} else {
				numberOfCharsRead++;
			}
			} else {
				if (originalHTMLDefSubStr.charAt(j) == '>') {
					openedTag = false;
				}
			}
		}
		
		htmlFormatted += originalHTMLDefSubStr;
		
		htmlFormatted += definition.getDefinition().substring(numberOfCharsRead);
		
		htmlFormatted += "</html>";

		definition.setDefinitionHTMLFormatted(htmlFormatted);
	}
}

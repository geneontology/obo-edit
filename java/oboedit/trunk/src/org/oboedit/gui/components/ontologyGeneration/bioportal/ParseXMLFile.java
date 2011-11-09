package org.oboedit.gui.components.ontologyGeneration.bioportal;

import java.util.Collection;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.log4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import de.tud.biotec.gopubmedOntologyLookupService.xsd.OBOLookupTerm;

/**
 * DEMO Implementation by Trish Whezel
 * 
 * from https://bmir-gforge.stanford.edu/svn/client_examples
 **/
public class ParseXMLFile {
	private static final Logger logger = Logger.getLogger(ParseXMLFile.class);

	/**
	 * @param uri
	 *            RESTful Search URI for BioPortal web services
	 * @return
	 */

	public static Collection<OBOLookupTerm> parseXMLFile(String uri) {
		try {
			DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = docBuilderFactory.newDocumentBuilder();

			Document doc = docBuilder.parse(uri);

			// Normalize text representation
			doc.getDocumentElement().normalize();

			/**
			 * NOTE: The values for "getElementsByTagName may need to be changed
			 * depending on the web service used
			 */
			NodeList listOfSearchResults = doc.getElementsByTagName("searchBean");
			int totalSearchResults = listOfSearchResults.getLength();

			if (totalSearchResults != 0) {
				for (int s = 0; s < listOfSearchResults.getLength(); s++) {
					Node firstSearchNode = listOfSearchResults.item(s);

					if (firstSearchNode.getNodeType() == Node.ELEMENT_NODE) {
						OBOLookupTerm lookupTerm = new OBOLookupTerm();
						Element firstSearchElement = (Element) firstSearchNode;

						// -------
						// NodeList ontologyVersionId =
						// firstSearchElement.getElementsByTagName("ontologyVersionId");
						// Element ontologyVersionIdElement = (Element)
						// ontologyVersionId.item(0);
						// NodeList textOVIDList =
						// ontologyVersionIdElement.getChildNodes();
						// System.out.print(((Node)textOVIDList.item(0)).getNodeValue().trim()+TAB);

						// -------
						NodeList ontologyIdList = firstSearchElement.getElementsByTagName("ontologyId");
						Element ontologyIdElement = (Element) ontologyIdList.item(0);
						NodeList textOIDList = ontologyIdElement.getChildNodes();
						String bioPortalOntologyId = (((Node) textOIDList.item(0)).getNodeValue().trim());
						// System.out.print(bioPortalOntologyId+TAB);

						// /// SET ID
						lookupTerm.setOboID(bioPortalOntologyId);

						// -------
						// NodeList ontologyDisplayLabelList =
						// firstSearchElement
						// .getElementsByTagName("ontologyDisplayLabel");
						// Element ontologyDisplayLabelElement = (Element)
						// ontologyDisplayLabelList.item(0);
						// NodeList ontologyDLList =
						// ontologyDisplayLabelElement.getChildNodes();
						// System.out.print(((Node)
						// ontologyDLList.item(0)).getNodeValue().trim() + TAB);

						// ----
						// NodeList conceptIdList =
						// firstSearchElement.getElementsByTagName("conceptIdShort");
						// Element conceptIdElement = (Element)
						// conceptIdList.item(0);
						// NodeList conceptIDList =
						// conceptIdElement.getChildNodes();
						// String conceptIdentifier = (((Node)
						// conceptIDList.item(0)).getNodeValue().trim() + TAB);
						// System.out.print(conceptIdentifier);

						// ------
						NodeList preferredNameList = firstSearchElement.getElementsByTagName("preferredName");
						Element preferredNameElement = (Element) preferredNameList.item(0);
						NodeList textPreferredNameList = preferredNameElement.getChildNodes();
						String label = ((Node) textPreferredNameList.item(0)).getNodeValue().trim();
						// System.out.print(label + TAB);

						// /// SET LABEL
						lookupTerm.setLabel(label);

						// -- Print URL to access term in BioPortal
						// System.out.print("http://bioportal.bioontology.org/virtual/"+bioPortalOntologyId+"/"+conceptIdentifier);
					}
					// System.out.println();
				}
			}
		} catch (SAXParseException err) {
			logger.warn("**PARSING ERROR" + ", line " + err.getLineNumber() + ", uri " + err.getSystemId(), err);
		} catch (SAXException e) {
			Exception x = e.getException();
			((x == null) ? e : x).printStackTrace();

		} catch (Throwable t) {
			throw new RuntimeException(t);
		}
		return null;
	}
}

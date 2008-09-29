package org.gmod.gbol.bioObject.conf;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import org.gmod.gbol.simpleObject.CV;
import org.gmod.gbol.simpleObject.CVTerm;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/** Class that stores the mapping between classes and types.
 * 
 * @author elee
 *
 */

public class BioObjectConfiguration {

	private Map<CVTerm, String> termToClass;
	private Map<String, CVTerm> classToDefault;
	private Map<String, List<CVTerm>> classToTerms;

	/** Constructor.
	 * 
	 * @param xmlFileName - Configuration XML file name for mappings
	 */
	public BioObjectConfiguration(String xmlFileName)
	{
		termToClass = new HashMap<CVTerm, String>();
		classToDefault = new HashMap<String, CVTerm>();
		classToTerms = new HashMap<String, List<CVTerm>>();
		init(xmlFileName);
	}
	
	/** Get the name of the class corresponding to the given CVTerm.
	 * 
	 * @param cvterm - CVTerm of interest
	 * @return Name of the class.  Returns <code>null</code> if CVTerm doesn't exist
	 */
	public String getClassForCVTerm(CVTerm cvterm)
	{
		return termToClass.get(cvterm);
	}
	
	/** Get the default CVTerm for a given class name.
	 * 
	 * @param className - Class name to get the default CVTerm for
	 * @return Default CVTerm for a given class.  Returns <code>null</code> if no default is set for class name
	 */
	public CVTerm getDefaultCVTermForClass(String className)
	{
		return classToDefault.get(className);
	}
	
	/** Get the associated cvterms for a given class
	 * 
	 * @param className - Class name to get the associated cvterms for
	 * @return Collection of CVTerm objects associated with a given class.  Returns <code>null</code> if none is found
	 */
	public Collection<CVTerm> getCVTermsForClass(String className)
	{
		return classToTerms.get(className);
	}

	private void init(String xmlFileName)
	{
		try {
			SchemaFactory sf = SchemaFactory.newInstance("http://www.w3.org/2001/XMLSchema");
			URL xsd = getClass().getResource("/conf/gbol_mappings.xsd");
			Schema schema = sf.newSchema(xsd);
			Validator validator = schema.newValidator();
			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			dbf.setNamespaceAware(true);
			dbf.setValidating(false);
			DocumentBuilder db = dbf.newDocumentBuilder();
			db.setErrorHandler(new BioObjectConfigurationErrorHandler());
			Document doc = db.parse(new File(xmlFileName));
			DOMSource source = new DOMSource(doc);
			validator.validate(source);
			processMappings(doc, "feature_mappings");
			processMappings(doc, "relationship_mappings");
		}
		catch (ParserConfigurationException e) {
			throw new BioObjectConfigurationException("Error parsing configuration: " + e.getMessage());
		}
		catch (SAXException e) {
			throw new BioObjectConfigurationException("Error parsing configuration: " + e.getMessage());
		}
		catch (IOException e) {
			throw new BioObjectConfigurationException("Error reading configuration: " + e.getMessage());
		}
	}
	
	private void processMappings(Document doc, String root)
	{
		NodeList featureMappings = doc.getElementsByTagName(root);
		NodeList types = ((Element)featureMappings.item(0)).getElementsByTagName("type");
		for (int i = 0; i < types.getLength(); ++i) {
			Element type = (Element)types.item(i);
			String cv = type.getAttribute("cv");
			String term = type.getAttribute("term");
			boolean isDefault = type.getAttribute("default").equals("true");
			String readClass = type.getElementsByTagName("read_class").item(0).getTextContent();
			CVTerm cvterm = new CVTerm(term, new CV(cv));
			termToClass.put(cvterm, readClass);
			if (isDefault) {
				classToDefault.put(readClass, cvterm);
			}
			List<CVTerm> cvterms = classToTerms.get(readClass);
			if (cvterms == null) {
				cvterms = new ArrayList<CVTerm>();
				classToTerms.put(readClass, cvterms);
			}
			cvterms.add(cvterm);
		}
	}
	
	private class BioObjectConfigurationErrorHandler implements ErrorHandler
	{
		
		public void error(SAXParseException e)
		{
			throw new BioObjectConfigurationException("Error in configuration XML: " + e.getMessage());
		}

		public void fatalError(SAXParseException e)
		{
			throw new BioObjectConfigurationException("Error in configuration XML: " + e.getMessage());
		}
		
		public void warning(SAXParseException e)
		{
		}
	}
}

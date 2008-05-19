package org.bbop.util;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Iterator;
import java.util.Map;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.bbop.io.IOUtil;

/**
 * Utilies for working with XML Code
 * 
 * @author jrichter
 * 
 */
import org.apache.log4j.*;

public class XMLUtil {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(XMLUtil.class);

	private XMLUtil() {
	}

	protected static final String INDENT_TRANSFORM = "<xsl:stylesheet  version=\"1.0\" \n"
			+ "                xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\">\n"
			+ "   <xsl:output method=\"xml\"/>\n"
			+ "   <xsl:param name=\"indent-increment\" select=\"\'   \'\" />\n"
			+

			"   <xsl:template match=\"*\">\n"
			+ "      <xsl:param name=\"indent\" select=\"\'&#xA;\'\"/>\n"
			+

			"      <xsl:value-of select=\"$indent\"/>\n"
			+ "      <xsl:copy>\n"
			+ "        <xsl:copy-of select=\"@*\" />\n"
			+ "        <xsl:apply-templates>\n"
			+ "          <xsl:with-param name=\"indent\"\n"
			+ "               select=\"concat($indent, $indent-increment)\"/>\n"
			+ "        </xsl:apply-templates>\n"
			+ "        <xsl:if test=\"*\">\n"
			+ "          <xsl:value-of select=\"$indent\"/>\n"
			+ "        </xsl:if>\n"
			+ "      </xsl:copy>\n"
			+ "   </xsl:template>\n"
			+

			"   <xsl:template match=\"comment()|processing-instruction()\">\n"
			+ "      <xsl:copy />\n"
			+ "   </xsl:template>\n"
			+

			"   <!-- WARNING: this is dangerous. Handle with care -->\n"
			+ "   <xsl:template match=\"text()[normalize-space(.)=\'\']\"/>\n" +

			"</xsl:stylesheet>";

	/**
	 * Transforms a string representation of an XML document using a string
	 * representation of an XSLT stylesheet.
	 * 
	 * @param xslt
	 * @param xml
	 * @return transformed xml string
	 * @throws TransformerException
	 */
	public static String transformToString(String xslt, String xml)
			throws TransformerException {
		Source xmlSource = new StreamSource(new StringReader(xslt));
		Source xsltSource = new StreamSource(new StringReader(xml));
		StringWriter writer = new StringWriter();
		Result result = new StreamResult(writer);
		transform(xsltSource, xmlSource, result, null, null);
		return writer.getBuffer().toString().trim();
	}

	/**
	 * Performs an XSL transform, replacing the original xml file with the
	 * result
	 * 
	 * @param xslt
	 * @param xml
	 * @throws TransformerException
	 * @throws IOException
	 */
	public static void transform(File xslt, File xml)
			throws TransformerException, IOException {
		File out = File.createTempFile("transform_result", "xml");
		out.deleteOnExit();
		transform(xslt, xml, out);
		IOUtil.copyFile(out, xml);
		out.delete();
	}

	/**
	 * Transforms an XML file using a string representation of an XSL transform.
	 * The original XML file is replaced by the transformed version.
	 * 
	 * @param xslt
	 * @param xml
	 * @param params
	 * @throws IOException
	 * @throws TransformerException
	 */
	public static void transform(String xslt, File xml, Map params) throws IOException,
			TransformerException {
		File out = File.createTempFile("transform_result", "xml");
//		logger.info("out = "+out);
		// out.deleteOnExit();
		Source xsltSource = new StreamSource(new StringReader(xslt));
		Source xmlSource = new StreamSource(new FileReader(xml));
		Result result = new StreamResult(new FileWriter(out));
		transform(xsltSource, xmlSource, result, null, params);
		IOUtil.copyFile(out, xml);
		// out.delete();
	}

	/**
	 * Performs an XSL transform on a file, producing an output file
	 * 
	 * @param xslt
	 *            The stylesheet file
	 * @param xml
	 *            The file to transform
	 * @param out
	 *            The output file
	 */
	public static void transform(File xslt, File xml, File out)
			throws TransformerException, IOException {
		Source xmlSource = new StreamSource(new FileReader(xml));
		Source xsltSource = new StreamSource(new FileReader(xslt));
		Result result = new StreamResult(new FileWriter(out));
		transform(xsltSource, xmlSource, result, null, null);
	}

	/**
	 * Does an XSL transform using the default transformer
	 * 
	 * @param xsltSource
	 * @param xmlSource
	 * @param result
	 * @throws TransformerException
	 */
	public static void transform(Source xsltSource, Source xmlSource,
			Result result, Map outputProperties, Map parameters)
			throws TransformerException {
		// the factory pattern supports different XSLT processors
		TransformerFactory transFact = TransformerFactory.newInstance();
		Transformer trans = transFact.newTransformer(xsltSource);
		if (outputProperties != null) {
			Iterator it = outputProperties.keySet().iterator();
			while (it.hasNext()) {
				String name = (String) it.next();
				String value = (String) outputProperties.get(name);
				trans.setOutputProperty(name, value);
			}
		}
		if (parameters != null) {
			Iterator it = parameters.keySet().iterator();
			while (it.hasNext()) {
				String name = (String) it.next();
				Object value = parameters.get(name);
				trans.setParameter(name, value);
			}	
		}
		trans.transform(xmlSource, result);
	}

	public static String indentXML(String xml) {

		// JAXP reads data using the Source interface
		Source xmlSource = new StreamSource(new StringReader(xml));
		Source xsltSource = new StreamSource(new StringReader(INDENT_TRANSFORM));
		StringWriter writer = new StringWriter();
		try {
			// the factory pattern supports different XSLT processors
			TransformerFactory transFact = TransformerFactory.newInstance();
			Transformer trans = transFact.newTransformer(xsltSource);
			trans.setOutputProperty(OutputKeys.INDENT, "yes");
			trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, (xml
					.trim().startsWith("<?xml") ? "no" : "yes"));
			trans.transform(xmlSource, new StreamResult(writer));
		} catch (Exception ex) {
			return xml;
		}
		return writer.toString().trim();
	}
}

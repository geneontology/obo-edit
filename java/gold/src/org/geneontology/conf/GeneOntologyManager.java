package org.geneontology.conf;

import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.List;

import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.PropertiesConfiguration;
import org.apache.log4j.PropertyConfigurator;
import org.geneontology.gold.io.DatabaseDialect;
import org.obolibrary.obo2owl.Obo2Owl;
import org.obolibrary.oboformat.model.OBODoc;
import org.obolibrary.oboformat.parser.OBOFormatParser;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;

import owltools.graph.OWLGraphWrapper;

//import sun.tools.tree.ThisExpression; // was causing problems for me--apparently unused(?) -SJC

/**
 * 
 * @author Shahid Manzoor This class is singleton class. It can only have one
 *         instance. Application level share resources of the Gene Ontology
 *         software pipe lines are managed through this class. This class reads
 *         properties files at conf/*.properties location. The properties
 *         contains systems's configuration information, e.g. data connection
 *         information. More information properties files can be found at
 *         {@link http
 *         ://commons.apache.org/configuration/userguide/howto_properties
 *         .html#Properties_files}
 * 
 */
public class GeneOntologyManager {

	/**
	 * Singleton instance
	 */
	private static GeneOntologyManager instance;

	/**
	 * properties in properties files are loaded into this object.
	 */
	private PropertiesConfiguration config;

	/**
	 * Instance cannot be created through the outside of this class scope
	 * 
	 * @throws ConfigurationException
	 */

	private GeneOntologyManager() throws ConfigurationException {
		init();
	}

	public String guessAbsolutePath(String relativePath) {

		if (relativePath == null)
			return null;

		File file = new File(relativePath);

		if (file.exists()) {
			return file.getAbsolutePath();
		}

		URL url = this.getClass().getClassLoader().getResource(relativePath);

		try {
			return new File(url.toURI()).getAbsolutePath();
		} catch (Exception ex) {
			// ignore the exception
		}

		return null;

	}

	private void init() throws ConfigurationException {

		config = new PropertiesConfiguration();

		File confDir = new File(guessAbsolutePath("conf/gold.properties"));

		File[] files = confDir.getParentFile().listFiles(new FileFilter() {

			public boolean accept(File pathname) {
				// TODO Auto-generated method stub
				return pathname.getName().endsWith(".properties");
			}
		});

		if (files == null) {
			throw new RuntimeException("The file at "
					+ confDir.getAbsolutePath() + " does not exist");
		}

		for (File f : files) {
			PropertiesConfiguration pc = new PropertiesConfiguration(f);

			config.append(pc);
		}
		
		try{
			PropertyConfigurator.configure("conf/log4j.properties");
		}catch(Exception ex){
			//ignore this
			ex.printStackTrace();
		}
	}

	public static GeneOntologyManager getInstance()
			{
		try{
			if (instance == null) {
				instance = new GeneOntologyManager();
			}
		}catch(Exception ex){
			throw new RuntimeException("Can't create instance of OntologyManager", ex);
		}

		return instance;
	}

	/**
	 * 
	 * @return It returns the vlaue of the geneontology.gold.username property
	 */
	public String getGolddbUserName() {
		return config.getString("geneontology.gold.username");
	}

	/**
	 * 
	 * @return It returns the value of the geneontology.gold.password property
	 */
	public String getGolddbUserPassword() {
		return config.getString("geneontology.gold.password");
	}

	/**
	 * 
	 * @return IP address or domain name of the host. It returns the value of
	 *         the geneontology.gold.host property.
	 */
	public String getGolddbHostName() {
		return config.getString("geneontology.gold.host");
	}

	/**
	 * 
	 * @return It returns the value of the geneontology.gold.db property
	 */
	public String getGolddbName() {
		return config.getString("geneontology.gold.db");
	}

	/**
	 * 
	 * @return It returns the value of the geneontology.gold.ontologylocation property
	 */
	public List getDefaultOntologyLocations() {
		return config.getList("geneontology.gold.ontologylocation");
	}

	/**
	 * 
	 * @return It returns the value of the geneontology.gold.tsvfiles value
	 */
	public String getTsvFilesDir() {
		return guessAbsolutePath(
				config.getString("geneontology.gold.tsvfiles"));
	}

	/**
	 * 
	 * @return It returns the value of the geneontology.gold.schemalocation
	 *         property
	 */
	public String getOntSqlSchemaFileLocation() {

		return guessAbsolutePath(config
				.getString("geneontology.gold.schemalocation"));

	}

	public String getGafSqlSchemaFileLocation() {

		return guessAbsolutePath(config
				.getString("geneontology.gold.gaf.schemalocation"));

	}

	
	/*
	 * @return Return the Integer representing the desired to port for Jetty to run on. If finding config fails, return 8080.
	 */
	public Integer getJettyPort() {
		int port = -1;
		try{
			port = config.getInt("geneontology.gold.jetty.port");	
		}catch(Exception ex){
			// TODO: debug or log here to indicate fallthrough
			port = 8080;
		}
	
		return port;
	}
	
	
	/**
	 * 
	 * @return It returns the value of the geneontology.gold.deltatableprefix
	 *         property
	 */
	public String getGoldDetlaTablePrefix() {

		return config
				.getString("geneontology.gold.deltatableprefix");

	}
	
	/**
	 * It builds the instance of the class referenced in the geneontology.gold.dialect property
	 * @return
	 * @throws Exception
	 */
	public DatabaseDialect buildDatabaseDialect() throws Exception{
		String clsName = config.getString("geneontology.gold.dialect");
		
		DatabaseDialect db = null;
		if(clsName != null && clsName.trim().length()>0){
			//db = (DatabaseDialect) this.getClass().getClassLoader().loadClass(clsName).newInstance();
			db = (DatabaseDialect)Class.forName("org.geneontology.gold.io.postgres.PostgresDialect").newInstance();
		}
		
		return db;
	}
	
	public String getProperty(String prop){
		return config.getString(prop);
				
	}
	
	public void setProperty(String prop, String value){
		config.setProperty(prop, value);
	}
	
	public List getDefaultGafFileLocations(){
		return config.getList("geneontology.gold.gaflocation");	
	}
	
	
	
}

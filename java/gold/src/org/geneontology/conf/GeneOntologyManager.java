package org.geneontology.conf;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;

import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.PropertiesConfiguration;

import sun.tools.tree.ThisExpression;

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

	private String guessAbsolutePath(String relativePath) {

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

			@Override
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
	}

	public static GeneOntologyManager getInstance()
			throws ConfigurationException {
		if (instance == null) {
			instance = new GeneOntologyManager();
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
	 * @return It returns the value of the geneontology.gold.obofile property
	 */
	public String getDefaultOboFile() {
		return guessAbsolutePath(
				config.getString("geneontology.gold.obofile"));
	}

	/**
	 * 
	 * @return It returns the vlaue of the geneontology.gold.tsvfiles value
	 */
	public String getTsvFilesDir() {
		return guessAbsolutePath(
				config.getString("geneontology.gold.tsvfiles"));
	}

	/**
	 * 
	 * @return It returns the vlaue of the geneontology.gold.schemalocation
	 *         property
	 */
	public String getOntSqlSchemaFileLocation() {

		return guessAbsolutePath(config
				.getString("geneontology.gold.schemalocation"));

	}

}

package org.geneontology.db.test;

import geneontology.db.util.HibernateUtil;

import org.apache.log4j.PropertyConfigurator;
import org.hibernate.SessionFactory;


/**
 * The Test class is used for testing ChadoAPI code.
 */
public class Test {
	
	/**
	 * Main function for testing code.
	 * @param args arguments for testing code.
	 */
	public static void main(String[] args){
		
		// Change this to the location of your log4j.properties file. You can turn on hibernate debugging for more information.
		PropertyConfigurator.configure( "/Users/suzi/workspace/CHAPI/src/log4j.properties");		
		
		SessionFactory sf = null;
		try {
			// This should be your configuration file. 
			sf = HibernateUtil.buildSessionFactory("hibernate.cfg.xml");
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
}
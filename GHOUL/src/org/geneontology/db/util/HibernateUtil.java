package org.geneontology.db.util;

import java.io.File;

import org.hibernate.HibernateException;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.Configuration;

/**
 * HibernateUtil is a class that creates a new Hibernate {@link SessionFactory}
 * based on the users configuration file.
 * 
 * @see SessionFactory
 * @see HibernateException
 * @see Configuration
 * @author Robert Bruggner
 * 
 */
public class HibernateUtil {
	

	/**
	 * Returns a new Hibernate {@link SessionFactory} given a hibernate configuration file.
	 * 
	 * @param config
	 *            Hibernate configuration
	 * @return a {@link SessionFactory}
	 * @throws Exception
	 *             when configuration of the Hibernate {@link SessionFactory} fails.  
	 */
	public static SessionFactory buildSessionFactory(File config) throws Exception {
    	try {
    		Configuration c = new Configuration().configure(config);
    		//Properties p = c.getProperties();
    		//p.list(System.out);
    		SessionFactory sessionFactory = c.buildSessionFactory();
    		return sessionFactory;
    	} catch (HibernateException he){
    		System.out.println("Couldn't build session!");
    		System.out.println(he.getMessage());
    		System.err.println("Couldn't build session. " + he.getMessage());
    		he.printStackTrace();
    		throw he;
    	}
	}
	
	/**
	 * Returns a new Hibernate {@link SessionFactory} given a hibernate configuration file.
	 * 
	 * @param filename
	 *            Hibernate configuration filename
	 * @return a {@link SessionFactory}
	 * @throws Exception
	 *             when configuration of the Hibernate {@link SessionFactory} fails.  
	 */
    public static SessionFactory buildSessionFactory(String filename) throws Exception{
    	return buildSessionFactory(new File(filename));
    }
    
    
}
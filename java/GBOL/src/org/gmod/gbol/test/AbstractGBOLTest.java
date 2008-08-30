package org.gmod.gbol.test;

import org.hibernate.SessionFactory;
import org.apache.log4j.PropertyConfigurator;
import org.gmod.gbol.util.HibernateUtil;
import junit.framework.TestCase;

public class AbstractGBOLTest extends TestCase {

	SessionFactory sf;
	private final String log4jPropFile = "dbConnectConfigurations/log4j.properties";
	
	public AbstractGBOLTest(String name) {
		super(name);
		PropertyConfigurator.configure(this.log4jPropFile);
	}
	
	public void configureConnection(String filename) throws Exception{
		try {
			this.sf = HibernateUtil.buildSessionFactory(filename);
			
		} catch (Exception e) {
			System.err.println("Unable configure session factory for GBOL Test: " + e.getMessage());
			e.printStackTrace();
			throw (e);
		}
	}

	protected void setUp() throws Exception {
		this.sf.getCurrentSession().getTransaction().begin();
		super.setUp();
	}

	protected void tearDown() throws Exception {
		this.sf.getCurrentSession().getTransaction().rollback();
		this.sf.getCurrentSession().close();
		super.tearDown();
	}

}

package org.geneontology.db.test;

import org.geneontology.db.factory.GOobjectFactory;
import org.geneontology.db.model.Term;

import junit.framework.Assert;

public class GOHibernateAPITest extends AbstractGOHibernateAPITest{
	
	// These probably will need to be adjusted as the database changes. 
	private final String test_name = "membrane-associated guanylate kinase";
	
	public GOHibernateAPITest () {
		super();
	}
	 
	public void testTermQuery(){
		this.getSessionFactory().getCurrentSession().beginTransaction();
		GOobjectFactory sof = new GOobjectFactory(this.getSessionFactory());
		
		Term a_term = sof.getTermByName(test_name);
		Assert.assertTrue((a_term != null));
		Assert.assertTrue(a_term.getName().equals(this.test_name));
	}
	
}
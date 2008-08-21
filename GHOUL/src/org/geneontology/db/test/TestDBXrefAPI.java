package org.geneontology.db.test;

import org.geneontology.db.model.DBXref;

public class TestDBXrefAPI extends AbstractGOHibernateAPITest{
	
	public TestDBXrefAPI () {
		super();
	}
	 
	public void testDBXrefQuery(){
		this.getSessionFactory().getCurrentSession().beginTransaction();
		DBXref xref = (DBXref) this.getSessionFactory().getCurrentSession().get(DBXref.class, 93);
		prettyPrint (xref, "XREF");
	}
	
}
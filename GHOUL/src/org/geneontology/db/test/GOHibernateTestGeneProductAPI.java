package org.geneontology.db.test;

import org.geneontology.db.factory.GOobjectFactory;
import org.geneontology.db.model.GeneProduct;
import org.geneontology.db.model.ProductSeq;
import org.geneontology.db.model.Sequence;

public class GOHibernateTestGeneProductAPI extends AbstractGOHibernateAPITest{
	
	// These probably will need to be adjusted as the database changes. 
	private final int test_val = 238988;

	public GOHibernateTestGeneProductAPI () {
		super();
	}
	 
	public void testGeneProductQuery(){
		/*
		GOobjectFactory goFactory = initSessionFactory();
		
		GeneProduct gp = goFactory.getGPByDBXref(test_val);
		prettyPrint (gp);
		*/
		this.getSessionFactory().getCurrentSession().beginTransaction();
		GeneProduct gp = (GeneProduct) this.getSessionFactory().getCurrentSession().get(GeneProduct.class, 141899);
		prettyPrint (gp);
	}
	
}
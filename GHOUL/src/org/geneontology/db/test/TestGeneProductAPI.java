package org.geneontology.db.test;

import org.geneontology.db.factory.GOobjectFactory;
import org.geneontology.db.model.GeneProduct;
import org.geneontology.db.model.ProductSeq;
import org.geneontology.db.model.Sequence;

public class TestGeneProductAPI extends AbstractGOHibernateAPITest{
	
	// These probably will need to be adjusted as the database changes. 
	private final int test_val = 238988;

	public TestGeneProductAPI () {
		super();
	}
	 
	public void testGeneProductQuery(){
		this.getSessionFactory().getCurrentSession().beginTransaction();
		GeneProduct gp = (GeneProduct) this.getSessionFactory().getCurrentSession().get(GeneProduct.class, 250070);
		prettyPrint (gp);
	}
	
}
package org.geneontology.db.test;

import org.geneontology.db.factory.GOobjectFactory;
import org.geneontology.db.model.GOModel;
import org.geneontology.db.model.GeneProduct;

public class GOHibernateTestGeneProductAPI extends AbstractGOHibernateAPITest{
	
	// These probably will need to be adjusted as the database changes. 
	private final int test_val = 218195;

	public GOHibernateTestGeneProductAPI () {
		super();
	}
	 
	public void testGeneProductQuery(){
		GOobjectFactory goFactory = initSessionFactory();
		
		GeneProduct gp = goFactory.getGPByDBXref(test_val);
		prettyPrint (gp);
	}
	
	public void prettyPrint(GOModel model) {
		GeneProduct gp = (GeneProduct) model;
		System.out.println("gp:" + gp.getSymbol() + " (" + gp.getFull_name() + ")\t" + 
				gp.getDbxref().getDb().getFullname() + ":" + gp.getDbxref().getAccession() + "\t" +
				gp.getSpecies().getGenus() + " " + gp.getSpecies().getSpecies() + "\t" +
				gp.getSO_type().getName());
	}
	
}
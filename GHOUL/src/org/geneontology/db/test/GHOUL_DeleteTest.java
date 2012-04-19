package org.geneontology.db.test;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.apache.log4j.Logger;
import org.geneontology.db.factory.GOobjectFactory;
import org.geneontology.db.model.Association;
import org.geneontology.db.model.DB;
import org.geneontology.db.model.DBXref;
import org.geneontology.db.model.Evidence;
import org.geneontology.db.model.GeneProduct;
import org.geneontology.db.model.Term;
import org.hibernate.Session;

public class GHOUL_DeleteTest extends TestCase{

	private GOobjectFactory factory;

	public final static String REFGENOME = "RefGenome";
	public final static String DESCENDANT_SEQUENCES_EC = "IBD"; // was IDS
	public static final String ANCESTRAL_EVIDENCE_CODE = "IBA"; // was IAS
	public final static String PAINT_DB = "PANTHER";

	/**
	 * The cross reference to the publication/evidence for the annotation 
	 * of this family 
	 */
	private static DBXref paint_dbxref;

	protected final static Logger logger = Logger.getLogger(GHOUL_DeleteTest.class);

	public GHOUL_DeleteTest(){
		try {
			factory = new GOobjectFactory("hibernate.cfg.xml");
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public Session startSession() {
		Session session = factory.startSession();
		return session;
	}

	public void endSession(Session session) {
		if (session != null) {
			session.close();
		}
	}

	public void testDeleteAssoc() {
		Session session = startSession();
		GeneProduct gene_product = (GeneProduct) factory.getGPByDBXref("RGD", "1308764", session);
//		GP gene_product = new GP();
		Assert.assertTrue(gene_product != null);		

		Term term = (Term) factory.getTermByAcc("GO:0000011", session); // vacuole inheritance
		Assert.assertTrue(term != null);

		endSession(session);
		
		if (term != null && gene_product != null) {
			Association new_assoc = createAssociation(term, new Integer(9042012), false);				
			gene_product.addAssociation(new_assoc);
			Association removed = gene_product.removeAssociation(term, REFGENOME);
//			Association removed = gene_product.removeAssociation(new_assoc);
			Assert.assertTrue(removed != null);
		}
	}


	private Association createAssociation(Term term, Integer date, boolean MRC) {
		Association assoc = new Association();
		/* 
		 * The association links a term to a gene product
		 * and is made on a specific date
		 */
		assoc.setTerm(term);

		if (date == null)
			assoc.setDate();
		else
			assoc.setDate(date);

		/*
		 * This is the group who makes this association
		 * Right now the only field being populated in the DB class is 
		 * the name "PANTHER", but the other fields (full name, description, 
		 * & URI information) is left blank
		 */
		DB db = new DB();
		db.setName(REFGENOME);
		assoc.setSource_db(db);
		String code = ANCESTRAL_EVIDENCE_CODE;
		assoc.addEvidence(createEvidence(term, code));
		assoc.setDirectMRC(MRC);
		assoc.setDirectNot(false);
		return assoc;
	}

	private Evidence createEvidence(Term term, String code) {
		Evidence evidence = new Evidence();
		/*
		 * Using inferred by sequence similarity as the default evidence code
		 */
		evidence.setCode(code);
		/* 
		 * Basically, this references states who made and is responsible for this 
		 * association of term to gene_product
		 */
		evidence.setDbxref(getPAINTEvidenceDBXref());
		return evidence;
	}

	public DBXref getPAINTEvidenceDBXref() {
		if (paint_dbxref == null) {
			paint_dbxref = new DBXref();
			/*
			 * The is the reference describing how this annotation was made
			 * there is only one for 
			 */
			paint_dbxref.setDb_name(PAINT_DB);
			/* setting the key_type, but not sure what it should really be */
			paint_dbxref.setKeytype("Personal Communication");

			/* 
			 * Augment the name with a description of the database and tool
			 */
			paint_dbxref.setDescription("PAINT protein family curation");
			/*
			 * This is not a dbxref for the definition of a term 
			 * It is a dbxref to the group doing the annotation
			 */
			paint_dbxref.setFor_definition(false);
		}
		paint_dbxref.setAccession("DEL_TEST1");
		return paint_dbxref;
	}

}
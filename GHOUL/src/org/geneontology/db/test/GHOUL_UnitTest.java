package org.geneontology.db.test;

import java.util.Iterator;
import java.util.Set;
import java.util.Vector;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.apache.log4j.Logger;
import org.geneontology.db.factory.GOobjectFactory;
import org.geneontology.db.model.Association;
import org.geneontology.db.model.DB;
import org.geneontology.db.model.DBXref;
import org.geneontology.db.model.Evidence;
import org.geneontology.db.model.GOModel;
import org.geneontology.db.model.GeneProduct;
import org.geneontology.db.model.HomolSet;
import org.geneontology.db.model.MetaRelationship;
import org.geneontology.db.model.ProductSeq;
import org.geneontology.db.model.Relationship;
import org.geneontology.db.model.Sequence;
import org.geneontology.db.model.Species;
import org.geneontology.db.model.Term;
import org.geneontology.db.model.TermDBXref;
import org.geneontology.db.model.TermSynonym;
import org.geneontology.db.util.HibernateUtil;
import org.hibernate.SessionFactory;
import org.hibernate.classic.Session;

public class GHOUL_UnitTest extends TestCase{

	private SessionFactory sessionFactory;
	private GOobjectFactory objFactory;
	protected final static Logger logger = Logger.getLogger(GHOUL_UnitTest.class);

	public GHOUL_UnitTest(){
		try {
			this.sessionFactory = HibernateUtil.buildSessionFactory("hibernate.cfg.xml");
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public SessionFactory getSessionFactory() {
		return this.sessionFactory;
	}
	
	public GOobjectFactory getObjFactory() {
		if (objFactory == null)
			return initSessionFactory();
		return objFactory;
	}

	public void setSessionFactory(SessionFactory sf) {
		this.sessionFactory = sf;
	}

	public GOobjectFactory initSessionFactory() {
		this.getSessionFactory().getCurrentSession().beginTransaction();
		objFactory = new GOobjectFactory(this.getSessionFactory());
		return objFactory;
	}

	public void testGP() {
//		this.getSessionFactory().getCurrentSession().beginTransaction();
//		GeneProduct gp = (GeneProduct) this.getSessionFactory().getCurrentSession().get(GeneProduct.class, 252956);
		GOobjectFactory factory = initSessionFactory();
		GeneProduct gp = (GeneProduct) factory.getGPByDBXrefStr("ZFIN:ZDB-GENE-030318-3");
		Assert.assertTrue(gp != null);
		if (gp != null) {
			logger.assertLog(gp.getSymbol().equals("myo6b"), "Symbol does not match myo6b");
			Assert.assertEquals (gp.getSymbol(), "myo6b");
			logger.assertLog(gp.getFull_name().equals("myosin VIb"), "Fullname does not match myosin VIb");
			Assert.assertEquals (gp.getFull_name(), "myosin VIb");
			logger.assertLog(gp.getDbxref().getDb_name().equals("ZFIN"), "DB does not match ZFIN");
			Assert.assertEquals(gp.getDbxref().getDb_name(), "ZFIN");
			logger.assertLog(gp.getDbxref().getAccession().equals("ZDB-GENE-030318-3"), "UID does not match ZDB-GENE-030318-3");
			Assert.assertEquals(gp.getDbxref().getAccession(), "ZDB-GENE-030318-3");
			logger.assertLog(gp.getSpecies().getNcbi_taxa_id() == 7955, "taxa_id does not match 7955");
			Assert.assertEquals(gp.getSpecies().getNcbi_taxa_id(), 7955);
			logger.assertLog(gp.getSpecies().getCommon_name().equals("zebrafish"), "Common name does not match zebrafish");
			Assert.assertEquals(gp.getSpecies().getCommon_name(), "zebrafish");
			logger.assertLog(gp.getSpecies().getGenus().equals("Danio"), "Genus name does not match Danio");
			Assert.assertEquals(gp.getSpecies().getGenus(), "Danio");
			logger.assertLog(gp.getSpecies().getSpecies().equals("rerio"), "species name does not match rerio");
			Assert.assertEquals(gp.getSpecies().getSpecies(), "rerio");
			logger.info("\n" + prettyPrint(gp));

			gp = (GeneProduct) factory.getGPByDBXref("RGD", "Q5M819");
			Assert.assertTrue(gp != null);

			Iterator<Association> it = factory.getAssociationsIteratorByGP(gp);
			Assert.assertTrue(it.hasNext());
			while (it.hasNext()) {
				Association assoc = it.next();
				logger.info(" assoc: "+assoc);
			}

		}
	}

	public void testGPJoin() {
		GOobjectFactory factory = initSessionFactory();
		GeneProduct gp = (GeneProduct) factory.getGPByDBXref("RGD", "Q5M819");
		Assert.assertTrue(gp != null);
		if (gp == null) {
			gp = (GeneProduct) factory.getGPByName("Psph");
			Assert.assertTrue(gp != null);
		}
		logger.info(prettyPrint(gp));
	}

	public void testObsolete() {
		Term term = (Term) getObjFactory().getTermByAcc("GO:0000005"); // ribosomal chaperone activity
		Assert.assertTrue(term != null);
		if (term != null) {
			logger.info("\n\t" + prettyPrint(term));
		}
		Assert.assertTrue(term.isObsolete());
	}

	public void testTerm() {
		Term term = (Term) getObjFactory().getTermByAcc("GO:0000011"); // vacuole inheritance
		Assert.assertTrue(term != null);
		if (term != null) {
			/** check for associations with foreign keys to DB table that don't exist in the DB table */
			for (Association a : term.getAssociations()) {
				logger.assertLog(a.getSource_db().getName() != null, 
						term.getAcc() + " has association (id=" + a.getAssoc_id() + ") with source_db_id null");
				Assert.assertTrue(term.getAcc() + " has association with source_db_id null", 
						a.getSource_db().getName() != null);
			}			
			logger.info("\n\t" + prettyPrint(term));
		}
	}

	public void testTermParents() {
		Vector<String> gp_list = new Vector<String> ();
		gp_list.add("MGI:98358");
		gp_list.add("3735"); // RGD
		gp_list.add("ZDB-GENE-011207-1");
		GOobjectFactory factory = initSessionFactory();
		Vector<Term> terms = (Vector<Term>) factory.getTermIntersectionByGP(gp_list);
		if (terms != null) {
			/** check for associations with foreign keys to DB table that don't exist in the DB table */
			for (Iterator<Term> it = terms.iterator(); it.hasNext();) {
				Term a = it.next();
				logger.assertLog(a.getAcc() != null, 
						"id=" + a.getAcc() + " is named " + a.getName());
				Assert.assertTrue(a.getAcc() + " has name ", 
						a.getName() != null);
			}			
		}

	}

	private GeneProduct testGetGP_byName() {
		GOobjectFactory factory = initSessionFactory();
		Iterator<GeneProduct> gps = factory.getGPByName("clock");
		GeneProduct found = null;
		while (gps.hasNext() && found == null) {
			GeneProduct gp = gps.next();
			found = (gp.getGp_id() == 244529 ? gp : null);
		}
		Assert.assertTrue(found != null);
		return found;
	}
	
	public void testAssocQualifier() {
		GOobjectFactory factory = initSessionFactory();
		GeneProduct gp = testGetGP_byName();
		boolean found = false;
		System.out.println("symbol = " + gp.getSymbol());
		Set<Association> assoc = gp.getAssociations();
		for (Iterator<Association> it = assoc.iterator(); it.hasNext(); ) {
			Association a = it.next();
			System.out.println("association id = " + a.getAssoc_id());
			Set<Term> quals = a.getQualifiers();
			for (Iterator<Term> t = quals.iterator(); t.hasNext(); ) {
				Term term = t.next();
				System.out.println("qualifier = " + term.getName());
				found = true;
			}
		}
		Assert.assertTrue(found);
	}

	/** for testing purposes */
	public void prettyPrint(GOModel model) {
		Assert.assertTrue((model != null));
		logger.info(model);
	}

	public String prettyPrint(GeneProduct gp) {
		StringBuffer buffer = new StringBuffer();
		buffer.append("GENEP:\t" + gp.getSymbol() + " (" + gp.getFull_name() + ")\t" + 
				gp.getDbxref().getDb_name() + (validDBName(gp.getDbxref().getDb_name())) + ":" + 
				gp.getDbxref().getAccession() + "\t" + getString(gp.getSpecies()) + "\tSO-type=" +
				gp.getSO_type().getName() + "\n");
		for (String s : gp.getSynonyms()) {
			buffer.append("\tSYN:\t" + s + "\n");
		}
		for (Association a : gp.getAssociations()) {
			prettyPrint (a, buffer);
		}
		prettyPrint(gp.getHomol_set(), buffer);
		for (ProductSeq s : gp.getSeqs()) {
			prettyPrint (s.getSeq(), buffer);
		}
		return buffer.toString();
	}

	public String getString (Species sp) {
		if (sp == null)
			return "species=null";
		else
			return sp.getGenus() + " " + sp.getSpecies();
	}

	public String prettyPrint (Sequence s, StringBuffer buffer) {
		buffer.append("\tSEQ:\t" + s.getName() + "\t(lastmodified=" + s.getTimelastmodified() + ")");
		buffer.append("\tmoltype=" + s.getMoltype() + "\tdescription=" + s.getDescription());
		buffer.append("\tlen=" + s.getSeq_len() + "\n");
		for (DBXref x : s.getDbxrefs()) {
			prettyPrint (x, buffer, "\tSEQ_XREF:");
		}
		buffer.append("\n");
		return buffer.toString();
	}

	protected String prettyPrint(Term term) {
		StringBuffer buffer = new StringBuffer();
		return prettyPrint(term, buffer);
	}

	public String prettyPrint(Term term, StringBuffer buffer) {
		buffer.append("TERM:\tCV=" + term.getCv() + "\tuid=" + term.getAcc() + "\tname=" + term.getName() + 
				"\tobsolete=" + term.getIs_obsolete() + "\tis_root=" + term.getIs_root() + "\n");
		buffer.append("\tTERM_DEF: " + term.getDefinition() + "\n");

		for (TermDBXref x : term.getTermDBXrefs()) {
			if (x.getIs_for_definition() == 1)
				prettyPrint (x.getDbxref(), buffer, "\tDEF_XREF");
			else
				prettyPrint (x.getDbxref(), buffer, "\tTERM_XREF");
		}
		prettyPrintSyns(term, buffer);

		for (Association a : term.getAssociations()) {
			prettyPrint (a, buffer);
		}
		prettyPrintParents (term.getParents(), buffer);
		prettyPrintChildren (term.getChildren(), buffer);
		prettyPrintSubsets (term.getSubsets(), buffer);
		prettyPrintConsiderations (term.getConsiderations(), buffer);
		return buffer.toString();
	}

	protected void prettyPrintParents(Set<Relationship> rels, StringBuffer buffer) {
		for (Relationship r : rels) {
			buffer.append("\tPARENT:\t" + r.getType().getName() + "\t" + r.getObject().getName() + "\n");
		}
	}

	protected void prettyPrintChildren(Set<Relationship> rels, StringBuffer buffer) {
		for (Relationship r : rels) {
			buffer.append("\tCHILD:\t" + r.getType().getName() + "\t" + r.getSubject().getName() + "\n");
		}
	}

	protected void prettyPrintSubsets(Set<Term> slims, StringBuffer buffer) {
		for (Term t : slims) {
			buffer.append("SUBSET:\t" + t.getName());
		}
		buffer.append("\n");
	}

	protected String prettyPrint(DBXref dbx, StringBuffer buffer, String tag) {
		if (dbx == null) {
			buffer.append(tag + "\tNo homolset" + "\n");
		} else {
			buffer.append(tag + "\t" + dbx.getDb_name() + 
					(validDBName(dbx.getDb_name())) + ":" + 
					dbx.getAccession() + 
					"\tdesc=" + dbx.getDescription() + "\tkeytype=" + dbx.getKeytype() + "\n");		
		}
		return buffer.toString();
	}

	private String validDBName(String db_name) {
		GOobjectFactory goFactory = initSessionFactory();	
		DB db = goFactory.getDBByName(db_name);
		return (db == null ? "(not in db table)" : "");
	}

	protected String prettyPrintSyns(Term term, StringBuffer buffer) {
		for (TermSynonym tsyn : term.getSynonyms()){
			buffer.append("\tSYN:\t" + tsyn.getSynonym() + "\ttype=" + tsyn.getSynonymType().getName() + 
					"\tcategory=" + tsyn.getSynonymCategory() + "\talt_id=" + tsyn.getAlternateID() + "\n");

		}
		return buffer.toString();
	}

	protected void prettyPrintConsiderations(Set<MetaRelationship> metas, StringBuffer buffer) {
		for (MetaRelationship r : metas) {
			buffer.append("META:\t" + r.getType().getAcc() + "\t" + r.getObject().getName());
		}
	}

	protected String prettyPrint(HomolSet homol_set, StringBuffer buffer) {
		if (homol_set == null) {
			buffer.append("No homolset");
		} else {
			GeneProduct gp = homol_set.getTarget_gp();
			Term term = homol_set.getTerm();
			buffer.append("\tHOMOL:\t" + homol_set.getSymbol() + "\tdescription=" + homol_set.getDescription() + "\t" +
					getString(homol_set.getSpecies()) + "\t" +
					(gp == null ? "gp=null" : "gp=" + gp.getSymbol()) + "\t" +
					(term == null ? "term=null" : "term=" + term.getName()) + "\n");
			prettyPrint (homol_set.getDbxref(), buffer, "\tHOMOL-XREF:");
			buffer.append("\tHOMOL_GENES:\t");
			for (GeneProduct member : homol_set.getGenes()) {
				buffer.append(member.getSymbol() + "\t");
			}
			buffer.append("\n");
		}
		return buffer.toString();
	}

	protected void prettyPrint(Association assoc, StringBuffer buffer) {
		if (assoc == null) {
			Assert.assertNotNull("Gene product without associations", assoc);
			logger.info("Association is null");
			buffer.append("\tASSOC:\tnone" + "\n");
		} else {
			buffer.append("\tASSOC:\tgp=" + assoc.getGene_product().getSymbol() + "\tterm=" + assoc.getTerm().getName() +
					(assoc.getIs_not() == null ? "\t" : (assoc.getIs_not() == 0 ? "\t" : "\tNOT")) +
					(assoc.getSource_db() == null ? "\tsource=null" : assoc.getSource_db().getName()) + 
					"\tdate=" + assoc.getDate());
			for (Evidence e : assoc.getEvidence()) {
				prettyPrint(e, buffer);
			}
		}
	}

	protected void prettyPrint (Evidence e, StringBuffer buffer) {
		buffer.append("\n\t\tEVIDENCE:\t" + e.getCode() + "\txref=" + e.getDbxref().getDb_name() + ":" + e.getDbxref().getAccession());
		for (DBXref x : e.getWiths()) {
			buffer.append("\tWith=" + x.getDb_name() + ":" + x.getAccession());
		}
		buffer.append("\n");
	}
}
package org.geneontology.db.test;

import java.util.Set;

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
import org.geneontology.db.model.MetaRelation;
import org.geneontology.db.model.ProductSeq;
import org.geneontology.db.model.Relation;
import org.geneontology.db.model.Sequence;
import org.geneontology.db.model.Species;
import org.geneontology.db.model.Term;
import org.geneontology.db.model.TermDBXref;
import org.geneontology.db.model.TermSynonym;
import org.geneontology.db.util.HibernateUtil;
import org.hibernate.SessionFactory;

public class GHOUL_UnitTest extends TestCase{
	
	private SessionFactory sessionFactory;
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

	public void setSessionFactory(SessionFactory sf) {
		this.sessionFactory = sf;
	}
	
	public GOobjectFactory initSessionFactory() {
		this.getSessionFactory().getCurrentSession().beginTransaction();
		return (new GOobjectFactory(this.getSessionFactory()));
	}
	
	public void testGP() {
		this.getSessionFactory().getCurrentSession().beginTransaction();
		GeneProduct gp = (GeneProduct) this.getSessionFactory().getCurrentSession().get(GeneProduct.class, 250070);
		Assert.assertTrue(gp != null);
		if (gp != null) {
			logger.assertLog(gp.getSymbol().equals("myo6b"), "Symbol matches myo6b");
			Assert.assertEquals (gp.getSymbol(), "myo6b");
			logger.assertLog(gp.getFull_name().equals("myosin VIb"), "Fullname matches myosin VIb");
			Assert.assertEquals (gp.getFull_name(), "myosin VIb");
			logger.assertLog(gp.getDbxref().getDb_name().equals("ZFIN"), "DB matches ZFIN");
			Assert.assertEquals(gp.getDbxref().getDb_name(), "ZFIN");
			logger.assertLog(gp.getDbxref().getAccession().equals("ZDB-GENE-030318-3"), "UID matches ZDB-GENE-030318-3");
			Assert.assertEquals(gp.getDbxref().getAccession(), "ZDB-GENE-030318-3");
			logger.assertLog(gp.getSpecies().getNcbi_taxa_id() == 7955, "taxa_id matches 7955");
			Assert.assertEquals(gp.getSpecies().getNcbi_taxa_id(), 7955);
			logger.assertLog(gp.getSpecies().getCommon_name().equals("zebrafish"), "Common name matches zebrafish");
			Assert.assertEquals(gp.getSpecies().getCommon_name(), "zebrafish");
			logger.assertLog(gp.getSpecies().getGenus().equals("Danio"), "Genus name matches Danio");
			Assert.assertEquals(gp.getSpecies().getGenus(), "Danio");
			logger.assertLog(gp.getSpecies().getSpecies().equals("rerio"), "species name matches rerio");
			Assert.assertEquals(gp.getSpecies().getSpecies(), "rerio");
			logger.info("\n" + prettyPrint(gp));
		}
	}
	
	public void testObsolete() {
		this.getSessionFactory().getCurrentSession().beginTransaction();
		Term term = (Term) this.getSessionFactory().getCurrentSession().get(Term.class, 25320);
		Assert.assertTrue(term != null);
		if (term != null) {
			logger.info("\n\t" + prettyPrint(term));
		}
	}
	
	public void testTerm() {
		this.getSessionFactory().getCurrentSession().beginTransaction();
		Term term = (Term) this.getSessionFactory().getCurrentSession().get(Term.class, 24482);
		Assert.assertTrue(term != null);
		if (term != null) {
			/** check for associations with foreign keys to DB table that don't exist in the DB table */
			for (Association a : term.getAssociations()) {
				logger.assertLog(a.getSource_db().getName() == null, 
						term.getAcc() + " has association (id=" + a.getAssoc_id() + ") with source_db_id null");
				Assert.assertTrue(term.getAcc() + " has association with source_db_id null", 
								a.getSource_db().getName() == null);
			}			
			logger.info("\n\t" + prettyPrint(term));
		}
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
			buffer.append("SYN:\t" + s + "\n");
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
	
	protected void prettyPrintParents(Set<Relation> rels, StringBuffer buffer) {
		for (Relation r : rels) {
			buffer.append("\tPARENTS:\t" + r.getType().getName() + "\t" + r.getObject().getName());
		}
		buffer.append("\n");
	}
	
	protected void prettyPrintChildren(Set<Relation> rels, StringBuffer buffer) {
		for (Relation r : rels) {
			buffer.append("\tCHILDREN:\t" + r.getType().getName() + "\t" + r.getSubject().getName());
		}
		buffer.append("\n");
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
			buffer.append("SYN:\t" + tsyn.getSynonym() + "\t" + tsyn.getSynonymCategory() + "\t" + 
					tsyn.getSynonymType().getName() + "\t" + tsyn.getAlternateID());
			
		}
		return buffer.toString();
	}

	protected void prettyPrintConsiderations(Set<MetaRelation> metas, StringBuffer buffer) {
		for (MetaRelation r : metas) {
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
	
	protected String prettyPrint(Association assoc, StringBuffer buffer) {
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
				buffer.append("\n\t\tEVI:\t");
				buffer.append(prettyPrint(e, buffer));
			}
		}
		return buffer.toString();
	}
	
	protected String prettyPrint (Evidence e, StringBuffer buffer) {
		buffer.append("\tevidence:\t" + e.getCode() + "\txref=" + e.getDbxref().getAccession());
		for (DBXref x : e.getWiths()) {
			buffer.append("\tWith=" + x.getAccession());
		}
		buffer.append("\n");
		return buffer.toString();
	}
}
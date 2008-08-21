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


public class AbstractGOHibernateAPITest extends TestCase{
	
	private SessionFactory sessionFactory;
	protected final static Logger logger = Logger.getLogger(AbstractGOHibernateAPITest.class);

	
	public AbstractGOHibernateAPITest(){
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
	
	/** for testing purposes */
	public void prettyPrint(GOModel model) {
		Assert.assertTrue((model != null));
		logger.info(model);
	}
	
	public void prettyPrint(GeneProduct gp) {
		System.out.println("GENEP:\t" + gp.getSymbol() + " (" + gp.getFull_name() + ")\t" + 
				gp.getDbxref().getDb_name() + (validDBName(gp.getDbxref().getDb_name())) + ":" + 
				gp.getDbxref().getAccession() + "\t" + getString(gp.getSpecies()) + "\tSO-type=" +
				gp.getSO_type().getName());
		for (String s : gp.getSynonyms()) {
			System.out.println("SYN:\t" + s);
		}
		for (Association a : gp.getAssociations()) {
			prettyPrint (a);
		}
		for (ProductSeq s : gp.getSeqs()) {
			prettyPrint (s.getSeq());
		}
		prettyPrint(gp.getHomol_set());
	}
	
	public String getString (Species sp) {
		if (sp == null)
			return "species=null";
		else
			return sp.getGenus() + " " + sp.getSpecies();
	}
	
	public void prettyPrint (Sequence s) {
		System.out.println("SEQ:\t" + s.getName() + "\t(lastmodified=" + s.getTimelastmodified() + ")");
		System.out.println("SEQ:\tmoltype=" + s.getMoltype() + "\tdescription=" + s.getDescription());
		System.out.println("SEQ:\tlen=" + s.getSeq_len() + "\tchecksum=" + s.getMd5checksum());
		for (DBXref x : s.getDbxrefs()) {
			prettyPrint (x, "SEQ_XREF:");
		}

	}

	public void prettyPrint(Term term) {
		System.out.println(term.getCv() + " - " + term.getAcc() + ": " + term.getName());
		System.out.println("def: " + term.getDefinition());
		System.out.println("obsolete=" + term.getIs_obsolete() + ", is_root=" + term.getIs_root());
		
		for (TermDBXref x : term.getTermDBXrefs()) {
			if (x.getIs_for_definition() == 1)
				prettyPrint (x.getDbxref(), "DEF_XREF");
			else
				prettyPrint (x.getDbxref(), "TERM_XREF");
		}
		prettyPrintSyns(term);
		
		for (Association a : term.getAssociations()) {
			prettyPrint (a);
		}
		prettyPrintParents (term.getParents());
		prettyPrintChildren (term.getChildren());
		prettyPrintSubsets (term.getSubsets());
		prettyPrintConsiderations (term.getConsiderations());
	}
	
	protected void prettyPrintParents(Set<Relation> rels) {
		for (Relation r : rels) {
			System.out.println("PARENTS:\t" + r.getType().getName() + "\t" + r.getObject().getName());
		}
	}
	
	protected void prettyPrintChildren(Set<Relation> rels) {
		for (Relation r : rels) {
			System.out.println("CHILDREN:\t" + r.getType().getName() + "\t" + r.getSubject().getName());
		}
	}

	protected void prettyPrintSubsets(Set<Term> slims) {
		for (Term t : slims) {
			System.out.println("SUBSET:\t" + t.getName());
		}
	}

	protected void prettyPrint(DBXref dbx, String tag) {
		if (dbx == null) {
			System.out.println(tag + "\tNo homolset");
		} else {
			System.out.println(tag + "\t" + dbx.getDb_name() + 
					(validDBName(dbx.getDb_name())) + ":" + 
					dbx.getAccession() + 
					"\tdesc=" + dbx.getDescription() + "\tkeytype=" + dbx.getKeytype());		
		}
	}
	
	private String validDBName(String db_name) {
		GOobjectFactory goFactory = initSessionFactory();	
		DB db = goFactory.getDBByName(db_name);
		return (db == null ? "(not in db table)" : "");
	}
	
	protected void prettyPrintSyns(Term term) {
		for (TermSynonym tsyn : term.getSynonyms()){
			System.out.println("SYN:\t" + tsyn.getSynonym() + "\t" + tsyn.getSynonymCategory() + "\t" + 
					tsyn.getSynonymType().getName() + "\t" + tsyn.getAlternateID());
			
		}
	}

	protected void prettyPrintConsiderations(Set<MetaRelation> metas) {
		for (MetaRelation r : metas) {
			System.out.println("META:\t" + r.getType().getAcc() + "\t" + r.getObject().getName());
		}
	}

	protected void prettyPrint(HomolSet homol_set) {
		if (homol_set == null) {
			System.out.println("No homolset");
		} else {
			GeneProduct gp = homol_set.getTarget_gp();
			Term term = homol_set.getTerm();
			System.out.println("HOMOL:\t" + homol_set.getSymbol() + "\tdescription=" + homol_set.getDescription());
			prettyPrint (homol_set.getDbxref(), "HOMOL-XREF:");
			System.out.println("HOMOL:\t" + getString(homol_set.getSpecies()) + "\t" +
					(gp == null ? "no target" : "gp=" + gp.getSymbol()) + "\t" +
					(term == null ? "no term" : "term=" + term.getName()) + "\t");
			for (GeneProduct member : homol_set.getGenes()) {
				System.out.println("\thomol_gene:\t" + member.getSymbol());
			}
		}
	}
	
	protected void prettyPrint(Association assoc) {
		if (assoc == null) {
			System.out.println("\tASSOC:\tnone");
		} else {
			System.out.println("\tASSOC:\t" + (assoc.getSource_db() == null ? "null" : assoc.getSource_db().getName()) + 
				"\tdate=" + assoc.getDate() + 
				(assoc.getIs_not() == null ? "\t" : (assoc.getIs_not() == 0 ? "\t" : "\tNOT")));
			for (Evidence e : assoc.getEvidence()) {
				prettyPrint(e);
			}
		}
	}
	
	protected void prettyPrint (Evidence e) {
		System.out.print("\tevidence:\t" + e.getCode() + "\txref=" + e.getDbxref().getAccession());
		for (DBXref x : e.getWiths()) {
			System.out.print("\tWith=" + x.getAccession());
		}
		System.out.println();
	}
}
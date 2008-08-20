package org.geneontology.db.test;

import java.util.Set;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.apache.log4j.Logger;
import org.geneontology.db.factory.GOobjectFactory;
import org.geneontology.db.model.DBXref;
import org.geneontology.db.model.GOModel;
import org.geneontology.db.model.GeneProduct;
import org.geneontology.db.model.MetaRelation;
import org.geneontology.db.model.ProductSeq;
import org.geneontology.db.model.Relation;
import org.geneontology.db.model.Sequence;
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
		System.out.println("GENEP:" + gp.getSymbol() + " (" + gp.getFull_name() + ")\t" + 
				gp.getDbxref().getDb().getFullname() + ":" + gp.getDbxref().getAccession() + "\t" +
				gp.getSpecies().getGenus() + " " + gp.getSpecies().getSpecies() + "\t" +
				gp.getSO_type().getName());
		for (String s : gp.getSynonyms()) {
			System.out.println("SYN:\t" + s);
		}
		for (ProductSeq s : gp.getSeqs()) {
			prettyPrint (s.getSeq());
		}

	}
	
	public void prettyPrint (Sequence s) {
		System.out.println("SEQ:\t" + s.getName() + "\tl(lastmodified=" + s.getTimelastmodified() + ")");
		System.out.println("SEQ:\tmoltype=" + s.getMoltype() + "\t" + s.getDescription());
		System.out.println("SEQ:\tlen=" + s.getSeq_len() + "\tchecksum=" + s.getMd5checksum());
		for (DBXref x : s.getDbxrefs()) {
			prettyPrint (x, "SEQ_XREF");
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
		System.out.println(tag + "\t" + dbx.getDb().getName() + ":" + dbx.getAccession() + "\t" 
				+ dbx.getDescription() + "\t" + dbx.getKeytype());		
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


}
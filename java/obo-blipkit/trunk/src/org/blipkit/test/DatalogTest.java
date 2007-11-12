package org.blipkit.test;

import java.io.*;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Hashtable;

import org.bbop.io.AuditedPrintStream;
import org.blipkit.datamodel.impl.DatalogBackedMutableLinkDatabase;
import org.obo.datamodel.impl.DefaultLinkDatabase;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.test.AbstractOBOTest;



import jpl.Atom;
import jpl.Compound;
import jpl.Query;
import jpl.Term;
import jpl.Variable;
import junit.framework.*;

public class DatalogTest extends AbstractOBOTest {

	protected DatalogTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"nucleus.obo"};
		return Arrays.asList(files);
	}
	
	public void setUpXXX() throws Exception {
		System.out.println("Setting up: " + this);
		ForwardChainingReasoner.checkRecache = false;
		session = getSessionFromResources(getFilesToLoad());

		// SessionManager.getManager().setSession(session);
		linkDatabase = new DatalogBackedMutableLinkDatabase(session);

	}

	public void setUp() throws Exception {
		System.out.println("foo");
		System.out.println("Setting up: " + this);
		
		assertFact("zyb:foo", "a", "b");
		new Query("use_module(bio(io))").allSolutions();
		new Query("load_bioresource(go)").allSolutions();
		System.out.println("asserted");
	}

	public void testHasLoaded() {
		// TODO
		Variable X = new Variable("X");
		Term iq =
		    new Compound(
		        "foo",
		        new Term[] {new Atom("a"),X}
//		        new Term[] {Y,new Atom("X")}

		    );
		//q = new Query("zyb:foo(a,X)");
		Query q = new Query(":",new Term[]{new Atom("zyb"),iq});
		
		for (Hashtable h : q.allSolutions()) {
//			System.out.println(q+"// "+X.toString()+" soln= "+h.get(X.toString()));
//			System.out.println(q+"// "+X.toString()+" soln= "+h.get("X"));
			System.out.println(q+"// "+X.toString()+" soln= "+h.get(X.toString()));
			
		}
		for (Hashtable h : new Query("ontol_db:subclassT(X,'GO:0005634')").allSolutions()) {
			System.out.println(h.get("X"));
		}
		assertTrue(true);
	}
	

	protected int assertFact(String pred, Object... args) {
		Atom[] atomA = new Atom[args.length];
		int i=0;
		for (Object a: args) {
			atomA[i] = new Atom((String)a);
			i++;
		}
		
		Compound term = new Compound(pred,atomA);
//				new Term[]{
//					atomA
//					});
		//Compound assertTerm = new Compound("assert",new Term[]{term});
		//System.out.println(term);
		Query q = new Query("assert", term);
		//System.out.println(q);
		for (Hashtable h : q.allSolutions()) {
			System.err.println("hmm "+h);
		}
		return 0;
	}

	public static Test suite() {
		PrintStream audited = new AuditedPrintStream(System.err, 25, true);

		System.setErr(audited);
		TestSuite suite = new TestSuite();
		addTests(suite);
		return suite;
	}

	public static void addTests(TestSuite suite) {
		suite.addTest(new DatalogTest("testHasLoaded"));
	}
}




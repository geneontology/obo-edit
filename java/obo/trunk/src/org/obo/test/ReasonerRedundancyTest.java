package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;
import org.obo.reasoner.impl.LinkPileReasonerFactory;

import org.apache.log4j.*;

public class ReasonerRedundancyTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ReasonerRedundancyTest.class);

	public ReasonerRedundancyTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"simple_redundancy_test.obo"};
		return Arrays.asList(files);
	}
	

	/**
	 * asserted:
	 * 
	 * B
	 * -A  == B that (part_of C)
	 * -D  (part_of C)
	 * --E
	 * -E
	 * 
	 * inferred:
	 * 
	 * B
	 * -A   == B that part_of C
	 * --D 
	 * ---E
	 * 
	 * note that the asserted D is_a B appears redundant:
	 *   D is_a A is_a B -- however, D is_a A comes from the xp def of A
	 *   
	 * 
	 * 
	 * @throws Exception
	 */
	public void testLinks() throws Exception {
		
		testForIsA("A","B"); /* genus */
		testForIsA("D","B"); /* asserted */
		testForIsA("D","A"); /* completeness + asserted */
		testForIsA("E","B"); /* transitivity */

		testForIsAInTrimmed("E","D");
		testForIsAInTrimmed("D","A");
		testForIsAInTrimmed("A","B");
		testForRedundantIsA("E","B");
		testForNonRedundantIsA("D","B"); // only appears redundant: removing will remove evidence
		assertFalse(hasIsALink(trimmedDB, "E", "A"));
		
		//TODO: option of not showing redundants in trimmed
		//assertFalse(hasIsALink(trimmedDB, "E", "B"));
		
	}
	
	public void testBasicLinks() throws Exception {
	
		testForIsA("X1","X2"); /* asserted */
		testForIsA("X2","X3"); /* asserted */
		testForIsA("X1","X3"); /* asserted & transitivity */

		testForRedundantIsA("X1","X3");
			
	}

	/**
	 * B
	 * -EB = B that df C
	 * -tripus (df C)
	 * 
	 * @throws Exception
	 */
	public void testBoneLinks() throws Exception {
		//AbstractReasonerTest.setReasonerFactory(new ForwardChainingReasonerFactory());
		
		testForIsA("endochondral_bone","bone"); /* genus */
		testForIsA("tripus","bone"); /* asserted */
		testForIsA("tripus","endochondral_bone"); /* completeness */

		testForIsAInTrimmed("tripus","endochondral_bone");
		//testForRedundantIsA("tripus","bone"); // only redundant in non-repair mode
		
	}



}


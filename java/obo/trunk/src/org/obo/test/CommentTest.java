package org.obo.test;

import java.util.Arrays;
import java.util.Collection;

import junit.framework.TestSuite;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;

import org.apache.log4j.*;

public class CommentTest extends AbstractOBOTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CommentTest.class);

	public CommentTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"nucleus.obo", "nucleus_xp.obo"};
		return Arrays.asList(files);
	}
	
	public static Collection<String> getTests() {
		String[] tests={};
		return Arrays.asList(tests);
	}

	public void testComment() throws Exception {
		// basic comment parsing
		// this test MUST work
		this.testForComment("GO:0005575",
				"Note that, in addition to forming the root of the cellular component ontology, this term "+
				"is recommended for use for the annotation of gene products whose cellular component is unknown. "+
				"Note that when this term is used for annotation, it indicates that no information was available "+
				"about the cellular component of the gene product annotated as of the date the annotation was made; "+
				"the evidence code ND, no data, is used to indicate this.");
		
		// this test is for situations in which the same term in loaded from multiple files.
		// for example, when MIREOTing is used. previously OE would concatenate comments
		// together, leading to duplication.
		// this test ensures that OE is smarter, and if a term T is loaded from F1 and F2,
		// and the value of the comment field is identical in F1 and F2, then the
		// comments are NOT concatenated together.
		this.testForComment("GO:0043229","test_comment");
		
		// we leave the 3rd situation undefined - namely if the comment field is
		// NOT identical in F1 and F2. TBD: either comment should take precedence, or
		// they should be concatenated.
		
		
	}


}


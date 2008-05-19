package org.oboedit.test;

import org.bbop.dataadapter.*;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.dataadapter.*;
import org.obo.history.HistoryGenerator;
import org.obo.history.HistoryItem;
import org.obo.history.HistoryList;
import org.obo.history.OperationModel;
import org.obo.history.OperationWarning;
import org.obo.util.TermUtil;
import org.oboedit.util.PathUtil;

import junit.framework.*;
import java.io.*;
import java.util.Iterator;
import javax.swing.tree.TreePath;

import org.apache.log4j.*;

public class TestUtil extends TestCase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TestUtil.class);

	protected OBOSession session;
	protected OperationModel operationModel;

	// protected List objectList = new ArrayList();
	// protected List linkList = new ArrayList();

	public TestUtil(OBOSession session) {
		this.session = session;
		operationModel = new DefaultOperationModel();
		operationModel.setSession(session);
	}

	public static String getRandomString(int length) {
		StringBuffer out = new StringBuffer();
		for (int i = 0; i < length; i++) {
			out.append(getRandomChar());
		}
		return out.toString();
	}

	public static String getRandomID() {
		StringBuffer out = new StringBuffer();
		out.append("ID:");
		for (int i = 0; i < 7; i++)
			out.append((int) Math.floor(Math.random() * 10));
		return out.toString();
	}

	public void apply(HistoryList historyList) {
		Iterator it = historyList.getHistoryItems();
		while (it.hasNext()) {
			HistoryItem item = (HistoryItem) it.next();

			OperationWarning warning = apply(item);
			assertNull("No operation should generate a warning. Got " + warning
					+ " from " + item, warning);
		}
	}

	public OperationWarning apply(HistoryItem item) {
		OperationWarning warn = operationModel.apply(item);
		if (warn != null)
			System.err
					.println("WARNING WHILE TESTING: " + warn + " on " + item);
		return warn;
	}

	public OperationWarning reverse(HistoryItem item) {
		return operationModel.reverse(item);
	}

	protected static char getRandomChar() {
		int val = (int) (Math.random() * 100);
		if (val < 20)
			return ' ';
		else if (val > 98)
			return getRandomExtendedChar();
		else
			return getRandomBasicChar();
	}

	protected static char getRandomBasicChar() {
		return (char) (33 + Math.random() * 93);
	}

	protected static char getRandomExtendedChar() {
		return (char) (2 + Math.random() * 126);
	}

	public static TestUtil getInstance() {
		return new TestUtil(createSession());
	}

	public static OBOSession createSession() {
		OBOFileAdapter adapter = new OBOFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		config.getReadPaths().add("test_resources/testfile.1.0.obo");
		try {
			OBOSession session = (OBOSession) adapter.doOperation(
					OBOAdapter.READ_ONTOLOGY, config, null);
			return session;
		} catch (Exception ex) {
			throw new RuntimeException(ex);
		}
	}

	public OBOSession getSession() {
		return session;
	}

	public OBOClass getRandomClass() {
		return (OBOClass) getRandomObject(OBOClass.class);
	}

	public OBOProperty getRandomProperty() {
		return (OBOProperty) getRandomObject(OBOProperty.class);
	}

	public Namespace getRandomNamespace(Namespace notThis) {
		if (session.getNamespaces().size() < 1
				|| ((notThis != null && session.getNamespaces().size() < 2)))
			return null;

		Namespace ns;
		do {
			ns = getRandomNamespace();
			logger.info("randomns = " + ns + ", notThis = " + notThis);
		} while (notThis != null && ns.equals(notThis));

		return ns;
	}

	public TreePath getRandomTermPath() {
		OBOClass oboClass = getRandomClass();
		TreePath[] paths = PathUtil.getPaths(oboClass);
		int index = (int) Math.floor(Math.random() * paths.length);
		return paths[index];
	}

	public Namespace getRandomNamespace() {
		Iterator it = session.getNamespaces().iterator();
		int index = (int) (Math.random() * session.getNamespaces().size());
		logger.info("random_index = " + index);
		Namespace o = null;
		for (int i = 0; it.hasNext() && (i < index || o == null); i++) {
			o = (Namespace) it.next();
		}
		return o;
	}

	public IdentifiedObject getRandomObject(Class c) {
		Iterator it = session.getObjects().iterator();
		int index = (int) (Math.random() * session.getObjects().size());
		Object o = null;
		for (int i = 0; it.hasNext() && (i < index || o == null); i++) {
			IdentifiedObject temp = (IdentifiedObject) it.next();
			if (!TermUtil.isObsolete(temp) && c.isInstance(temp))
				o = temp;
		}
		return (IdentifiedObject) o;
	}

	public Link getRandomLink() {
		int count = 0;
		Iterator it = session.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			if (io instanceof LinkedObject) {
				count += ((LinkedObject) io).getParents().size();
			}
		}
		int index = (int) (Math.random() * count);
		it = session.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			if (io instanceof LinkedObject) {
				Iterator it2 = ((LinkedObject) io).getParents().iterator();
				while (it2.hasNext()) {
					Link link = (Link) it2.next();
					if (index == 0)
						return link;
					index--;
				}
			}
		}
		return null;
	}

	public static OBOSession getSession(String path)
			throws DataAdapterException {
		OBOFileAdapter adapter = new OBOFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		config.getReadPaths().add(path);
		OBOSession session = (OBOSession) adapter.doOperation(OBOAdapter.READ_ONTOLOGY,
				config, null);
		return session;
	}

	public static void sessionCheck(TestCase test, OBOSession a, OBOSession b) {
		HistoryList allChanges = HistoryGenerator.getHistory(a, b);
		test.assertTrue("Differences found in sessions " + a + " and " + b,
				allChanges.size() == 0);
	}

	public static void fileCheck(TestCase test, File a, File b)
			throws IOException {
		BufferedReader readera = new BufferedReader(new FileReader(a));
		BufferedReader readerb = new BufferedReader(new FileReader(b));
		String astr;
		String bstr;
		do {
			astr = readera.readLine();
			bstr = readerb.readLine();

			if (astr == null || bstr == null) {
				if (astr != bstr)
					test.fail("Different numbers of lines in files to compare");
				else
					break;
			}

			if (!(astr.startsWith("date") || astr.startsWith("saved-by")
					|| astr.startsWith("auto-generated-by")
					|| astr.startsWith("format-version")
					|| astr.startsWith("remark") || astr.startsWith("!"))
					|| (astr.startsWith("!") && astr.startsWith("type")))
				test.assertTrue("Lines '" + astr + "' & '" + bstr
						+ "' don't match", astr.equals(bstr));
		} while (astr != null && bstr != null);
	}

	public static Test createSuite(TestCase test, int count) {
		TestSuite suite = new TestSuite();

		for (int i = 0; i < count; i++)
			suite.addTest(test);

		return suite;
	}
}

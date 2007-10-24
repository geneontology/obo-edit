package org.obo.dataadapter;

import java.io.*;
import java.net.*;
import java.util.*;

import org.bbop.dataadapter.*;
import org.bbop.io.IOUtil;
import org.bbop.io.ProgressableInputStream;
import org.bbop.io.SafeFileOutputStream;
import org.bbop.util.*;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.util.TermUtil;

import java.awt.Color;

public class GOFlatFileAdapter implements OBOAdapter {

	Stack termStack;

	OBOClass root;

	OBOClass writeRoot;

	String currentType;

	GOFlatFileConfiguration config;

	protected BufferedReader currentReader;

	protected List writeStreamList = new ArrayList();

	protected List printStreamList = new ArrayList();

	Vector parentageList;

	CompoundGOFlatFileParseException exceptionHolder;

	protected boolean strictParentage = false;

	protected boolean strictDefinition = false;

	protected boolean hideDownstream = true;

	protected static OBOProperty FAILED_TYPE = new OBOPropertyImpl(
			"goflatfile:failed");

	protected static OBOProperty UNKNOWN_TYPE = new OBOPropertyImpl("UNKNOWN",
			"unknown");

	protected static final Map RESERVED_SYMBOLS = new HashMap();

	protected HashMap categories = new HashMap();

	protected HashMap namespaceHash = new HashMap();

	protected HashMap nsHash = new HashMap();

	protected boolean lostDefs = false;

	protected boolean cancelled = false;

	protected int readfileindex;

	protected Map readerCache = new HashMap();

	protected String progressString;

	protected int progressValue = -1;

	protected GraphicalUI advancedUI;

	protected void setProgressValue(int progressValue) {
		this.progressValue = progressValue;
	}

	protected class Queue {
		protected List contents = new Vector();

		public void enqueue(Object o) {
			contents.add(o);
		}

		public Object dequeue() {
			if (size() < 1)
				throw new NoSuchElementException();
			return contents.remove(0);
		}

		public Object peekAt(int index) {
			if (index >= size())
				throw new NoSuchElementException();
			return contents.get(index);
		}

		public Object peek() {
			if (size() < 1)
				throw new NoSuchElementException();
			return contents.get(0);
		}

		public boolean isEmpty() {
			return contents.size() == 0;
		}

		public int size() {
			return contents.size();
		}
	}

	// dummy term used to pass information about already created terms
	// between parseLine and getRoot
	protected OBOClassImpl dummyTerm = new OBOClassImpl("dummy");

	protected static final Vector scratchVector = new Vector();

	public final static char BOUNDARY_CHAR = '@';

	public final static HashMap IDMAPPINGS = new HashMap();

	protected List listeners = new Vector();

	protected ProgressableInputStream currentStream;

	protected void setProgressString(String progressString) {
		this.progressString = progressString;
	}

	public String getProgressString() {
		return progressString;
	}

	protected BufferedReader openReader(String filename) throws IOException {
		currentStream = IOUtil.getProgressableStream(filename);
		currentStream.setProgressMessage("Reading " + filename + "...");
		currentReader = new BufferedReader(new InputStreamReader(currentStream));
		return currentReader;

	}

	public static class GOFlatFileConfiguration extends
			FileAdapterConfiguration {
		protected String defFilename;

		protected String comment;

		protected boolean hideDownstream = true;

		protected boolean allowCycles = false;

		protected boolean allowDangling = false;

		protected boolean reduceSize = false;

		protected boolean useLegacyTypes = false;

		protected boolean translateTypes = false;

		protected List saveRecords = new ArrayList();

		protected String saveDefFilename;

		protected List typeMappings = new ArrayList();

		protected boolean basicSave = false;

		public GOFlatFileConfiguration() {
			/*
			 * typeMappings.add(new CharTypeMapping("%", "is_a", "is_a"));
			 * typeMappings.add(new CharTypeMapping("<", "part_of",
			 * "part_of"));
			 */
		}

		public boolean getBasicSave() {
			return basicSave;
		}

		public void setBasicSave(boolean basicSave) {
			this.basicSave = basicSave;
		}

		public void setTypeMappings(List typeMappings) {
			this.typeMappings = typeMappings;
		}

		public List getTypeMappings() {
			return typeMappings;
		}

		public void setSaveDefFilename(String saveDefFilename) {
			this.saveDefFilename = saveDefFilename;
		}

		public String getSaveDefFilename() {
			return saveDefFilename;
		}

		public void setSaveRecords(List saveRecords) {
			this.saveRecords = saveRecords;
		}

		public List getSaveRecords() {
			return saveRecords;
		}

		public void setTranslateTypes(boolean translateTypes) {
			this.translateTypes = translateTypes;
		}

		public boolean getTranslateTypes() {
			return translateTypes;
		}

		public void setUseLegacyTypes(boolean useLegacyTypes) {
			this.useLegacyTypes = useLegacyTypes;
		}

		public boolean getUseLegacyTypes() {
			return useLegacyTypes;
		}

		public void setReduceSize(boolean reduceSize) {
			this.reduceSize = reduceSize;
		}

		public boolean getReduceSize() {
			return reduceSize;
		}

		public void setAllowDangling(boolean allowDangling) {
			this.allowDangling = allowDangling;
		}

		public boolean getAllowDangling() {
			return allowDangling;
		}

		public void setAllowCycles(boolean allowCycles) {
			this.allowCycles = allowCycles;
		}

		public boolean getAllowCycles() {
			return allowCycles;
		}

		public void setHideDownstream(boolean hideDownstream) {
			this.hideDownstream = hideDownstream;
		}

		public boolean getHideDownstream() {
			return hideDownstream;
		}

		public void setComment(String comment) {
			this.comment = comment;
		}

		public String getComment() {
			return comment;
		}

		public void setDefFilename(String defFilename) {
			this.defFilename = defFilename;
		}

		public String getDefFilename() {
			return defFilename;
		}
	}

	public static class CharTypeMapping {
		protected String typeChar;

		protected String propertyID;

		protected String propertyName;

		public CharTypeMapping() {
		}

		public CharTypeMapping(String typeChar, String propertyID,
				String propertyName) {
			setTypeChar(typeChar);
			setPropertyID(propertyID);
			setPropertyName(propertyName);
		}

		@Override
		public String toString() {
			return propertyID;
		}

		public void setTypeChar(String typeChar) {
			this.typeChar = typeChar;
		}

		public String getTypeChar() {
			return typeChar;
		}

		public void setPropertyID(String propertyID) {
			this.propertyID = propertyID;
		}

		public String getPropertyID() {
			return propertyID;
		}

		public void setPropertyName(String propertyName) {
			this.propertyName = propertyName;
		}

		public String getPropertyName() {
			return propertyName;
		}
	}

	/*
	 * protected ProgressListener loadProgressListener = new ProgressListener() {
	 * 
	 * ReusableProgressEvent rpe = new ReusableProgressEvent(this);
	 * 
	 * public void progressMade(ProgressEvent pe) { int progressVal;
	 * rpe.setDescription(pe.getDescription());
	 * 
	 * if (pe instanceof ReusableProgressEvent) { progressVal =
	 * rpe.getFastVal(); } else { if (pe.getValue() == null) progressVal = -1;
	 * else progressVal = pe.getValue().intValue(); } int filecount = 1; if
	 * (config != null) filecount = config.getReadPaths().size() +
	 * (config.getDefFilename() != null ? 1 : 0); int newval = (progressVal +
	 * readfileindex * 100) / filecount; rpe.setFastVal(newval);
	 * fireProgressEvent(rpe); } };
	 */

	public Number getProgressValue() {
		if (progressValue == -1) {
			int filecount = 1;
			if (config != null)
				filecount = config.getReadPaths().size()
						+ (config.getDefFilename() != null ? 1 : 0);

			int newval = (currentStream.getProgressValue().intValue() + readfileindex * 100)
					/ filecount;
			return newval;
		} else
			return progressValue;
	}

	OBOSession history;

	protected static final Comparator idComparator = new Comparator() {
		public int compare(Object a, Object b) {
			String x = (String) a;
			String y = (String) b;
			return x.compareToIgnoreCase(y);
		}
	};

	protected static final Comparator dbxrefComparator = new Comparator() {
		public int compare(Object a, Object b) {
			return a.toString().compareToIgnoreCase(b.toString());
		}
	};

	protected static final Comparator relByChildComparator = new Comparator() {
		public int compare(Object a, Object b) {
			Link tra = (Link) a;
			Link trb = (Link) b;
			return ((OBOClass) tra.getChild()).compareTo(trb.getChild());
		}
	};

	protected static final Comparator relByParentComparator = new Comparator() {
		public int compare(Object a, Object b) {
			Link tra = (Link) a;
			Link trb = (Link) b;
			return ((OBOClass) tra.getParent()).compareTo(trb.getParent());
		}
	};

	static {
		Object junk = new Object();
		RESERVED_SYMBOLS.put(new Character(';'), junk);
		RESERVED_SYMBOLS.put(new Character('$'), junk);
		RESERVED_SYMBOLS.put(new Character(','), junk);
		RESERVED_SYMBOLS.put(new Character(':'), junk);
		RESERVED_SYMBOLS.put(new Character('!'), junk);
		RESERVED_SYMBOLS.put(new Character('\\'), junk);
		RESERVED_SYMBOLS.put(new Character('?'), junk);
		RESERVED_SYMBOLS.put(new Character(BOUNDARY_CHAR), junk);
		IDMAPPINGS.put("DEVELOPSFROM", "develops_from");
		IDMAPPINGS.put("DEVELOPS_FROM", "develops_from");
		IDMAPPINGS.put("ISA", "is_a");
		IDMAPPINGS.put("IS_A", "is_a");
		IDMAPPINGS.put("PARTOF", "part_of");
		IDMAPPINGS.put("PART_OF", "part_of");
	}

	public static class SaveRecord {
		protected String id;

		protected String filename;

		protected String defFilename;

		public SaveRecord() {
		}

		public SaveRecord(String id, String filename) {
			this.id = id;
			this.filename = filename;
		}

		public String getID() {
			return id;
		}

		public void setDefFilename(String defFilename) {
			this.defFilename = defFilename;
		}

		public String getDefFilename() {
			return defFilename;
		}

		public String getFilename() {
			return filename;
		}

		public void setID(String id) {
			this.id = id;
		}

		public void setFilename(String filename) {
			this.filename = filename;
		}

		public URL getURL() {
			return getURLForPath(filename);
		}

		@Override
		public String toString() {
			return "id = " + id + ", path = " + filename;
		}
	}

	public static boolean isReservedCharacter(char c) {
		return RESERVED_SYMBOLS.containsKey(new Character(c));
	}

	/*
	 * protected static Map getDefaultTypeBindings() { Map out = new HashMap();
	 * out.put("%", new OBOPropertyImpl("is_a", "is_a")); out.put("<", new
	 * OBOPropertyImpl("part_of", "part_of")); return out; }
	 * 
	 * protected static OBOProperty getDefaultLink() { return (OBOProperty)
	 * getDefaultTypeBindings(). get(new Character('%')); }
	 */
	Map charToType = new HashMap();

	Map typeToChar = new HashMap();

	protected String convertTypeToChar(OBOProperty trt) {
		return (String) typeToChar.get(trt);
	}

	protected OBOProperty convertCharToType(String chr) {
		OBOProperty out = (OBOProperty) charToType.get(chr);
		return out;
	}

	protected void initTypeMappings() {
		typeToChar.clear();
		charToType.clear();
	}

	protected void addTypeMapping(CharTypeMapping mapping) {
		String propID = mapping.getPropertyID();
		if (config.getTranslateTypes() && IDMAPPINGS.get(propID) != null)
			propID = (String) IDMAPPINGS.get(propID);

		OBOProperty prop = (OBOProperty) history.getObject(propID);
		if (prop == null) {
			prop = new OBOPropertyImpl(propID, mapping.getPropertyName());
			history.addObject(prop);
		}

		addTypeMapping(mapping.getTypeChar(), prop);

	}

	protected void addTypeMapping(String typeChar, OBOProperty prop) {
		if (typeChar.length() == 1 && tokenizer != null) {
			tokenizer.addKeeperTokenChar(typeChar.charAt(0));
		}

		charToType.put(typeChar, prop);
		typeToChar.put(prop, typeChar);
	}

	public GOFlatFileAdapter() {
		// setCharTypeMappings(getDefaultTypeBindings());
	}

	public void setAllowCycles(boolean allowCycles) {
		config.setAllowCycles(allowCycles);
	}

	public boolean getAllowCycles() {
		return config.getAllowCycles();
	}

	public void setAllowDangling(boolean allowDangling) {
		config.setAllowDangling(allowDangling);
	}

	public boolean getAllowDangling() {
		return config.getAllowDangling();
	}

	public String getTermText(OBOClass term) throws DataAdapterException {
		throw new DataAdapterException("Not supported");
	}

	public Set getTermCategories() throws DataAdapterException {
		return new HashSet(categories.values());
	}

	/*
	 * public void setCharTypeMappings(Map charToType) { this.charToType = new
	 * HashMap(charToType);
	 * 
	 * typeToChar = new HashMap();
	 * 
	 * Iterator e = charToType.keySet().iterator(); while(e.hasNext()) { Object
	 * key = e.next(); Object value = charToType.get(key); typeToChar.put(value,
	 * key); }
	 * 
	 * typeToChar.put(UNKNOWN_TYPE, "?"); }
	 * 
	 * public void setTypeCharMappings(Map typeToChar) { this.typeToChar = new
	 * HashMap(typeToChar); charToType = new HashMap(); Iterator e =
	 * typeToChar.keySet().iterator(); while(e.hasNext()) { Object key =
	 * e.next(); Object value = typeToChar.get(key); charToType.put(value, key); }
	 * 
	 * charToType.put("?", UNKNOWN_TYPE); }
	 */

	private class ParentageHolder {
		public String parentID;

		public String childID;

		public String line;

		public String name;

		public String type;

		public int lineNumber;

		public int colNumber;
	}

	public void setHideDownstream(boolean hide) {
		this.hideDownstream = hide;
	}

	public String[] getIDs(OBOSession history, OBOClass term, String prefix,
			int length, int count) throws DataAdapterException {
		throw new DataAdapterException("Not supported");
	}

	public String[] getIDs(OBOSession history, OBOClass term, String prefix,
			int min, int max, int length, int count)
			throws DataAdapterException {
		throw new DataAdapterException("Not supported");
	}

	public Vector getHistories() throws DataAdapterException {
		throw new DataAdapterException("Not supported");
	}

	public void setStrictParentage(boolean strict) {
		strictParentage = strict;
	}

	public void setStrictDefinition(boolean strict) {
		strictDefinition = strict;
	}

	public DataAdapterUI getPreferredUI() {
		FileAdapterUI ui = new FileAdapterUI() {
			@Override
			public AdapterConfiguration createEmptyConfig() {
				return new GOFlatFileConfiguration();
			}

			@Override
			public void setConfiguration(AdapterConfiguration c) {
				super.setConfiguration(c);
				if (c instanceof GOFlatFileConfiguration) {
					GOFlatFileConfiguration config = (GOFlatFileConfiguration) c;
					if (config.getDefFilename() != null) {
						String pathStr = (String) readField.getSelectedItem();
						pathStr += " '"
								+ escapePath(config.getDefFilename(), true)
								+ "'";
						readField.setSelectedItem(pathStr);
					}
				}
			}

			@Override
			public void acceptComponentConfig(boolean storeonly)
					throws DataAdapterUIException {
				super.acceptComponentConfig(storeonly);
				if (storeonly)
					return;

				readerCache.clear();
				Iterator it = ((FileAdapterConfiguration) config)
						.getReadPaths().iterator();
				boolean foundDefFile = false;
				while (it.hasNext()) {
					String s = (String) it.next();
					if (isDefinitionFile(s)) {
						if (foundDefFile)
							throw new DataAdapterUIException(
									"More than one definition "
											+ "file was specified.");
						it.remove();
						((GOFlatFileConfiguration) config).setDefFilename(s);
						foundDefFile = true;
					}
				}
				((GOFlatFileConfiguration) config).setBasicSave(true);
			}
		};
		ui.setReadOperation(READ_ONTOLOGY);
		ui.setWriteOperation(WRITE_ONTOLOGY);
		GraphicalUI advanced = getAdvancedUI();
		if (advanced != null) {
			ui.setAdvancedUI(advanced);
			advanced.setSimpleUI(ui);
		}

		return ui;
	}

	protected void setDefinition(IDWrapper id, Map terms, String definition,
			String comment, Vector refs,
			CompoundGOFlatFileParseException defExceptions, String filename,
			String startline, int startlineNumber) throws DataAdapterException {

		if (id == null) {
			defExceptions.addException(new GOFlatFileParseException(
					"No goid field in entry.", filename, startline,
					startlineNumber, 0));
			return;
		}
		if (definition == null) {
			defExceptions.addException(new GOFlatFileParseException(
					"No definition field in entry.", filename, startline,
					startlineNumber, 0));
			return;
		}

		if (refs.size() < 1) {
			defExceptions.addException(new GOFlatFileParseException(
					"No definition_reference field in entry.", filename,
					startline, startlineNumber, 0));
			return;
		}

		OBOClass term = (OBOClass) terms.get(id.toString());
		if (term != null) {
			term.setDefinition(definition);
			if (comment == null)
				term.setComment("");
			else
				term.setComment(comment);
			for (int i = 0; i < refs.size(); i++) {
				term.addDefDbxref((Dbxref) refs.get(i));
			}
		} else if (term == null) {
			lostDefs = true;
			if (strictDefinition) {
				defExceptions.addException(new GOFlatFileParseException(
						"Reference to non-existant GO id " + id.toString(),
						filename, startline, startlineNumber, 0));
			}
		}
	}

	protected int unescapedIndex(String str, char findme) {
		for (int i = 0; i < str.length(); i++) {
			char c = str.charAt(i);
			if (c == '\\') {
				i++;
				continue;
			} else if (c == findme)
				return i;
		}
		return -1;
	}

	public void populateDefinitions(Map allterms, String filename)
			throws DataAdapterException {
		try {
			BufferedReader reader = openReader(filename);

			CompoundGOFlatFileParseException defExceptions = new CompoundGOFlatFileParseException(
					hideDownstream);
			int lineNumber = 0;
			IDWrapper goid = null;
			String term = null;
			String def = null;
			String comment = null;
			Vector references = new Vector();
			boolean inDefinition = false;
			int startlineNumber = -1;
			String startline = null;
			setProgressString("Parsing definitions...");
			while (true) {
				String line = reader.readLine();
				lineNumber++;

				if (cancelled)
					throw new CancelledAdapterException();

				if (line == null) {
					if (!(goid == null || term == null || def == null || references
							.size() == 0))
						setDefinition(goid, allterms, unescapeDefText(def),
								unescapeDefText(comment), references,
								defExceptions, filename, startline,
								startlineNumber);
					break;
				}

				if (line.trim().equals("")) {
					if (!(goid == null || term == null || def == null || references
							.size() == 0)) {
						setDefinition(goid, allterms, unescapeDefText(def),
								unescapeDefText(comment), references,
								defExceptions, filename, startline,
								startlineNumber);
					}
					goid = null;
					term = null;
					def = null;
					comment = null;
					references = new Vector();
					startlineNumber = -1;
					startline = null;
					inDefinition = false;
				}

				if (line.trim().length() == 0 || line.charAt(0) == '!') {
					continue;
				}

				if (startline == null) {
					startline = line;
					startlineNumber = lineNumber;
				}

				int start = findStartOfDefFieldData(line);
				if (start == -1) {
					if (inDefinition)
						def += " " + line;
					else {
						defExceptions
								.addException(new GOFlatFileParseException(
										"No type field found, and line is not a "
												+ "continuation of definition text.",
										filename, line, lineNumber, 0));
					}
				} else {
					String key = line.substring(0, start - 2);
					String value = line.substring(start);
					if (key.equals("definition")) {
						inDefinition = true;
						if (def == null) {
							def = value;
						} else
							defExceptions
									.addException(new GOFlatFileParseException(
											"Multiple definition fields for one entry.",
											filename, line, lineNumber, 0));
					} else if (key.equals("comment")) {
						inDefinition = false;
						if (comment == null) {
							comment = value;
						} else
							defExceptions
									.addException(new GOFlatFileParseException(
											"Multiple comment fields for one entry.",
											filename, line, lineNumber, 0));
					} else if (key.equals("goid") || key.equals("id")) {
						inDefinition = false;
						if (goid == null) {
							try {
								Queue tokenList = getQueueForLine(filename
										.toString(), line, lineNumber, null);
								// discard goid type tag
								tokenList.dequeue();
								// discard colon
								tokenList.dequeue();
								goid = pullOffID(tokenList, exceptionHolder);
							} catch (NoSuchElementException e) {
								GOFlatFileParseException ex = new GOFlatFileParseException(
										"Unexpected end of line ", filename,
										line, lineNumber, line.length());
								defExceptions.addException(ex);
							}
						} else
							defExceptions
									.addException(new GOFlatFileParseException(
											"Multiple id fields for one entry.",
											filename, line, lineNumber, 0));
					} else if (key.equals("term")) {
						inDefinition = false;
						if (term == null) {
							term = value;
						} else
							defExceptions
									.addException(new GOFlatFileParseException(
											"Multiple term name fields for one entry.",
											filename, line, lineNumber, 0));
					} else if (key.equals("definition_reference")) {
						inDefinition = false;

						int index = unescapedIndex(value, ':');
						String dbname;
						String dbid;
						if (index == -1) {
							dbname = "";
							dbid = value;
						} else {
							dbname = value.substring(0, index);
							dbid = value.substring(index + 1, value.length());
						}

						Dbxref ref = new DbxrefImpl(dbname, dbid,
								Dbxref.DEFINITION);
						if (!references.contains(ref))
							references.addElement(ref);
					} else {
						defExceptions
								.addException(new GOFlatFileParseException(
										"Unrecognized type field \"" + key
												+ "\"", filename, line,
										lineNumber, 0));
					}
				}
			}
			if (!defExceptions.isEmpty())
				throw defExceptions;
		} catch (FileNotFoundException e) {
			throw new DataAdapterException("Cannot find " + filename);
		} catch (IOException e) {
			if (cancelled)
				throw new CancelledAdapterException();
			else
				throw new DataAdapterException("File unreadable", e);
		}
	}

	private int findStartOfDefFieldData(String in) {
		int firstSpace = in.indexOf(" ");
		if (firstSpace < 1 || in.charAt(firstSpace - 1) != ':')
			return -1;
		else
			return firstSpace + 1;
	}

	public void init() {
		// do nothing
	}

	protected static URL getURLForPath(String path) {
		URL url = null;
		try {
			url = new URL(path);
		} catch (MalformedURLException e) {
			try {
				url = new URL("file:" + path);
			} catch (MalformedURLException ex) {
			}
		}
		return url;
	}

	/*
	 * public void setDefinitionPath(String defFilename) { if (defFilename !=
	 * null && defFilename.length() > 0) this.defFilename =
	 * getURLForPath(defFilename); else this.defFilename = null; }
	 */
	public OBOClass importTerms(OBOClass root, boolean stripIds)
			throws DataAdapterException {
		OBOClass term;
		if (stripIds) {
			term = getRoot(new HashMap());
			wipeOutIDs(term);
		} else {
			Map map = TermUtil.createIDMap(TermUtil.getDescendants(root, true));
			term = getRoot(map);
		}
		return term;
	}

	private void wipeOutIDs(OBOClass term) {
		// term.setID(null);
		Iterator it = term.getChildren().iterator();
		while (it.hasNext()) {
			Link tr = (Link) it.next();
			wipeOutIDs((OBOClass) tr.getChild());
		}
	}

	protected GOFlatFileTokenizer tokenizer;

	protected String username = null;

	protected String autogen;

	public AdapterConfiguration getConfiguration() {
		return config;
	}

	public Object doOperation(IOOperation op, AdapterConfiguration config,
			Object input) throws DataAdapterException {
		if (!(config instanceof GOFlatFileConfiguration)) {
			throw new DataAdapterException("Adapter requires a "
					+ "GOFlatFileConfiguration " + "object.");
		}
		cancelled = false;
		this.config = (GOFlatFileConfiguration) config;
		if (op.equals(OBOAdapter.READ_ONTOLOGY)) {
			OBOSession session = getRoot();
			return session;
		} else if (op.equals(OBOAdapter.WRITE_ONTOLOGY)) {
			return write((OBOSession) input);
		} else
			throw new DataAdapterException("Unsupported operation " + op);
	}

	public boolean isDefinitionFile(String filename) {
		try {
			BufferedReader reader = openReader(filename);
			reader.mark(10000);
			String currentLine;
			while ((currentLine = reader.readLine()) != null) {
				if (currentLine.trim().length() == 0
						|| currentLine.charAt(0) == '!')
					continue;
				reader.reset();
				if (currentLine.startsWith("term:")
						|| currentLine.startsWith("goid:")
						|| currentLine.startsWith("id:")
						|| currentLine.startsWith("comment:")
						|| currentLine.startsWith("definition:")
						|| currentLine.startsWith("definition_reference:")) {
					return true;
				} else {
					return false;
				}
			}
			reader.reset();
			return false;
		} catch (Exception ex) {
			ex.printStackTrace();
			return false;
		}
	}

	public OBOSession getRoot() throws DataAdapterException {
		categories.clear();
		namespaceHash.clear();
		initTypeMappings();

		history = new OBOSessionImpl();

		// clear here
		lostDefs = false;
		Map table = new HashMap();

		tokenizer = new GOFlatFileTokenizer();
		tokenizer.addTokenChar(' ');
		tokenizer.addTokenChar('\t');
		tokenizer.addKeeperTokenChar('$');
		tokenizer.addKeeperTokenChar(';');
		tokenizer.addKeeperTokenChar(',');
		tokenizer.addKeeperTokenChar(':');
		// go through the type mappings here
		Iterator it = config.getTypeMappings().iterator();
		while (it.hasNext()) {
			CharTypeMapping ctm = (CharTypeMapping) it.next();
			addTypeMapping(ctm);
		}

		tokenizer.addBoundaryChar(BOUNDARY_CHAR);

		nsHash = new HashMap();
		OBOClass root = getRoot(table);

		readerCache.clear();

		history.setDefaultNamespace(root.getNamespace());

		it = TermUtil.getDescendants(root, true).iterator();
		while (it.hasNext()) {
			history.addObject((IdentifiedObject) it.next());
		}

		history.getCurrentHistory().setUser(System.getProperty("user.name"));
		history.getCurrentHistory().setDate(new Date());
		history.getCurrentHistory().setComment(config.getComment());
		history.getCategories().addAll(getTermCategories());

		it = nsHash.values().iterator();
		while (it.hasNext()) {
			Namespace ns = (Namespace) it.next();
			history.addNamespace(ns);
		}

		history.setLoadRemark(createLoadRemark());
		return history;
	}

	protected String createLoadRemark() {
		StringBuffer out = new StringBuffer();
		Iterator it = config.getReadPaths().iterator();
		boolean first = true;
		while (it.hasNext()) {
			String filename = (String) it.next();
			if (!first)
				out.append(", ");
			first = true;
			out.append(filename);
		}
		return "GO File from : " + out.toString();
	}

	public boolean getLostDefs() {
		return lostDefs;
	}

	public synchronized OBOClass getRoot(Map allterms)
			throws DataAdapterException {
		OBOClass root = null;
		String rootfile = null;
		Iterator it = config.getReadPaths().iterator();
		for (readfileindex = 0; it.hasNext(); readfileindex++) {
			String filename = (String) it.next();
			exceptionHolder = new CompoundGOFlatFileParseException(
					hideDownstream);
			parentageList = new Vector();
			Map termHash = new HashMap();

			OBOClass term = getTerms(allterms, filename);
			if (cancelled)
				throw new CancelledAdapterException();
			checkParentage(allterms, parentageList, filename);
			if (root == null) {
				root = term;
				rootfile = filename;
			} else {

				if (!term.getName().equals(root.getName())
						|| !term.getID().equals(root.getID()))
					throw new DataAdapterException(rootfile + " and "
							+ filename + " have different root " + "terms!");
			}
			// allterms.putAll(termHash);
		}
		if (config.getDefFilename() != null) {
			populateDefinitions(allterms, config.getDefFilename());
			readfileindex++;
		}

		parentageList.removeAllElements();
		return root;
	}

	protected static void println(PrintStream stream) {
		println(stream, "");
	}

	protected static void println(PrintStream stream, String string) {
		stream.print(string + "\n");
	}

	public void checkParentage(Map allterms, Vector parentageList,
			String filename) throws DataAdapterException {
		Map parentCounter = new HashMap();
		CompoundGOFlatFileParseException parentageErrors = new CompoundGOFlatFileParseException(
				hideDownstream);
		for (int i = 0; i < parentageList.size(); i++) {

			if (cancelled)
				throw new CancelledAdapterException();

			ParentageHolder ph = (ParentageHolder) parentageList.get(i);

			OBOClass term = (OBOClass) allterms.get(ph.childID);
			OBOClass parent = (OBOClass) allterms.get(ph.parentID);

			OBOProperty type;
			type = convertCharToType(ph.type);

			if (type == null) {
				type = FAILED_TYPE;
				parentageErrors.addException(new GOFlatFileParseException(
						"Unrecognized parentage character \"" + ph.type + "\"",
						filename, ph.line, ph.lineNumber, ph.colNumber));
			}

			if (parent == null) {
				parentageErrors.addException(new GOFlatFileParseException(
						"No term exists with id " + ph.parentID, filename,
						ph.line, ph.lineNumber, ph.colNumber));
			} else {
				Vector trs = getTRsForParentWithID(term, ph.parentID);
				if (trs.size() != 0) {
					if (strictParentage) {
						parentageErrors
								.addException(new GOFlatFileParseException(
										ph.parentID
												+ " is listed as parent of "
												+ ph.childID + ", " + "but "
												+ ph.childID
												+ " does not appear in "
												+ "the file as a child of "
												+ ph.parentID, filename,
										ph.line, ph.lineNumber, ph.colNumber));
					} else {
						Link tr = new OBORestrictionImpl(term, parent, type);
						// if (!relationshipTypes.contains(type))
						// relationshipTypes.add(type);
						parent.addChild(tr);
					}
				}

				/*
				 * // check for type agreement if (tr != null &&
				 * !tr.getType().equals(type)) { parentageErrors.addException(
				 * new GOFlatFileParseException( "The parentage relationship
				 * given contradicts an "+ "earlier statement of this
				 * relationship.", filename, ph.line, ph.lineNumber,
				 * ph.colNumber)); }
				 */
				// check for cycles
				// if (!allowCycles && contains(term, parent)) {
				if (!config.getAllowCycles()
						&& TermUtil.hasAncestor(parent, term)) {
					parentageErrors.addException(new GOFlatFileParseException(
							"This relationship would create a cycle.",
							filename, ph.line, ph.lineNumber, ph.colNumber));
					throw parentageErrors;
				}
			}
		}
		if (!parentageErrors.isEmpty())
			throw parentageErrors;

	} // end checkParentage()

	protected static boolean contains(OBOClass a, OBOClass b) {
		return contains(a, b, new HashMap());
	}

	private static boolean contains(OBOClass a, OBOClass b, Map lookedAt) {
		if (a == b || lookedAt.containsKey(a))
			return true;
		lookedAt.put(a, a);
		Iterator e = a.getChildren().iterator();
		while (e.hasNext()) {
			Link tr = (Link) e.next();
			OBOClass child = (OBOClass) tr.getChild();
			try {
				if (contains(child, b, lookedAt)) {
					return true;
				}
			} catch (Throwable ex) {
				System.err.println("got " + ex);
				System.exit(1);
			}
		}
		return false;
	}

	public Vector getTRsForParentWithID(OBOClass child, String id) {
		Vector out = new Vector();
		Vector parents = new Vector(child.getParents());
		for (int i = 0; i < parents.size(); i++) {
			Link tr = (Link) parents.get(i);
			Vector ids = new Vector();
			ids.addAll(((AnnotatedObject) tr.getParent()).getSecondaryIDs());
			ids.add(tr.getParent().getID());
			for (int j = 0; j < ids.size(); j++) {
				String pid = (String) ids.get(j);
				if (tr.getParent().getID().equals(pid)) {
					out.add(tr);
					break;
				}
			}
		}
		return out;
	}

	public OBOClass findTermInHash(OBOClass term, Map hash) {
		if (hash.containsKey(term.getID())) {
			return (OBOClass) hash.get(term.getID());
		}
		/*
		 * Vector synonyms = term.getSynonyms(); for(int i=0; i <
		 * synonyms.size(); i++) { Synonym s = (Synonym) synonyms.get(i); if
		 * (s.getID() != null && hash.containsKey(s.getID())) { return
		 * (OBOClass) hash.get(s.getID()); } }
		 */
		return null;
	}

	public void putTermInHash(OBOClass term, Map hash) {
		hash.put(term.getID(), term);
		/*
		 * Vector synonyms = term.getSynonyms(); for(int i=0; i <
		 * synonyms.size(); i++) { Synonym s = (Synonym) synonyms.get(i); if
		 * (s.getID() != null) hash.put(s.getID(), term); }
		 */
	}

	// make put term in hash function too

	public OBOClass getTerms(Map allterms, String filename)
			throws DataAdapterException {
		OBOClass oldTerm = null;
		root = null;
		termStack = new Stack();
		int currentDepth = 0;
		int lineNumber = 0;
		String currentLine = null;
		boolean inputStarted = false;
		String leaderComment = null;
		try {
			BufferedReader reader = openReader(filename);

			File file = new File(filename);
			String nsName = file.getName();
			String shortfilename = file.getName();
			file = file.getParentFile();

			// generate a short namespace name for this file
			for (; file != null; file = file.getParentFile()) {
				if (!nsHash.containsKey(nsName.toString()))
					break;
				else
					nsName = file.getName() + "/" + nsName;
			}
			if (file == null && nsHash.containsKey(nsName)) {
				int i = 1;
				for (; nsHash.containsKey(nsName + "_" + i); i++) {
				}
				nsName = nsName + "_" + i;
			}

			Namespace ns = new Namespace(nsName, filename);
			nsHash.put(nsName, ns);
			setProgressString("Parsing " + shortfilename + "...");
			long loopstart = System.currentTimeMillis();
			while ((currentLine = reader.readLine()) != null) {
				lineNumber++;

				if (currentLine.trim().length() == 0) {
					GOFlatFileParseException ex = new GOFlatFileParseException(
							"Blank lines are not allowed", filename,
							currentLine, lineNumber, 1);
					exceptionHolder.addException(ex);
					continue;
				}
				if (isComment(currentLine)) {
					if (!inputStarted) {
						String commentLine = currentLine.substring(1);
						if (commentLine.startsWith("type:")) {
							commentLine = commentLine.substring(5).trim();
							StringTokenizer stringTokenizer = new StringTokenizer(
									commentLine);
							try {
								String typeChar = stringTokenizer.nextToken();
								String typeName = stringTokenizer.nextToken();
								String typeDesc = typeName;
								boolean first = true;
								while (stringTokenizer.hasMoreTokens()) {
									if (!first) {
										typeDesc = typeDesc + " "
												+ stringTokenizer.nextToken();
									} else {
										first = false;
										typeDesc = stringTokenizer.nextToken();
									}
								}

								addTypeMapping(new CharTypeMapping(typeChar,
										typeName, typeDesc));

								/*
								 * 
								 * OBOProperty trt = new
								 * OBOPropertyImpl(typeName, typeDesc); if
								 * (!relationshipTypes.contains(trt)) {
								 * relationshipTypes.add(trt);
								 * charToType.put(typeChar, trt);
								 * typeToChar.put(trt, typeChar); if
								 * (typeChar.length() == 1) { tokenizer.
								 * addKeeperTokenChar(typeChar. charAt(0)); } }
								 */

							} catch (NoSuchElementException ex) {
							}
						} else if (leaderComment == null
								|| leaderComment.length() == 0) {
							leaderComment = commentLine;
						} else
							leaderComment += "\n" + commentLine;
					}
					continue;
				} else
					inputStarted = true;

				int oldDepth = currentDepth;
				currentDepth = countLeadingSpaces(currentLine);

				OBOClass term = parseLine(filename, currentLine, lineNumber,
						allterms);
				if (root == null && currentDepth > 0) {
					GOFlatFileParseException ex = new GOFlatFileParseException(
							"Bad indentation. The first line of a file must "
									+ "not be indented. root = " + root
									+ ", currentDepth = " + currentDepth,
							filename, currentLine, lineNumber, 1);
					exceptionHolder.addException(ex);
					throw exceptionHolder;
				} else if (root == null && !currentType.equals("$")) {
					GOFlatFileParseException ex = new GOFlatFileParseException(
							"Bad root symbol. The root term must always be "
									+ "listed with the $ symbol.", filename,
							currentLine, lineNumber, 1);
					exceptionHolder.addException(ex);
					throw exceptionHolder;
				}
				// quit if we've reached an end of file marker
				if (term == null)
					break;

				OBOClass found = (OBOClass) allterms.get(term.getID());

				// check to see if synonyms are found too

				if (found != null) {
					if (!found.getName().equals(term.getName())) {
						GOFlatFileParseException ex = new GOFlatFileParseException(
								"Misspelled term name or illegal reuse of a"
										+ " GO id. This term was previously defined with "
										+ "the name \"" + found.getName()
										+ "\", but new name is \""
										+ term.getName() + "\".", filename,
								currentLine, lineNumber, 1);
						exceptionHolder.addException(ex);
					}
					term = found;
				} else {
					putTermInHash(term, allterms);
				}
				term.setNamespace(ns);

				if (oldDepth > currentDepth) {
					for (int i = 0; i < oldDepth - currentDepth; i++)
						termStack.pop();
				} else if (oldDepth < currentDepth) {
					// structural problem; exception must be thrown immediately
					if (currentDepth - oldDepth != 1) {
						GOFlatFileParseException ex = new GOFlatFileParseException(
								"Bad indentation. No line may be greater than 1 "
										+ "space deeper than its parent.",
								filename, currentLine, lineNumber, 1);
						exceptionHolder.addException(ex);
						throw exceptionHolder;
					}
					termStack.push(oldTerm);
				}
				if (termStack.size() > 0) {
					OBOClass parent = (OBOClass) termStack.peek();

					if (!config.getAllowCycles()
							&& TermUtil.hasAncestor(parent, term)) {
						GOFlatFileParseException ex = new GOFlatFileParseException(
								"Attempt to make "
										+ term.getID()
										+ " an "
										+ "ancestor of itself. This could be a "
										+ "structural error, or it could be caused "
										+ "by the improper reuse of a go id.",
								filename, currentLine, lineNumber, 1);
						exceptionHolder.addException(ex);
						throw exceptionHolder;
					}

					OBOProperty relType = convertCharToType(currentType);
					if (relType == null) {
						relType = FAILED_TYPE;
						GOFlatFileParseException ex = new GOFlatFileParseException(
								"Unrecognized type character \"" + currentType
										+ "\"", filename, currentLine,
								lineNumber, 1);
						exceptionHolder.addException(ex);
					}
					/*
					 * else { if (!relationshipTypes.contains(relType))
					 * relationshipTypes.add(relType); }
					 */
					Link newTR = new OBORestrictionImpl(term, relType, parent);
					term.addParent(newTR);
				} else {

					if (root == null) {
						root = term;
						// root.setRoot(true);
					} else {
						// structural problem
						if (currentDepth - oldDepth != 1) {
							GOFlatFileParseException ex = new GOFlatFileParseException(
									"Attempt to assign second root term.",
									filename, currentLine, lineNumber, 1);
							exceptionHolder.addException(ex);
							throw exceptionHolder;
						}
					}
				}
				oldTerm = term;
			}

			// discard trailing whitespace
			while ((currentLine = reader.readLine()) != null) {
				if (currentLine.trim().length() > 0) {
					GOFlatFileParseException ex = new GOFlatFileParseException(
							"Data after end of file token found.", filename,
							currentLine, lineNumber, 1);
					exceptionHolder.addException(ex);
					throw exceptionHolder;
				}
			}
			if (root == null) {
				GOFlatFileParseException ex = new GOFlatFileParseException(
						"File contains no data.", filename, "", 0, 1);
				exceptionHolder.addException(ex);
			}
			if (!exceptionHolder.isEmpty())
				throw exceptionHolder;

			if (leaderComment != null && leaderComment.length() > 0)
				history.getCurrentHistory().setComment(leaderComment);
			return root;
		} catch (NoSuchElementException e) {
			GOFlatFileParseException ex = new GOFlatFileParseException(
					"Unexpected end of line during term definition."
							+ " (There may be other errors after this one).",
					filename, currentLine, lineNumber, currentLine.length());
			exceptionHolder.addException(ex);
			throw exceptionHolder;
		} catch (FileNotFoundException e) {
			throw new DataAdapterException("Cannot find " + filename);
		} catch (IOException e) {
			if (cancelled)
				throw new CancelledAdapterException();
			else
				throw new DataAdapterException("File unreadable", e);
		}
	}

	protected boolean isComment(String in) {
		return in.charAt(0) == '!';
	}

	protected OBOClass parseLine(String filename, String line, int lineNum,
			Map allterms) throws GOFlatFileParseException {
		Queue tokenList = getQueueForLine(filename, line, lineNum, allterms);
		if (((GOToken) tokenList.peek()).getToken().equals("$")
				&& tokenList.size() == 1)
			return null;
		OBOClass t = getTermFromTokens(tokenList, filename, line, lineNum,
				allterms);
		return t;
	}

	protected OBOClass getTermFromTokens(Queue tokens, String filename,
			String line, int lineNum, Map allterms)
			throws GOFlatFileParseException {
		currentType = pullOffType(tokens);

		if (tokens.size() == 0) {
			GOFlatFileParseException ex = new GOFlatFileParseException(
					"Expected term name, found nothing.", filename, line,
					lineNum, 0);
			exceptionHolder.addException(ex);
			return new OBOClassImpl(null);
		}

		GOToken toptoken = (GOToken) tokens.peek();

		String currentLine = toptoken.getLine();
		String currentFilename = toptoken.getFilename();
		int currentLineNumber = toptoken.getLineNumber();

		String name = pullOffTerm(tokens);

		// discard semicolon
		GOToken semicolonToken = (GOToken) tokens.dequeue();
		if (!semicolonToken.getToken().equals(";")) {
			GOFlatFileParseException ex = new GOFlatFileParseException(
					"Expected \";\" instead of " + "\""
							+ semicolonToken.getToken() + "\"", semicolonToken
							.getFilename(), semicolonToken.getLine(),
					semicolonToken.getLineNumber(), semicolonToken
							.getColNumber());
			exceptionHolder.addException(ex);
		}

		IDWrapper id = pullOffID(tokens, exceptionHolder);
		OBOClassImpl term = (OBOClassImpl) allterms.get(id.toString());
		if (term != null) {
			dummyTerm.setName(name);
			dummyTerm.setID(id.toString());
			return dummyTerm;
		}
		term = new OBOClassImpl(name, id.toString());
		try {
			Vector altids = pullOffIDList(tokens);
			term.getSecondaryIDs().addAll(altids);
			Vector dbxrefs = new Vector();
			Vector synonyms = new Vector();
			while (!tokens.isEmpty()) {
				GOToken token = (GOToken) tokens.dequeue();
				if (convertCharToType(token.getToken()) != null) {
					discardParentageTokens(token, id.toString(), tokens);
				} else if (token.getToken().equals(";")) {
					GOToken keytoken = (GOToken) tokens.dequeue();
					String key = keytoken.getToken();

					token = (GOToken) tokens.dequeue();
					if (!token.getToken().equals(":")) {
						GOFlatFileParseException ex = new GOFlatFileParseException(
								"Unexpected seperator \"" + token.getToken()
										+ "\" found. Expected \":\"", token
										.getFilename(), token.getLine(), token
										.getLineNumber(), token.getColNumber());
						exceptionHolder.addException(ex);
					}

					String value = pullOffValues(tokens);
					if (key.equals("synonym")) {
						synonyms.add(new SynonymImpl(value));
					} else {
						dbxrefs.add(new DbxrefImpl(key, value, Dbxref.ANALOG));
					}
					/*
					 * else { GOFlatFileParseException ex = new
					 * GOFlatFileParseException( "Unexpected property
					 * \""+key+"\" found. "+ "Expected \"synonym\", \"TC\", or
					 * \"EC\".", keytoken.getFilename(), keytoken.getLine(),
					 * keytoken.getLineNumber(), keytoken.getColNumber());
					 * exceptionHolder.addException(ex); }
					 */
				} else {
					GOFlatFileParseException ex = new GOFlatFileParseException(
							"Unexpected character " + token.getToken(), token
									.getFilename(), token.getLine(), token
									.getLineNumber(), token.getColNumber());
					exceptionHolder.addException(ex);
				}
			}
			term.getSynonyms().addAll(synonyms);
			term.getDbxrefs().addAll(dbxrefs);
		} catch (NoSuchElementException e) {
			GOFlatFileParseException ex = new GOFlatFileParseException(
					"Unexpected end of line ", currentFilename, currentLine,
					currentLineNumber, currentLine.length());
			exceptionHolder.addException(ex);
		}
		return term;
	}

	protected Dbxref decodeDbxref(GOToken keyToken, String dbxrefString)
			throws GOFlatFileParseException {
		int colonloc = dbxrefString.indexOf(":");
		if (colonloc == -1) {
			throw new GOFlatFileParseException("Expected : in dbxref "
					+ "definition", keyToken.getFilename(), keyToken.getLine(),
					keyToken.getLineNumber(), keyToken.getColNumber());
		}
		int startQuote = dbxrefString.indexOf("\"", colonloc);
		if (startQuote == -1) {
			throw new GOFlatFileParseException("Expected open \" in dbxref "
					+ "definition", keyToken.getFilename(), keyToken.getLine(),
					keyToken.getLineNumber(), keyToken.getColNumber());
		}

		int endQuote = dbxrefString.lastIndexOf("\"");
		if (startQuote == -1) {
			throw new GOFlatFileParseException("Expected closing \" in "
					+ "dbxref definition", keyToken.getFilename(), keyToken
					.getLine(), keyToken.getLineNumber(), keyToken
					.getColNumber());
		}

		String key = dbxrefString.substring(0, colonloc).trim();
		String val = dbxrefString.substring(colonloc + 1, startQuote - 1)
				.trim();
		String desc = dbxrefString.substring(startQuote + 1, endQuote).trim();
		String typeStr = dbxrefString.substring(endQuote + 1,
				dbxrefString.length()).trim();
		int type;
		if (typeStr.equals("ANT"))
			type = Dbxref.ANATOMICAL;
		else if (typeStr.equals("SYN"))
			type = Dbxref.RELATED_SYNONYM;
		else if (typeStr.equals("DEF"))
			type = Dbxref.DEFINITION;
		else if (typeStr.equals("ANA"))
			type = Dbxref.ANALOG;
		else if (typeStr.equals("UNK"))
			type = Dbxref.UNKNOWN;
		else {
			throw new GOFlatFileParseException("Expected dbxref type tag "
					+ " (ANT, ANA, SYN, DEF, UNK) "
					+ "in dbxref definition; found " + typeStr, keyToken
					.getFilename(), keyToken.getLine(), keyToken
					.getLineNumber(), keyToken.getColNumber());
		}
		Dbxref dbxref = new DbxrefImpl(key, val, desc, type);
		return dbxref;
	}

	protected String pullOffValues(Queue tokens) {
		GOToken token = (GOToken) tokens.dequeue();
		String value = token.getToken();
		while (!tokens.isEmpty()
				&& !((GOToken) tokens.peek()).getToken().equals(";")
				&& ((GOToken) tokens.peek()).getToken().charAt(0) != BOUNDARY_CHAR
				&& !((((GOToken) tokens.peek()).getToken().length() == 1) && (convertCharToType(((GOToken) tokens
						.peek()).getToken()) != null))) {
			GOToken currentToken = (GOToken) tokens.dequeue();
			if (!currentToken.isFlush())
				value += " ";
			value += currentToken.getToken();
		}
		return value;
	}

	protected void discardParentageTokens(GOToken typeToken, String id,
			Queue tokens) throws GOFlatFileParseException {
		// discard parent name

		String name = pullOffTerm(tokens);

		// make sure parent name and id are seperated with a semicolon
		if (tokens.size() < 1) {
			GOFlatFileParseException ex = new GOFlatFileParseException(
					"Expected ; ", typeToken.getFilename(),
					typeToken.getLine(), typeToken.getLineNumber(), typeToken
							.getColNumber());
			exceptionHolder.addException(ex);
			return;
		}
		GOToken token = (GOToken) tokens.dequeue();
		if (!token.getToken().equals(";")) {
			GOFlatFileParseException ex = new GOFlatFileParseException(
					"Expected ; instead of " + token.getToken(), token
							.getFilename(), token.getLine(), token
							.getLineNumber(), token.getColNumber());
			exceptionHolder.addException(ex);
		} else {
			// get id and record it
			String parentid = pullOffID(tokens, exceptionHolder).toString();
			ParentageHolder ph = new ParentageHolder();
			ph.parentID = parentid;
			ph.childID = id;
			ph.name = name;
			ph.line = typeToken.getLine();
			ph.lineNumber = typeToken.getLineNumber();
			ph.colNumber = typeToken.getColNumber();
			ph.type = typeToken.getToken();
			parentageList.addElement(ph);
		}
	}

	protected Vector pullOffIDList(Queue tokens)
			throws GOFlatFileParseException {
		Vector ids = new Vector();
		while (!tokens.isEmpty()
				&& ((GOToken) tokens.peek()).getToken().equals(",")) {
			// discard comma
			tokens.dequeue();
			ids.add(pullOffID(tokens, exceptionHolder).toString());
		}
		return ids;
	}

	protected String pullOffType(Queue tokens) throws GOFlatFileParseException {
		GOToken token = (GOToken) tokens.dequeue();
		String type = token.getToken();
		if (!(type.equals("$") || convertCharToType(type) != null)) {
			GOFlatFileParseException ex = new GOFlatFileParseException(
					"Unrecognized type character '" + type
							+ "' found. condition 1", token.getFilename(),
					token.getLine(), token.getLineNumber(), token
							.getColNumber());
			exceptionHolder.addException(ex);
			type = "";
		}
		return type;
	}

	protected String pullOffTerm(Queue tokens) {
		String name = ((GOToken) tokens.dequeue()).getToken();
		while (!tokens.isEmpty()
				&& !((GOToken) tokens.peek()).getToken().equals(";")
				&& convertCharToType(((GOToken) tokens.peek()).getToken()) == null) {
			GOToken token = (GOToken) tokens.dequeue();
			if (!token.isFlush())
				name += " ";
			name += token.getToken();
		}
		return name;
	}

	protected class IDWrapper {
		String prefix;

		String id;

		public IDWrapper(String prefix, String id) {
			this.prefix = prefix;
			this.id = id;
		}

		public String getPrefix() {
			return prefix;
		}

		public String getID() {
			return id;
		}

		@Override
		public String toString() {
			return prefix + ":" + id;
		}
	}

	protected boolean isNextItemID(Queue tokens) {
		if (tokens.size() < 3)
			return false;
		String prefix = ((GOToken) tokens.peekAt(0)).getToken();
		GOToken token = (GOToken) tokens.peekAt(1);
		if (!token.getToken().equals(":"))
			return false;
		token = (GOToken) tokens.peekAt(2);
		try {
			Integer.parseInt(token.getToken());
		} catch (NumberFormatException e) {
			return false;
		}
		return true;
	}

	protected IDWrapper pullOffID(Queue tokens,
			CompoundGOFlatFileParseException exList)
			throws GOFlatFileParseException {

		GOToken token;

		// get prefix
		String prefix = ((GOToken) tokens.dequeue()).getToken();

		// make sure GO seperator char is correct
		token = (GOToken) tokens.dequeue();
		if (!token.getToken().equals(":")) {
			GOFlatFileParseException ex = new GOFlatFileParseException(
					"Unexpected GO id seperator " + token.getToken()
							+ " found. Expected :", token.getFilename(), token
							.getLine(), token.getLineNumber(), token
							.getColNumber());
			exList.addException(ex);
		}

		// make sure GO id is the correct length
		token = (GOToken) tokens.dequeue();

		return new IDWrapper(prefix, token.getToken());
	}

	protected Queue getQueueForLine(String filename, String in, int lineNum,
			Map allterms) throws GOFlatFileParseException {
		Queue queue = new Queue();
		tokenizer.setStrings(filename, in, lineNum);

		GOToken token;
		while ((token = tokenizer.getNextToken()) != null) {
			queue.enqueue(token);
		}
		return queue;
	}

	protected int countLeadingSpaces(String line) {
		for (int i = 0; i < line.length(); i++) {
			if (line.charAt(i) != ' ')
				return i;
		}
		return line.length();
	}

	public String getID() {
		return "OBO:GOFlatFileAdapter";
	}

	public String getName() {
		return "GO Flat File Adapter";
	}

	public IOOperation[] getSupportedOperations() {
		IOOperation[] supported = { OBOAdapter.READ_ONTOLOGY,
				OBOAdapter.WRITE_ONTOLOGY };

		return supported;
	}

	public void init(Object params) throws DataAdapterException {
	}

	public OBOSession write(OBOSession history) throws DataAdapterException {
		this.history = history;
		initTypeMappings();
		writeStreamList.clear();
		printStreamList.clear();

		Iterator it = config.getTypeMappings().iterator();
		while (it.hasNext()) {
			CharTypeMapping ctm = (CharTypeMapping) it.next();
			addTypeMapping(ctm);
		}

		Set trueroots = new LinkedHashSet();
		it = history.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			if (io instanceof OBOClass
					&& ((OBOClass) io).getParents().size() == 0
					&& !TermUtil.isObsolete(io) && !io.isBuiltIn()) {
				trueroots.add(io);
			}
		}
		/*
		 */
		List saveRecords;
		if (config.getBasicSave()) {
			saveRecords = new LinkedList();
			saveRecords.add(new SaveRecord(null, config.getWritePath()));
		} else
			saveRecords = config.getSaveRecords();

		it = saveRecords.iterator();

		Vector roots = new Vector();
		while (it.hasNext()) {
			SaveRecord sr = (SaveRecord) it.next();

			OBOClass root = (OBOClass) trueroots.iterator().next();

			OBOClass term;
			if (sr.getID() == null) {
				if (trueroots.size() > 1)
					throw new DataAdapterException("Cannot write multiply "
							+ "rooted " + "file to this format, " + "roots = "
							+ trueroots);
				term = root;
			} else
				term = (OBOClass) TermUtil
						.cloneParentTree((LinkedObject) history.getObject(sr
								.getID()));
			roots.add(term);
			Vector relationshipTypes = new Vector(TermUtil
					.getRelationshipTypes(history));
			write(sr.getFilename(), sr.getDefFilename(), term,
					relationshipTypes, new Vector(history.getCategories()),
					OBOProperty.IS_A);
		}
		if (!config.getBasicSave())
			writeDefinitions(roots);

		// close all the streams

		it = printStreamList.iterator();
		while (it.hasNext()) {
			PrintStream sfos = (PrintStream) it.next();
			sfos.close();
			if (sfos.checkError()) {
				if (cancelled)
					throw new CancelledAdapterException();
				else
					throw new DataAdapterException(
							"Failed while trying to close file");
			}
		}
		/*
		 * try { } catch (IOException ex) { if (cancelled) throw new
		 * CancelledAdapterException(); else throw new
		 * DataAdapterException("Failed while trying to close file", ex); }
		 */
		return history;
	}

	public OBOProperty getDefaultType() {
		return OBOProperty.IS_A;
	}

	public void write(String filename, String defFilename, OBOClass root,
			Vector relationshipTypes, Vector categories, OBOProperty defaultType)
			throws DataAdapterException {
		writeRoot = root;
		try {
			Collection<LinkedObject> terms = TermUtil
					.getDescendants(root, true);
			if (filename != null) {
				Map h = TermUtil.createIDMap(terms);
				int termCount = h.size();

				h = null;
				OutputStream rawstream = new SafeFileOutputStream(filename);

				writeStreamList.add(rawstream);

				PrintStream stream = new PrintStream(new BufferedOutputStream(
						rawstream));
				printStreamList.add(stream);

				writeHeader(root, relationshipTypes, categories, defaultType,
						stream);
				setProgressString("Writing terms "
						+ (filename != null ? "to file " + filename : "")
						+ "...");
				writeTerm(filename, stream, 0, "$", root, null, true,
						new HashMap(), termCount);
			}

			if (defFilename != null) {
				writeDefinitionsForTerms(terms, defFilename);
			}

		} catch (IOException e) {
			if (cancelled)
				throw new CancelledAdapterException();
			else
				throw new DataAdapterException(e.getMessage(), e);
		}
	}

	public void writeDefinitions(Vector roots) throws DataAdapterException {
		Set set = new HashSet();
		for (int i = 0; i < roots.size(); i++) {
			Iterator e = TermUtil.getDescendants(((OBOClass) roots.get(i)),
					true).iterator();
			while (e.hasNext()) {
				OBOClass term = (OBOClass) e.next();
				set.add(term);
			}
		}
		writeDefinitionsForTerms(set);
	}

	public void writeDefinitionsForTerms(Collection t)
			throws DataAdapterException {
		writeDefinitionsForTerms(t, config.getSaveDefFilename());
	}

	public void writeDefinitionsForTerms(Collection t, String defFilename)
			throws DataAdapterException {
		try {
			if (defFilename != null && defFilename.length() > 0) {
				OutputStream rawstream = new SafeFileOutputStream(defFilename);
				writeStreamList.add(rawstream);
				PrintStream stream = new PrintStream(new BufferedOutputStream(
						rawstream));
				printStreamList.add(stream);
				Vector terms = new Vector();
				terms.addAll(t);
				writeDefs(defFilename, stream, terms);
			}
		} catch (IOException e) {
			if (cancelled)
				throw new CancelledAdapterException();
			else
				throw new DataAdapterException(e.getMessage());
		}
	}

	public void setUserName(String username) {
		this.username = username;
	}

	public String getUserName() {
		return username;
	}

	public void setAutogenString(String autogen) {
		this.autogen = autogen;
	}

	public String getAutogenString() {
		return autogen;
	}

	protected void writeDefs(String filename, PrintStream stream, Vector terms)
			throws DataAdapterException {
		Collections.sort(terms);
		/*
		 * Vector terms = new Vector(); for(int i=0; i < roots.size(); i++) {
		 * Iterator e = ((OBOClass) roots.get(i)).getAllDescendants(true);
		 * while(e.hasNext()) { OBOClass term = (OBOClass) e.next(); if
		 * (!terms.contains(term)) VectorUtil.insertSorted(terms, term); } }
		 */
		println(stream, "!version: " + "$Revision: 1.21 $");
		println(stream, "!date:                 " + (new Date()));
		if (getUserName() == null) {
			println(stream, "!saved-by: " + System.getProperty("user.name"));
		} else {
			println(stream, "!saved-by: " + getUserName());
		}
		if (autogen != null)
			println(stream, "!autogenerated-by: " + autogen);
		println(stream, "!");
		println(stream, "!Gene Ontology definitions");
		println(stream, "!");

		int index = 0;
		int oldProgress = -1;
		setProgressString("Writing definitions "
				+ (filename != null ? "to file " + filename : "") + "...");
		Iterator e = terms.iterator();
		while (e.hasNext()) {
			if (cancelled)
				throw new CancelledAdapterException();

			int progress = 100 * index / terms.size();
			setProgressValue(progress);
			setProgressString(progressString);

			OBOClass term = (OBOClass) e.next();
			if (term.getDefinition() != null
					&& term.getDefinition().length() > 0)
				writeDef(stream, term);

			index++;
		}
	}

	protected static String unescapeDefText(String def) {
		if (def == null)
			return null;
		StringBuffer out = new StringBuffer();
		boolean escape = false;
		for (int i = 0; i < def.length(); i++) {
			char c = def.charAt(i);
			if (!escape) {
				if (c == '\\') {
					escape = true;
				} else {
					out.append(c);
				}
			} else {
				escape = false;
				if (c == '\\')
					out.append('\\');
				else if (c == 'n')
					out.append('\n');
			}
		}
		return out.toString();
	}

	protected static String escapeDefText(String def) {
		StringBuffer out = new StringBuffer();
		for (int i = 0; i < def.length(); i++) {
			char c = def.charAt(i);
			if (c == '\\')
				out.append("\\\\");
			else if (c == '\r')
				out.append("\\n");
			else if (c == '\n')
				out.append("\\n");
			else {
				out.append(c);
			}
		}
		return out.toString();
	}

	protected void writeDef(PrintStream stream, OBOClass term) {
		println(stream, "term: " + term.getName());
		if (useLegacyTypes())
			println(stream, "goid: " + term.getID());
		else
			println(stream, "id: " + term.getID());
		println(stream, "definition: " + escapeDefText(term.getDefinition()));
		Iterator it = term.getDefDbxrefs().iterator();
		while (it.hasNext()) {
			Dbxref ref = (Dbxref) it.next();
			println(stream, "definition_reference: " + ref.getDatabase() + ":"
					+ ref.getDatabaseID());
		}
		if (!RootAlgorithm.STRICT.isRoot(term) && term.getComment() != null
				&& term.getComment().length() > 0)
			println(stream, "comment: " + escapeDefText(term.getComment()));
		println(stream);
	}

	protected void writeHeader(OBOClass root, Vector relationshipTypes,
			Vector categories, OBOProperty defaultType, PrintStream stream)
			throws DataAdapterException {
		String comment = config.getComment();
		if (comment == null)
			comment = root.getComment();
		Vector commentList = new Vector();
		if (comment != null) {
			StringTokenizer tokenizer = new StringTokenizer(comment, "\n\r");
			while (tokenizer.hasMoreTokens()) {
				String token = tokenizer.nextToken();
				if (!token.startsWith("date") && !token.startsWith("saved-by")
						&& !token.startsWith("autogenerated-by")
						&& !token.startsWith("version")
						&& !token.startsWith("type"))
					commentList.add(token);
			}
		}

		if (getAutogenString() != null)
			println(stream, "!autogenerated-by:     " + getAutogenString());
		if (getUserName() != null) {
			println(stream, "!saved-by:             " + getUserName());
		}
		println(stream, "!date:                 " + (new Date()));
		println(stream, "!version: " + "$Revision: 1.21 $");
		if (defaultType != null)
			writeTypeLine(defaultType, stream);
		for (int i = 0; i < relationshipTypes.size(); i++) {
			OBOProperty trt = (OBOProperty) relationshipTypes.get(i);
			if (defaultType == null || !defaultType.equals(trt))
				writeTypeLine(trt, stream);
		}

		for (int i = 0; i < commentList.size(); i++) {
			println(stream, "!" + commentList.get(i));
		}
	}

	public void writeTypeLine(OBOProperty trt, PrintStream stream)
			throws DataAdapterException {
		if (trt.isBuiltIn() && !TermUtil.isUsed(history, trt))
			return;
		String propID = trt.getID();
		if (config.getTranslateTypes() && IDMAPPINGS.get(trt.getID()) != null)
			propID = (String) IDMAPPINGS.get(trt.getID());
		if (useLegacyTypes()) {
			String typeStr = convertTypeToChar(trt);
			if (typeStr == null) {
				throw new DataAdapterException("Couldn't find a "
						+ "relationship character " + "for relationship type "
						+ trt);
			} else {
				println(stream, "!type: " + typeStr + " " + propID + " "
						+ trt.getName());
			}
		} else {
			println(stream, "!type: @" + propID + "@ " + propID + " "
					+ trt.getName());
			addTypeMapping("@" + propID + "@", trt);
		}
	}

	public void setUseLegacyTypes(boolean useLegacyTypes) {
		config.setUseLegacyTypes(useLegacyTypes);
	}

	public boolean useLegacyTypes() {
		return config.getUseLegacyTypes();
	}

	protected void writeTerm(String filename, PrintStream stream,
			int spaceDepth, String leaderChar, OBOClass term,
			OBOClass currentParent, boolean treatAsRoot, Map lookedAt,
			int termCount) throws DataAdapterException {
		if (cancelled)
			throw new CancelledAdapterException();

		boolean isCycle = false;
		if (lookedAt.containsKey(term)
				&& (config.getReduceSize() || (isCycle = TermUtil.hasAncestor(
						term, term)))) {
			if (config.getAllowCycles() || (config.getReduceSize() && !isCycle))
				return;
			else
				throw new DataAdapterException(
						"File contains a cycle! OBOClass " + term + " (id = "
								+ term.getID() + ") is its own parent!");
		} else if (!treatAsRoot) {
			setProgressValue(100 * lookedAt.size() / termCount);
			lookedAt.put(term, term);
		}
		/*
		 * if (!allowDangling && term.getDanglingParents().size() > 0) { throw
		 * new DataAdapterException("OBOClass "+ term+" (id = "+term.getID()+ ")
		 * contains dangling parents!"); }
		 */
		writeLineForTerm(stream, spaceDepth, leaderChar, term, currentParent,
				treatAsRoot);
		Vector children = VectorUtil.transform(new RelToHolderTransformation(
				false), new Vector(term.getChildren()));
		Collections.sort(children);

		for (int i = 0; i < children.size(); i++) {
			Link tr = ((RelationshipHolder) children.get(i)).getRelationship();
			writeTerm(filename, stream, spaceDepth + 1,
					getCharForRelationship(tr), (OBOClass) tr.getChild(), term,
					false, lookedAt, termCount);
		}
	}

	protected void writeLineForTerm(PrintStream stream, int spaceDepth,
			String leaderChar, OBOClass term, OBOClass currentParent,
			boolean treatAsRoot) {
		if (treatAsRoot)
			leaderChar = "$";
		String output = getNChars(spaceDepth, ' ') + leaderChar
				+ getTermString(term) + getSecondaryIDString(term)
				+ getDbxrefString(term) + getSynonymString(term)
				+ (treatAsRoot ? "" : getParentString(term, currentParent));
		println(stream, output);
	}

	protected String getDbxrefString(OBOClass term) {
		StringBuffer out = new StringBuffer();
		scratchVector.clear();
		Iterator it = term.getDbxrefs().iterator();
		while (it.hasNext()) {
			VectorUtil.insertSorted(scratchVector, dbxrefComparator, it.next());
		}

		for (int i = 0; i < scratchVector.size(); i++) {
			Dbxref ref = (Dbxref) scratchVector.get(i);
			out.append(" ; " + escape(ref.getDatabase()) + ":"
					+ escape(ref.getDatabaseID()));
		}

		return out.toString();
	}

	protected String getSynonymString(OBOClass term) {
		StringBuffer out = new StringBuffer();
		scratchVector.clear();
		scratchVector.addAll(term.getSynonyms());
		Collections.sort(scratchVector, Synonym.COMPARATOR);

		for (int i = 0; i < scratchVector.size(); i++) {
			Synonym syn = (Synonym) scratchVector.get(i);
			out.append(formatSynonym(syn));
		}
		return out.toString();
	}

	protected String formatSynonym(Synonym syn) {
		return " ; synonym:" + escape(syn.getText());
	}

	protected String getParentString(OBOClass term, OBOClass parent) {
		StringBuffer out = new StringBuffer();

		scratchVector.clear();
		Iterator it = term.getParents().iterator();
		while (it.hasNext()) {
			VectorUtil.insertSorted(scratchVector, relByParentComparator, it
					.next());
		}

		for (int i = 0; i < scratchVector.size(); i++) {
			Link tr = (Link) scratchVector.get(i);
			// someday, we should memoize the ancestor search
			if (!tr.getParent().getID().equals(parent.getID()) &&
			// !tr.getParent().isNonTriviallyObsolete() &&
					(TermUtil.hasAncestor(tr.getParent(), writeRoot) || tr
							.getParent().equals(writeRoot)))
				out.append(" " + getCharForRelationship(tr) + " "
						+ getTermString((OBOClass) tr.getParent()));
		}

		/*
		 * scratchVector.clear();
		 * 
		 * for(int i=0; i < term.getDanglingParents().size(); i++) {
		 * VectorUtil.insertSorted(scratchVector, danglingSorter,
		 * term.getDanglingParents().get(i)); }
		 * 
		 * Vector dangling = (Vector) term.getDanglingParents().clone();
		 * VectorUtil.sort(dangling, danglingSorter);
		 * 
		 * for(int i=0; i < scratchVector.size(); i++) { DanglingRelationship dr =
		 * (DanglingRelationship) scratchVector. get(i); out.append("
		 * "+getCharForRelationship(dr.getType())+" "+ getTermString(dr)); }
		 */
		return out.toString();
	}

	protected String getCharForRelationship(OBOProperty type) {
		String c = convertTypeToChar(type);
		if (c == null)
			return "?";
		else
			return c;
	}

	protected String getCharForRelationship(Link tr) {
		return getCharForRelationship(tr.getType());
	}

	protected String escape(String text) {
		StringBuffer out = new StringBuffer();
		for (int i = 0; i < text.length(); i++) {
			char c = text.charAt(i);
			if (Character.isISOControl(c) || Character.isIdentifierIgnorable(c)
					|| c == '\n' || c == '\r')
				continue;
			if (isReservedCharacter(c) || convertCharToType(c + "") != null)
				out.append('\\');
			out.append(c);
		}
		return out.toString();
	}

	protected String getTermString(OBOClass term) {
		if (term.getID() == null) {
			// this solution sucks
			// do something better
			(new Exception("unexpected condition encountered!"))
					.printStackTrace();
			/*
			 * try { String [] ids = idAdapter.getIDs(history, term, "GO", 7,
			 * 1); if (ids.length > 0) term.setID(ids[0]); } catch (Exception e) {
			 * throw new RuntimeException("Unexpected condition encountered!"); }
			 */
		}
		return escape(term.getName()) + " ; " + term.getID();
	}

	protected String getSecondaryIDString(OBOClass term) {
		String trailingIDs = "";
		if (term.getSecondaryIDs().size() > 0) {
			Iterator it = term.getSecondaryIDs().iterator();
			scratchVector.clear();
			while (it.hasNext()) {
				VectorUtil.insertSorted(scratchVector, idComparator, it.next());
			}
			for (int i = 0; i < scratchVector.size(); i++) {
				trailingIDs += ", " + (String) scratchVector.get(i);
			}
		}
		return trailingIDs;
	}

	protected String getNChars(int n, char c) {
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < n; i++)
			sb.append(c);
		return sb.toString();
	}

	public void cancel() {
		try {
			cancelled = true;
			if (currentReader != null)
				currentReader.close();
			Iterator it = writeStreamList.iterator();
			while (it.hasNext()) {
				SafeFileOutputStream stream = (SafeFileOutputStream) it.next();
				stream.fail();
			}
		} catch (Exception ex) {
		}
	}

	public GraphicalUI getAdvancedUI() {
		return advancedUI;
	}

	public void setAdvancedUI(GraphicalUI advancedUI) {
		this.advancedUI = advancedUI;
	}
}

class RelToHolderTransformation implements VectorTransformer {
	private boolean sortOnParent;

	public RelToHolderTransformation(boolean sortOnParent) {
		this.sortOnParent = sortOnParent;
	}

	public Object transform(Object in) {
		Link tr = (Link) in;
		return new RelationshipHolder(tr, sortOnParent);
	}
}

class RelationshipHolder implements Comparable {

	Link tr;

	boolean sortOnParent;

	public RelationshipHolder(Link in, boolean sortOnParent) {
		this.tr = in;
		this.sortOnParent = sortOnParent;
	}

	public Link getRelationship() {
		return tr;
	}

	public int compareTo(Object in) {
		if (in instanceof RelationshipHolder) {
			Link rel = ((RelationshipHolder) in).getRelationship();
			if (sortOnParent)
				return ((OBOClass) tr.getParent()).compareTo(rel.getParent());
			else
				return ((OBOClass) tr.getChild()).compareTo(rel.getChild());
		}
		return -1;
	}
}

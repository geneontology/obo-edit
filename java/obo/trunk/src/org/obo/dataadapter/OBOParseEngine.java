package org.obo.dataadapter;

import java.io.*;
import java.util.*;
import java.util.regex.*;
import java.net.*;
import java.text.*;

import javax.xml.parsers.*;
import org.xml.sax.*;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.w3c.dom.*;
import javax.xml.transform.*;
import javax.xml.transform.dom.*;
import javax.xml.transform.stream.*;

import org.bbop.io.*;
import org.bbop.util.*;

public class OBOParseEngine extends AbstractParseEngine {

	protected String line;

	protected int linenum = 0;

	protected int totalSize = 0;

	protected int bytesRead = 0;

	protected StringBuffer tempBuffer = new StringBuffer();

	protected SimpleDateFormat dateFormat = new SimpleDateFormat(
			"dd:MM:yyyy HH:mm");
	
	protected boolean readIDForStanza = false;

	// protected String currentStanza;

	protected static final HashMap<Character, Character> escapeChars = new HashMap<Character, Character>();

	protected static final HashMap<Character, Character> unescapeChars = new HashMap<Character, Character>();

	static {
		escapeChars.put(new Character(':'), new Character(':'));
		escapeChars.put(new Character('W'), new Character(' '));
		escapeChars.put(new Character('t'), new Character('\t'));
		escapeChars.put(new Character(','), new Character(','));
		escapeChars.put(new Character('"'), new Character('"'));
		escapeChars.put(new Character('\''), new Character('\''));
		escapeChars.put(new Character('n'), new Character('\n'));
		escapeChars.put(new Character('\\'), new Character('\\'));
		escapeChars.put(new Character('{'), new Character('{'));
		escapeChars.put(new Character('}'), new Character('}'));
		escapeChars.put(new Character('['), new Character('['));
		escapeChars.put(new Character(']'), new Character(']'));
		escapeChars.put(new Character('!'), new Character('!'));
		Iterator<Character> it = escapeChars.keySet().iterator();
		while (it.hasNext()) {
			Character key = it.next();
			Character value = escapeChars.get(key);
			unescapeChars.put(value, key);
		}
	}

	public static class SOPair {
		public String str = null;

		public int index = -1;

		public int endIndex = -1;

		public SOPair(String str, int index) {
			this(str, index, -1);
		}

		public SOPair(String str, int index, int endIndex) {
			this.str = str;
			this.index = index;
			this.endIndex = endIndex;
		}
	}

	public OBOParseEngine() {

	}

	public OBOParseEngine(OBOSimpleParser parser) {
		this();
		// setPath(path);
		setParser(parser);
	}

	public void setPath(String path) {
		paths = new LinkedList<String>();
		paths.add(convertPath(path));
	}

	protected static class NVPair {
		public NestedValue nv;

		public int endIndex;

		public NVPair(NestedValue nv, int endIndex) {
			this.nv = nv;
			this.endIndex = endIndex;
		}
	}

	public static boolean isEscapeStarter(char c) {
		return c == '\\';
	}

	public static boolean isQuote(char c) {
		return c == '"';
	}

	protected StringBuffer getTempBuffer() {
		tempBuffer.delete(0, tempBuffer.length());
		return tempBuffer;
	}

	protected SOPair readQuotedString(String value, int startIndex,
			int stopIndex, char terminatingChar, boolean requireQuotes,
			boolean legalEndOfLine) throws OBOParseException {

		char quoteChar = '\0';
		StringBuffer out = getTempBuffer();
		int i = startIndex;
		boolean useQuotes = false;

		for (; i < stopIndex; i++) {
			// burn through any leading whitespace
			if (Character.isWhitespace(value.charAt(i)))
				continue;
			// if the first non-whitespace character is not a quote,
			// proceed in non-quoted mode
			else if (!isQuote(value.charAt(i))) {
				if (requireQuotes)
					throw new OBOParseException(
							"Expected start of quoted string.", line, value,
							linenum, 0);
				useQuotes = false;
				break;
			} else {
				useQuotes = true;
				quoteChar = value.charAt(i);
				i++;
				break;
			}
		}

		// look for a closing quote or final delimiter
		for (; i < stopIndex; i++) {
			if (isEscapeStarter(value.charAt(i))) {
				i++;
				if (i >= value.length())
					throw new OBOParseException("Incomplete escape sequence.",
							getCurrentPath(), line, linenum, 0);
				out.append(value.charAt(i));
			} else if ((useQuotes && value.charAt(i) == quoteChar)
					|| (!useQuotes && value.charAt(i) == terminatingChar)) {
				if (!useQuotes)
					return new SOPair(out.toString().trim(), startIndex, i - 1);
				else
					return new SOPair(out.toString(), startIndex, i);
			} else {
				out.append(value.charAt(i));
			}
		}
		if (!useQuotes && legalEndOfLine)
			return new SOPair(out.toString().trim(), startIndex, i);
		else
			throw new OBOParseException("Unterminated quoted string.",
					getCurrentPath(), line, linenum, 0);
	}

	protected int getNestedValue(NestedValue nv, String str, int startIndex)
			throws OBOParseException {
		while (startIndex < str.length()) {
			int equalsIndex = findUnescaped(str, '=', startIndex, str.length());
			if (equalsIndex == -1)
				throw new OBOParseException("Expected = in trailing modifier",
						getCurrentPath(), line, linenum, 0);
			String name = str.substring(startIndex, equalsIndex).trim();
			SOPair value = readQuotedString(str, equalsIndex + 1, str.length(),
					',', false, true);
			PropertyValue pv = new PropertyValueImpl(unescape(name), value.str);
			nv.addPropertyValue(pv);
			startIndex = value.endIndex + 1;
			for (; startIndex < str.length(); startIndex++) {
				if (Character.isWhitespace(str.charAt(startIndex)))
					continue;
				else if (str.charAt(startIndex) == ',') {
					startIndex++;
					break;
				} else {
					System.err.println("found character |"
							+ str.charAt(startIndex) + "|");
					throw new OBOParseException("Expected comma in trailing"
							+ "modifier.", getCurrentPath(), line, linenum, 0);
				}
			}
		}
		return str.length();
	}

	public static String stripSpecialCharacters(String s) {
		StringBuffer out = new StringBuffer();
		for (int i = 0; i < s.length(); i++) {
			Character.UnicodeBlock unicodeBlock = Character.UnicodeBlock.of(s
					.charAt(i));
			if (unicodeBlock == null)
				System.err.println("got null unicode block in " + s);
			if (unicodeBlock != null
					&& unicodeBlock.equals(Character.UnicodeBlock.BASIC_LATIN))
				out.append(s.charAt(i));
		}
		return out.toString();
	}

	protected boolean detectXML(String path) throws IOException {
		BufferedReader reader = new BufferedReader(new InputStreamReader(
				new BufferedInputStream(IOUtil.getProgressableStream(path))));
		String line = reader.readLine();
		reader.close();
		return Pattern.matches("\\Q<?xml\\E\\s.*\\Q?>\\E", line);
	}

	protected BufferedReader transformAndReadXML(InputStream stream)
			throws IOException {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

		try {
			File stylesheet = new File("/home/jrichter/research/"
					+ "oboxml_to_obotext.xsl");
			DocumentBuilder builder = factory.newDocumentBuilder();
			Document document = builder.parse(stream);
			// Use a Transformer for output
			TransformerFactory tFactory = TransformerFactory.newInstance();
			StreamSource stylesource = new StreamSource(stylesheet);
			Transformer transformer = tFactory.newTransformer(stylesource);
			// Transformer transformer = tFactory.newTransformer();
			DOMSource source = new DOMSource(document);
			ByteArrayOutputStream outstream = new ByteArrayOutputStream();

			StreamResult result = new StreamResult(outstream);
			transformer.transform(source, result);
			outstream.close();
			return new BufferedReader(new InputStreamReader(
					new ByteArrayInputStream(outstream.toByteArray())));
		} catch (TransformerConfigurationException tce) {
			return null;
		} catch (TransformerException te) {
			return null;
		} catch (SAXException sxe) {
			return null;
		} catch (ParserConfigurationException pce) {
			return null;
		}
	}

	protected void doParse(String path) throws IOException, OBOParseException {
		setProgressString("Reading " + path);
		String currentStanza = null;

		BufferedReader reader = null;

		if (reader == null) {
			BufferedInputStream is = new BufferedInputStream(IOUtil
					.getProgressableStream(path));
			reader = new BufferedReader(new InputStreamReader(is));
		}
		URL url = IOUtil.getURL(path);
		totalSize += url.openConnection().getContentLength();
		for (linenum = 1; (line = reader.readLine()) != null; linenum++) {
			if (halt)
				throw new OBOParseException("Operation cancelled by user",
						getCurrentPath(), null, -1);
			bytesRead += line.length();
			// line = stripSpecialCharacters(line);

			if (line.length() == 0)
				continue;
			while (line.charAt(line.length() - 1) == '\\'
					&& line.charAt(line.length() - 2) != '\\') {
				String str = reader.readLine();
				linenum++;
				if (str == null)
					throw new OBOParseException("Unexpected end of file",
							getCurrentPath(), line, linenum);
				line = line.substring(0, line.length() - 1) + str;
			}
			if (line.charAt(0) == '!') {
				parser.readBangComment(line.substring(1));
			} else if (line.charAt(0) == '[') {
				if (line.charAt(line.length() - 1) != ']')
					throw new OBOParseException("Unclosed stanza \"" + line
							+ "\"", getCurrentPath(), line, linenum);
				String stanzaname = line.substring(1, line.length() - 1);
				if (stanzaname.length() < 1)
					throw new OBOParseException("Empty stanza",
							getCurrentPath(), line, linenum);
				currentStanza = stanzaname;
				parser.startStanza(stanzaname);
				readIDForStanza = false;
			} else {
				SOPair pair;
				try {
					pair = unescape(line, ':', 0, true);
				} catch (OBOParseException ex) {
					translateAndThrow(ex, line, linenum, 0);
					break;
				}

				String name = pair.str;

				int lineEnd = findUnescaped(line, '!', 0, line.length(), true);
				if (lineEnd == -1)
					lineEnd = line.length();

				// find nested values
				NestedValue nv = null;

				int trailingStartIndex = -1;
				int trailingEndIndex = -1;
				for (int i = lineEnd - 1; i >= 0; i--) {
					if (Character.isWhitespace(line.charAt(i))) {
						// keep going until we see non-whitespace
					} else if (line.charAt(i) == '}') {
						// if the first thing we see is a closing brace,
						// we have a trailing modifier
						if (i >= 1 && line.charAt(i - 1) == '\\')
							continue;
						trailingEndIndex = i;
						break;
					} else
						break;
				}

				if (trailingEndIndex != -1) {
					for (int i = trailingEndIndex - 1; i >= 0; i--) {
						if (line.charAt(i) == '{') {
							if (i >= 1 && line.charAt(i - 1) == '\\')
								continue;
							trailingStartIndex = i + 1;
						}
					}
				}

				int valueStopIndex;
				if (trailingStartIndex == -1 && trailingEndIndex != -1)
					throw new OBOParseException("Unterminated "
							+ "trailing modifier.", getCurrentPath(), line,
							linenum, 0);
				else if (trailingStartIndex != -1) {
					valueStopIndex = trailingStartIndex - 1;
					String trailing = line.substring(trailingStartIndex,
							trailingEndIndex).trim();
					nv = new NestedValueImpl();
					getNestedValue(nv, trailing, 0);
				} else
					valueStopIndex = lineEnd;

				String value = line.substring(pair.index + 1, valueStopIndex);
				/*
				 * if (nv != null) System.err.println("nv = "+nv+", value =
				 * |"+value+"|");
				 */
				if (value.length() == 0)
					throw new OBOParseException("Tag found with no value",
							getCurrentPath(), line, linenum);
				if (parser instanceof OBOParser) {
					try {
						parseTag(currentStanza, line, linenum, pair.index + 1,
								name, value, nv);
					} catch (OBOParseException ex) {
						ex.printStackTrace();
						translateAndThrow(ex, line, linenum, pair.index + 1);
					}
				} else {
					try {
						parser.readTagValue(name, value, nv, false);
					} catch (OBOParseException ex) {
						translateAndThrow(ex, line, linenum, pair.index + 1);
					}
				}
			}
			int percentVal = 100 * bytesRead / totalSize;
			setProgressValue(percentVal);
		}
	}

	protected void parseTag(String stanza, String line, int linenum,
			int charoffset, String name, String value, NestedValue nv)
			throws OBOParseException, IOException {
		value = value.trim();
		if (stanza != null) {
			if (!readIDForStanza && !name.equals("id")
					&& parser instanceof OBOParser) {
				((OBOParser) parser).readImpliedID();
				readIDForStanza = true;
			}
			if (!(stanza.equalsIgnoreCase("term")
					|| stanza.equalsIgnoreCase("typedef") || stanza
					.equalsIgnoreCase("instance"))) {
				parser.readTagValue(name, value, nv, false);
				return;
			}
		}

		if (name.equals("import")) {
			if (stanza != null) {
				throw new OBOParseException("import tags may only occur "
						+ "in the header", getCurrentPath(), line, linenum, 0);
			}
			System.err.println("reading import with value " + value);
			int myLineNum = this.linenum;
			((OBOParser) parser).readImport(value);
			this.linenum = myLineNum;
		} else if (name.equals("namespace-id-rule")) {
			StringTokenizer tokenizer = new StringTokenizer(value);

			List tokens = new ArrayList();
			while (tokenizer.hasMoreTokens()) {
				tokens.add(tokenizer.nextToken());
			}
			if (tokens.size() != 2)
				throw new OBOParseException("Wrong number of arguments to "
						+ "id-mapping tag.", getCurrentPath(), line, linenum, 0);

			String ns = (String) tokens.get(0);

			((OBOParser) parser).readNamespaceIDRule((ns.equals("*") ? null
					: ns), (String) tokens.get(1));
		} else if (name.equals("default-namespace")) {
			((OBOParser) parser).readDefaultNamespace(value);
		} else if (name.equals("default-relationship-id-prefix")) {
			value = value.trim();
			for (int i = 0; i < value.length(); i++) {
				if (Character.isWhitespace(value.charAt(i))) {
					throw new OBOParseException("No whitespace is allowed "
							+ "in an id prefix", getCurrentPath(), line,
							linenum, 0);
				}
			}
			((OBOParser) parser).readIDPrefix(value);
		} else if (name.equals("id-mapping")) {
			StringTokenizer tokenizer = new StringTokenizer(value);
			List tokens = new Vector();
			while (tokenizer.hasMoreTokens()) {
				tokens.add(tokenizer.nextToken());
			}
			if (tokens.size() != 2)
				throw new OBOParseException("Wrong number of arguments to "
						+ "id-mapping tag.", getCurrentPath(), line, linenum, 0);
			((OBOParser) parser).readIDMapping((String) tokens.get(0),
					(String) tokens.get(1));
		} else if (name.equals("format-version")) {
			((OBOParser) parser).readFormatVersion(value);
		} else if (name.equals("version") || name.equals("data-version")) {
			((OBOParser) parser).readFileVersion(value);
		} else if (name.equals("date")) {

			try {
				((OBOParser) parser).readDate(dateFormat.parse(value));
			} catch (ParseException ex) {
				throw new OBOParseException("Badly formatted date: " + value,
						getCurrentPath(), line, linenum, 0);
			}
		} else if (name.equals("creation_date")) {
			try {
				((OBOParser) parser).readCreationDate(dateFormat.parse(value),
						nv);
			} catch (ParseException ex) {
				throw new OBOParseException("Badly formatted date: " + value,
						getCurrentPath(), line, linenum, 0);
			}
		} else if (name.equals("modification_date")) {
			try {
				((OBOParser) parser).readModificationDate(dateFormat
						.parse(value), nv);
			} catch (ParseException ex) {
				throw new OBOParseException("Badly formatted date: " + value,
						getCurrentPath(), line, linenum, 0);
			}
		} else if (name.equals("saved-by")) {
			((OBOParser) parser).readSavedBy(value);
		} else if (name.equals("auto-generated-by")) {
			((OBOParser) parser).readAutogeneratedBy(value);
		} else if (name.equals("remark")) {
			((OBOParser) parser).readRemark(value);
		} else if (name.equals("subsetdef")) {
			int subIndex = findUnescaped(value, '"', 0, value.length());
			int endDescIndex = findUnescaped(value, '"', subIndex + 1, value
					.length());
			String subset = value.substring(0, subIndex).trim();
			String subsetDesc = value.substring(subIndex + 1, endDescIndex)
					.trim();
			((OBOParser) parser).readSubsetDef(subset, subsetDesc);
		} else if (name.equals("synonymtypedef")) {
			int subIndex = findUnescaped(value, '"', 0, value.length());
			int endDescIndex = findUnescaped(value, '"', subIndex + 1, value
					.length());
			String id = value.substring(0, subIndex).trim();
			String desc = value.substring(subIndex + 1, endDescIndex).trim();
			String trailing = value.substring(endDescIndex + 1, value.length())
					.trim();
			int scope = Synonym.UNKNOWN_SCOPE;
			if (trailing.equals("EXACT"))
				scope = Synonym.EXACT_SYNONYM;
			else if (trailing.equals("BROAD"))
				scope = Synonym.BROAD_SYNONYM;
			else if (trailing.equals("NARROW"))
				scope = Synonym.NARROW_SYNONYM;
			else if (trailing.equals("RELATED"))
				scope = Synonym.RELATED_SYNONYM;
			((OBOParser) parser).readSynonymCategory(id, desc, scope);
		} else if (name.equals("id")) {
			((OBOParser) parser).readID(value, nv);
			readIDForStanza = true;
		} else if (name.equals("name")) {
			((OBOParser) parser).readName(unescape(value), nv);
		} else if (name.equals("alt_id")) {
			((OBOParser) parser).readAltID(value, nv);
		} else if (name.equals("comment")) {
			((OBOParser) parser).readComment(unescape(value), nv);
		} else if (name.equals("created_by")) {
			((OBOParser) parser).readCreatedBy(unescape(value), nv);
		} else if (name.equals("modified_by")) {
			((OBOParser) parser).readModifiedBy(unescape(value), nv);
		} else if (name.equals("namespace")) {
			((OBOParser) parser).readNamespace(value, nv);
		} else if (name.equals("domain")) {
			((OBOParser) parser).readDomain(value, nv);
		} else if (name.equals("range")) {
			((OBOParser) parser).readRange(value, nv);
		} else if (name.equals("def")) {
			int startIndex = findUnescaped(value, '"', 0, value.length());
			if (startIndex == -1)
				throw new OBOParseException("Expected \"", getCurrentPath(),
						line, linenum, 0);
			SOPair p = unescape(value, '"', startIndex + 1, value.length(),
					true);
			int defIndex = findUnescaped(value, '[', p.index, value.length());
			if (defIndex == -1) {
				throw new OBOParseException("Badly formatted definition. "
						+ "No dbxref list found.", getCurrentPath(), line,
						linenum, 0);
			}
			OBOParser.XrefPair[] refs = getDbxrefList(value, linenum,
					defIndex + 1, value.length());
			((OBOParser) parser).readDef(p.str, refs, nv);
		} else if (name.equals("xref_analog")) {
			OBOParser.XrefPair pair = parseXref(value, linenum, 0, value
					.length());
			pair.setNestedValue(nv);
			((OBOParser) parser).readXrefAnalog(pair);
		} else if (name.equals("xref_unk")) {
			OBOParser.XrefPair pair = parseXref(value, linenum, 0, value
					.length());
			pair.setNestedValue(nv);
			((OBOParser) parser).readXrefUnk(pair);
		} else if (name.equals("xref")) {
			OBOParser.XrefPair pair = parseXref(value, linenum, 0, value
					.length());
			pair.setNestedValue(nv);
			((OBOParser) parser).readXrefUnk(pair);
		} else if (name.equals("subset")) {
			((OBOParser) parser).readSubset(value, nv);
		} else if (name.equals("instance_of")) {
			((OBOParser) parser).readInstanceOf(value, nv);
		} else if (name.equals("property_value")) {
			try {
				int quoteIndex = findUnescaped(value, '"', 0, value.length());
				if (quoteIndex == -1) {
					StringTokenizer tokenizer = new StringTokenizer(value);
					List tokens = new Vector();
					while (tokenizer.hasMoreTokens()) {
						tokens.add(tokenizer.nextToken());
					}
					if (tokens.size() != 2)
						throw new OBOParseException("Wrong number of "
								+ "arguments to " + "property_value tag.",
								getCurrentPath(), line, linenum, 0);

					((OBOParser) parser).readPropertyValue((String) tokens
							.get(0), (String) tokens.get(1), null, false, nv);
				} else {
					SOPair p = unescape(value, '"', quoteIndex + 1, value
							.length(), true);
					String propID = value.substring(0, quoteIndex).trim();
					String optional = value.substring(p.index + 1,
							value.length()).trim();
					if (optional.length() == 0)
						optional = null;
					((OBOParser) parser).readPropertyValue(propID, p.str,
							optional, true, nv);
				}
			} catch (OBOParseException ex) {
				((OBOParser) parser).readTagValue(name, value, nv, false);
				return;
			}
		} else if (name.equals("synonym")) {
			int startIndex = findUnescaped(value, '"', 0, value.length());
			if (startIndex == -1)
				throw new OBOParseException("Expected \"", getCurrentPath(),
						line, linenum, 0);
			SOPair p = unescape(value, '"', startIndex + 1, value.length(),
					true);
			int defIndex = findUnescaped(value, '[', p.index, value.length());
			if (defIndex == -1) {
				throw new OBOParseException("Badly formatted synonym. "
						+ "No dbxref list found.", getCurrentPath(), line,
						linenum, 0);
			}
			String leftovers = value.substring(p.index + 1, defIndex).trim();
			StringTokenizer tokenizer = new StringTokenizer(leftovers, " \t");
			int scope = Synonym.RELATED_SYNONYM;
			String catID = null;
			for (int i = 0; tokenizer.hasMoreTokens(); i++) {
				String token = tokenizer.nextToken();
				if (i == 0) {
					if (token.equals("RELATED"))
						scope = Synonym.RELATED_SYNONYM;
					else if (token.equals("UNSPECIFIED"))
						scope = Synonym.RELATED_SYNONYM;
					else if (token.equals("EXACT"))
						scope = Synonym.EXACT_SYNONYM;
					else if (token.equals("BROAD"))
						scope = Synonym.BROAD_SYNONYM;
					else if (token.equals("NARROW"))
						scope = Synonym.NARROW_SYNONYM;
					else
						throw new OBOParseException("Found unexpected scope "
								+ "identifier " + token, getCurrentPath(),
								line, linenum, 0);
				} else if (i == 1) {
					catID = token;
				} else
					throw new OBOParseException("Expected dbxref list,"
							+ " instead found " + token, getCurrentPath(),
							line, linenum, 0);
			}
			OBOParser.XrefPair[] refs = getDbxrefList(value, linenum,
					defIndex + 1, value.length());
			((OBOParser) parser).readSynonym(p.str, refs, scope, catID, nv);
		} else if (name.equals("related_synonym")) {
			int startIndex = findUnescaped(value, '"', 0, value.length());
			if (startIndex == -1)
				throw new OBOParseException("Expected \"", getCurrentPath(),
						line, linenum, 0);
			SOPair p = unescape(value, '"', startIndex + 1, value.length(),
					true);
			int defIndex = findUnescaped(value, '[', p.index, value.length());
			if (defIndex == -1) {
				throw new OBOParseException("Badly formatted definition. "
						+ "No dbxref list found.", getCurrentPath(), line,
						linenum, 0);
			}
			OBOParser.XrefPair[] refs = getDbxrefList(value, linenum,
					defIndex + 1, value.length());
			((OBOParser) parser).readSynonym(p.str, refs,
					Synonym.RELATED_SYNONYM, null, nv);
		} else if (name.equals("exact_synonym")) {
			int startIndex = findUnescaped(value, '"', 0, value.length());
			if (startIndex == -1)
				throw new OBOParseException("Expected \"", getCurrentPath(),
						line, linenum, 0);
			SOPair p = unescape(value, '"', startIndex + 1, value.length(),
					true);
			int defIndex = findUnescaped(value, '[', p.index, value.length());
			if (defIndex == -1) {
				throw new OBOParseException("Badly formatted definition. "
						+ "No dbxref list found.", getCurrentPath(), line,
						linenum, 0);
			}
			OBOParser.XrefPair[] refs = getDbxrefList(value, linenum,
					defIndex + 1, value.length());
			((OBOParser) parser).readSynonym(p.str, refs,
					Synonym.EXACT_SYNONYM, null, nv);
		} else if (name.equals("narrow_synonym")) {
			int startIndex = findUnescaped(value, '"', 0, value.length());
			if (startIndex == -1)
				throw new OBOParseException("Expected \"", getCurrentPath(),
						line, linenum, 0);
			SOPair p = unescape(value, '"', startIndex + 1, value.length(),
					true);
			int defIndex = findUnescaped(value, '[', p.index, value.length());
			if (defIndex == -1) {
				throw new OBOParseException("Badly formatted definition. "
						+ "No dbxref list found.", getCurrentPath(), line,
						linenum, 0);
			}
			OBOParser.XrefPair[] refs = getDbxrefList(value, linenum,
					defIndex + 1, value.length());
			((OBOParser) parser).readSynonym(p.str, refs,
					Synonym.NARROW_SYNONYM, null, nv);
		} else if (name.equals("broad_synonym")) {
			int startIndex = findUnescaped(value, '"', 0, value.length());
			if (startIndex == -1)
				throw new OBOParseException("Expected \"", getCurrentPath(),
						line, linenum, 0);
			SOPair p = unescape(value, '"', startIndex + 1, value.length(),
					true);
			int defIndex = findUnescaped(value, '[', p.index, value.length());
			if (defIndex == -1) {
				throw new OBOParseException("Badly formatted definition. "
						+ "No dbxref list found.", getCurrentPath(), line,
						linenum, 0);
			}
			OBOParser.XrefPair[] refs = getDbxrefList(value, linenum,
					defIndex + 1, value.length());
			((OBOParser) parser).readSynonym(p.str, refs,
					Synonym.BROAD_SYNONYM, null, nv);
		} else if (name.equals("relationship")) {
			RelStruct relStruct = parseRelationship(value, nv, null);
			doReadRelationship(relStruct, (OBOParser) parser);
			/*
			 * ((GOBOParser) parser).readRelationship(relStruct.getType(),
			 * relStruct.getID(), relStruct.getNec(), relStruct.getInvNec(),
			 * relStruct.completes(), relStruct.getMinCard(),
			 * relStruct.getMaxCard(), relStruct.getCard(), relStruct.getNS(),
			 * relStruct.getNV());
			 */
		} else if (name.equals("intersection_of")) {
			RelStruct relStruct = parseRelationship(value, nv, null, true);
			relStruct.setCompletes(true);
			if (relStruct.getType() == null)
				relStruct.setType("is_a");
			doReadRelationship(relStruct, (OBOParser) parser);
		} else if (name.equals("is_a")) {
			RelStruct relStruct = parseRelationship(value, nv, "is_a");
			doReadRelationship(relStruct, (OBOParser) parser);
			/*
			 * String ns = null; boolean completes = false; if (nv != null) {
			 * Vector dumpEm = new Vector(); for(int i=0; i <
			 * nv.getPropertyValues().size(); i++) { PropertyValue pv =
			 * (PropertyValue) nv.getPropertyValues(). get(i); if
			 * (pv.getProperty(). equalsIgnoreCase("namespace")) { ns =
			 * pv.getValue(); dumpEm.add(pv); } else if (pv.getProperty().
			 * equalsIgnoreCase("completes")) { completes =
			 * pv.getValue().equalsIgnoreCase("true"); dumpEm.add(pv); } }
			 * nv.getPropertyValues().removeAll(dumpEm); } ((GOBOParser)
			 * parser).readIsa(value.trim(), ns, completes, nv);
			 */
		} else if (name.equals("disjoint_from")) {
			RelStruct relStruct = parseRelationship(value, nv, "disjoint_from");
			doReadRelationship(relStruct, (OBOParser) parser);
			/*
			 * String ns = null; if (nv != null) { Vector dumpEm = new Vector();
			 * for(int i=0; i < nv.getPropertyValues().size(); i++) {
			 * PropertyValue pv = (PropertyValue) nv.getPropertyValues().
			 * get(i); if (pv.getProperty(). equalsIgnoreCase("namespace")) { ns =
			 * pv.getValue(); dumpEm.add(pv); } }
			 * nv.getPropertyValues().removeAll(dumpEm); } ((GOBOParser)
			 * parser).readDisjoint(value.trim(), ns, nv);
			 */
		} else if (name.equals("inverse_of")) {
			RelStruct relStruct = parseRelationship(value, nv, "inverse_of");
			doReadRelationship(relStruct, (OBOParser) parser);
			/*
			 * String ns = null; if (nv != null) { Vector dumpEm = new Vector();
			 * for(int i=0; i < nv.getPropertyValues().size(); i++) {
			 * PropertyValue pv = (PropertyValue) nv.getPropertyValues().
			 * get(i); if (pv.getProperty(). equalsIgnoreCase("namespace")) { ns =
			 * pv.getValue(); dumpEm.add(pv); } }
			 * nv.getPropertyValues().removeAll(dumpEm); } ((GOBOParser)
			 * parser).readInverseOf(value.trim(), ns, nv);
			 */
		} else if (name.equals("is_obsolete")) {
			if (value.trim().equalsIgnoreCase("true"))
				((OBOParser) parser).readIsObsolete(nv);
		} else if (name.equals("is_anonymous")) {
			if (value.trim().equalsIgnoreCase("true"))
				((OBOParser) parser).readIsAnonymous(nv);
		} else if (name.equals("is_cyclic")) {
			if (value.trim().equalsIgnoreCase("true"))
				((OBOParser) parser).readIsCyclic(true, nv);
			else if (value.trim().equalsIgnoreCase("false"))
				((OBOParser) parser).readIsCyclic(false, nv);
			else
				throw new OBOParseException("Invalid value for is_cyclic",
						getCurrentPath(), line, linenum, 0);
		} else if (name.equals("is_symmetric")) {
			if (value.trim().equalsIgnoreCase("true"))
				((OBOParser) parser).readIsSymmetric(true, nv);
			else if (value.trim().equalsIgnoreCase("false"))
				((OBOParser) parser).readIsSymmetric(false, nv);
			else
				throw new OBOParseException("Invalid value for is_symmetric",
						getCurrentPath(), line, linenum, 0);
		} else if (name.equals("is_reflexive")) {
			if (value.trim().equalsIgnoreCase("true"))
				((OBOParser) parser).readIsReflexive(true, nv);
			else if (value.trim().equalsIgnoreCase("false"))
				((OBOParser) parser).readIsReflexive(false, nv);
			else
				throw new OBOParseException(
						"Invalid value for always_implies_inverse",
						getCurrentPath(), line, linenum, 0);
		} else if (name.equals("always_implies_inverse")) {
			if (value.trim().equalsIgnoreCase("true"))
				((OBOParser) parser).readAlwaysImpliesInverse(true, nv);
			else if (value.trim().equalsIgnoreCase("false"))
				((OBOParser) parser).readAlwaysImpliesInverse(false, nv);
			else
				throw new OBOParseException(
						"Invalid value for always_implies_inverse",
						getCurrentPath(), line, linenum, 0);
		} else if (name.equals("is_transitive")) {
			if (value.trim().equalsIgnoreCase("true"))
				((OBOParser) parser).readIsTransitive(true, nv);
			else if (value.trim().equalsIgnoreCase("false"))
				((OBOParser) parser).readIsTransitive(false, nv);
			else
				throw new OBOParseException("Invalid value for is_transitive",
						getCurrentPath(), line, linenum, 0);
		} else if (name.equals("consider")) {
			((OBOParser) parser).readConsider(value.trim(), nv);
		} else if (name.equals("replaced_by")) {
			((OBOParser) parser).readReplacedBy(value.trim(), nv);
		}
	}

	public static class RelStruct {
		protected String type;

		protected String id;

		protected boolean nec;

		protected boolean invNec;

		protected boolean completes;

		protected boolean implied;

		protected Integer minCard;

		protected Integer maxCard;

		protected Integer card;

		protected String ns;

		protected NestedValue nv;

		public RelStruct(String type, String id, boolean nec, boolean invNec,
				boolean completes, boolean implied, Integer minCard,
				Integer maxCard, Integer card, String ns, NestedValue nv) {
			this.type = type;
			this.id = id;
			this.nec = nec;
			this.invNec = invNec;
			this.completes = completes;
			this.implied = implied;
			this.minCard = minCard;
			this.maxCard = maxCard;
			this.card = card;
			this.ns = ns;
			this.nv = nv;
		}

		public boolean isImplied() {
			return implied;
		}

		public void setNestedValue(NestedValue nv) {
			this.nv = nv;
		}

		public NestedValue getNV() {
			return nv;
		}

		public void setNamespace(String ns) {
			this.ns = ns;
		}

		public String getNS() {
			return ns;
		}

		public void setCard(Integer card) {
			this.card = card;
		}

		public Integer getCard() {
			return card;
		}

		public void setMaxCard(Integer maxCard) {
			this.maxCard = maxCard;
		}

		public Integer getMaxCard() {
			return maxCard;
		}

		public void setMinCard(Integer minCard) {
			this.minCard = minCard;
		}

		public Integer getMinCard() {
			return minCard;
		}

		public void setCompletes(boolean completes) {
			this.completes = completes;
		}

		public boolean completes() {
			return completes;
		}

		public boolean getInvNec() {
			return invNec;
		}

		public void setInvNec(boolean invNec) {
			this.invNec = invNec;
		}

		public boolean getNec() {
			return nec;
		}

		public void setNec(boolean nec) {
			this.nec = nec;
		}

		public String getID() {
			return id;
		}

		public void setID(String id) {
			this.id = id;
		}

		public String getType() {
			return type;
		}

		public void setType(String type) {
			this.type = type;
		}

	}

	protected void doReadRelationship(RelStruct relStruct, OBOParser parser)
			throws OBOParseException {
		if (relStruct.getType().equals("is_a")) {
			parser.readIsa(relStruct.getID(), relStruct.getNS(), relStruct
					.completes(), relStruct.isImplied(), relStruct.getNV());
		} else if (relStruct.getType().equals("disjoint_from")) {
			parser.readDisjoint(relStruct.getID(), relStruct.getNS(), relStruct
					.isImplied(), relStruct.getNV());
		} else if (relStruct.getType().equals("inverse_of")) {
			parser.readInverseOf(relStruct.getID(), relStruct.getNS(),
					relStruct.isImplied(), relStruct.getNV());
		} else
			parser.readRelationship(relStruct.getType(), relStruct.getID(),
					relStruct.getNec(), relStruct.getInvNec(), relStruct
							.completes(), relStruct.isImplied(), relStruct
							.getMinCard(), relStruct.getMaxCard(), relStruct
							.getCard(), relStruct.getNS(), relStruct.getNV());
	}

	protected RelStruct parseRelationship(String value, NestedValue nv,
			String type) throws OBOParseException {
		return parseRelationship(value, nv, type, false);
	}

	protected RelStruct parseRelationship(String value, NestedValue nv,
			String type, boolean intersection) throws OBOParseException {
		value = value.trim();
		int typeIndex = 0;
		if (type == null) {
			typeIndex = findUnescaped(value, ' ', 0, value.length()) + 1;
			type = value.substring(0, typeIndex).trim();
			if ((typeIndex == -1 || type.length() == 0) && !intersection)
				throw new OBOParseException("No id specified for"
						+ " relationship.", getCurrentPath(), line, linenum, 0);
		}

		int endoffset = findUnescaped(value, '[', typeIndex + type.length(),
				value.length());
		String id;
		if (endoffset == -1)
			id = value.substring(typeIndex, value.length()).trim();
		else {
			id = value.substring(typeIndex, endoffset).trim();
		}

		if (id.length() == 0)
			throw new OBOParseException("Empty id specified for"
					+ " relationship.", getCurrentPath(), line, linenum, 0);
		boolean necessary = true;
		boolean inverseNecessary = false;
		boolean completes = false;
		boolean implied = false;
		Integer minCardinality = null;
		Integer maxCardinality = null;
		Integer cardinality = null;
		String ns = null;
		if (nv != null) {
			Vector dumpEm = new Vector();
			Iterator it = nv.getPropertyValues().iterator();
			while (it.hasNext()) {
				PropertyValue pv = (PropertyValue) it.next();

				if (pv.getProperty().equalsIgnoreCase("necessary")) {
					necessary = !pv.getValue().equalsIgnoreCase("false");
					dumpEm.add(pv);
				} else if (pv.getProperty().equalsIgnoreCase("namespace")) {
					ns = pv.getValue();
					dumpEm.add(pv);
				} else if (pv.getProperty().equalsIgnoreCase(
						"inverse_necessary")) {
					inverseNecessary = pv.getValue().equalsIgnoreCase("true");
					dumpEm.add(pv);
				} else if (pv.getProperty().equalsIgnoreCase("contingent")
						|| pv.getProperty().equalsIgnoreCase("novel_inferred")
						|| pv.getProperty().equalsIgnoreCase("autoparent")
						|| pv.getProperty().equalsIgnoreCase("implied")) {
					implied = pv.getValue().equalsIgnoreCase("true");
					System.err.println("read implied = " + implied
							+ ", value = " + pv.getValue());
					dumpEm.add(pv);
				} else if (pv.getProperty().equalsIgnoreCase("completes")) {
					completes = pv.getValue().equalsIgnoreCase("true");
					dumpEm.add(pv);
				} else if (pv.getProperty().equalsIgnoreCase("minCardinality")) {
					try {
						minCardinality = new Integer(pv.getValue().trim());
					} catch (NumberFormatException ex) {
						// in the future, throw an error if this is
						// malformed
					}
					dumpEm.add(pv);
				} else if (pv.getProperty().equalsIgnoreCase("maxCardinality")) {
					try {
						maxCardinality = new Integer(pv.getValue().trim());
					} catch (NumberFormatException ex) {
						// in the future, throw an error if this is
						// malformed
					}
					dumpEm.add(pv);
				} else if (pv.getProperty().equalsIgnoreCase("cardinality")) {
					try {
						cardinality = new Integer(pv.getValue().trim());
					} catch (NumberFormatException ex) {
						// in the future, throw an error if this is
						// malformed
					}
					dumpEm.add(pv);
				}
			}
			nv.getPropertyValues().removeAll(dumpEm);
		}

		if (type.length() == 0)
			type = null;

		return new RelStruct(type, id, necessary, inverseNecessary, completes,
				implied, minCardinality, maxCardinality, cardinality, ns, nv);
	}

	protected OBOParser.XrefPair[] getDbxrefList(String line, int linenum,
			int startoffset, int endoffset) throws OBOParseException {
		Vector temp = new Vector();
		boolean stop = false;
		while (!stop) {
			int braceIndex = findUnescaped(line, '{', startoffset, endoffset);
			int endIndex = findUnescaped(line, ',', startoffset, endoffset,
					true);
			boolean trailing = false;
			if (endIndex == -1) {
				endIndex = findUnescaped(line, ']', startoffset, endoffset,
						true);
				if (endIndex == -1) {
					throw new OBOParseException("Unterminated xref list",
							getCurrentPath(), line, linenum, startoffset);
				}
				stop = true;
			}
			if (braceIndex != -1 && braceIndex < endIndex) {
				endIndex = braceIndex;
				trailing = true;
			}

			OBOParser.XrefPair pair = parseXref(line, linenum, startoffset,
					endIndex);
			if (pair == null) {
				startoffset++;
				continue;
			}
			NestedValue nv = null;
			if (trailing) {
				nv = new NestedValueImpl();
				endIndex = getNestedValue(nv, line, endIndex + 1);
				if (endIndex == -1) {
					throw new OBOParseException("Badly formatted "
							+ "trailing properties", getCurrentPath(), line,
							linenum, startoffset);
				}
				pair.nv = nv;
			}

			temp.add(pair);
			startoffset = endIndex + 1;
		}
		OBOParser.XrefPair[] out = new OBOParser.XrefPair[temp.size()];
		for (int i = 0; i < temp.size(); i++) {
			OBOParser.XrefPair pair = (OBOParser.XrefPair) temp.get(i);
			out[i] = pair;
		}
		return out;
	}

	protected OBOParser.XrefPair parseXref(String line, int linenum,
			int startoffset, int endoffset) throws OBOParseException {
		String xref_str = null;
		String desc_str = null;

		SOPair xref = unescape(line, '"', startoffset, endoffset, false);
		xref_str = xref.str.trim();
		if (xref_str.length() == 0)
			return null;

		if (xref.index != -1) {
			SOPair desc = unescape(line, '"', xref.index + 1, endoffset, true);
			desc_str = desc.str.trim();
		}
		return new OBOParser.XrefPair(xref_str, desc_str);
	}

	public static String escape(String str, boolean escapespaces) {
		StringBuffer out = new StringBuffer();
		for (int i = 0; i < str.length(); i++) {
			char c = str.charAt(i);
			Object o = unescapeChars.get(new Character(c));
			if (o == null)
				out.append(c);
			else {
				if (escapespaces || (!escapespaces && c != ' ' && c != '\t')) {
					out.append("\\" + o);
				} else
					out.append(c);
			}
		}
		return out.toString();
	}

	public String unescape(String str) throws OBOParseException {
		return unescape(str, '\0', 0, str.length(), false).str;
	}

	public SOPair unescape(String str, char toChar, int startindex,
			boolean mustFindChar) throws OBOParseException {
		return unescape(str, toChar, startindex, str.length(), mustFindChar);
	}

	public SOPair unescape(String str, char toChar, int startindex,
			int endindex, boolean mustFindChar) throws OBOParseException {
		StringBuffer out = new StringBuffer();
		int endValue = -1;
		for (int i = startindex; i < endindex; i++) {
			char c = str.charAt(i);
			if (c == '\\') {
				i++;
				c = str.charAt(i);
				Character mapchar = (Character) escapeChars
						.get(new Character(c));
				if (mapchar == null)
					throw new OBOParseException("Unrecognized escape"
							+ " character " + c + " found.", getCurrentPath(),
							null, -1, i);
				out.append(mapchar);
			} else if (c == toChar) {
				endValue = i;
				break;
			} else {
				out.append(c);
			}
		}
		if (endValue == -1 && mustFindChar) {
			throw new OBOParseException("Expected " + toChar + ".",
					getCurrentPath(), str, -1);
		}
		return new SOPair(out.toString(), endValue);
	}

	protected String getCurrentPath() {
		return (String) pathStack.peek();
	}

	public static int findUnescaped(String str, char toChar) {
		return findUnescaped(str, toChar, 0, str.length());
	}

	public static int findUnescaped(String str, char toChar, int startIndex,
			int endIndex) {
		return findUnescaped(str, toChar, startIndex, endIndex, false);
	}

	public static int findUnescaped(String str, char toChar, int startindex,
			int endindex, boolean honorQuotes) {
		boolean inQuotes = false;
		char quoteChar = '\0';
		for (int i = startindex; i < endindex; i++) {
			char c = str.charAt(i);
			if (c == '\\') {
				i++;
				continue;
			} else if (inQuotes) {
				if (c == quoteChar)
					inQuotes = false;
				continue;
			} else if (c == toChar) {
				return i;
			} else if (honorQuotes && isQuote(c)) {
				inQuotes = true;
				quoteChar = c;
			}
		}
		return -1;
	}

	public static void translateAndThrow(OBOParseException ex,
			String wholeline, int linenum, int charoffset)
			throws OBOParseException {
		throw translateException(ex, wholeline, linenum, charoffset);
	}

	public static OBOParseException translateException(OBOParseException ex,
			String wholeline, int linenum, int charoffset) {
		int charnum = ex.getCharNum();
		if (charnum == -1)
			charnum = 0;
		return new OBOParseException(ex.getMessage(), ex.getPath(), wholeline,
				linenum, charnum + charoffset);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.obo.dataadapter.ParseEngine#getLineNum()
	 */
	public int getLineNum() {
		return linenum;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.obo.dataadapter.ParseEngine#getCurrentLine()
	 */
	public String getCurrentLine() {
		return line;
	}
}

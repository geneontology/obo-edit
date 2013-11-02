package org.obo.dataadapter;

import java.io.IOException;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

import org.bbop.io.IOUtil;
import org.bbop.util.AbstractProgressValued;

public abstract class AbstractParseEngine extends AbstractProgressValued implements ParseEngine {

	protected OBOSimpleParser parser;
	protected List<String> paths = new LinkedList<String>();
	protected boolean halt = false;
	protected Stack<String> pathStack;
	protected boolean readIDForStanza = false;

	public AbstractParseEngine() {
		super();
	}

	public void cancel() {
		halt = true;
		parser.cancel();
	}
	
	protected static String convertPath(String path) {
		return IOUtil.getURL(path).toString();
	}
	
	public void setPaths(Collection<String> paths) {
		this.paths = new LinkedList<String>();
		for(String path : paths) {
			this.paths.add(convertPath(path));
		}
	}

	public void parse(String uri, boolean isImport) throws IOException, OBOParseException {
		pathStack.push(uri);
		parser.startFileParse(uri);
		doParse(uri, isImport);
		parser.endFileParse(uri);
		pathStack.pop();
	}
	
	/**
	 * @param path to the file or URL at which the ontology resides
	 * @throws IOException
	 * @throws OBOParseException
	 * return list of imports
	 */
	protected abstract void doParse(String path, boolean isImport) throws IOException, OBOParseException;

	public void setParser(OBOSimpleParser parser) {
		this.parser = parser;
		parser.setParseEngine(this);
	}

	public void parse() throws IOException, OBOParseException {
		halt = false;
		if (parser == null)
			throw new IOException("Could not parse; no parser installed!");
		pathStack = new Stack<String>();
		parser.startParse();
		for (String path : paths) {
			parse(path, false);
		}
		parser.endParse();
	}

	public void setReadIDForCurrentBlock(boolean readIDForStanza) {
		this.readIDForStanza = readIDForStanza;
	}

	public boolean getReadIDForCurrentBlock() {
		return readIDForStanza;
	}
}

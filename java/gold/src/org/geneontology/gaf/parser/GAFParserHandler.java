package org.geneontology.gaf.parser;

import java.io.File;

public interface GAFParserHandler {

	public void startDocument(File gafFile);
	
	public void endDocument();
	
	public void handleColumns(String []cols);
	
	
}

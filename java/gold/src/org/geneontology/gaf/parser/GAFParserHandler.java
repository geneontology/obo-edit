package org.geneontology.gaf.parser;

public interface GAFParserHandler {

	public void startDocument();
	
	public void endDocument();
	
	public void handleColumns(String []cols);
	
	
}

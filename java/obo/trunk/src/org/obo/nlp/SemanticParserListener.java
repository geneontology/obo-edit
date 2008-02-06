package org.obo.nlp;

import org.bbop.util.AsynchronousListener;

public interface SemanticParserListener extends AsynchronousListener {

	public void parsingStarted();
	public void parsingFinished();
}

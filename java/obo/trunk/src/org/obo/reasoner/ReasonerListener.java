package org.obo.reasoner;

import org.bbop.util.AsynchronousListener;

public interface ReasonerListener extends AsynchronousListener {

	public void reasoningStarted();
	public void reasoningFinished();
	public void reasoningCancelled();
}

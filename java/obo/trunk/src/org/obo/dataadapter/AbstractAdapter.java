package org.obo.dataadapter;

import org.bbop.util.AbstractProgressValued;

public abstract class AbstractAdapter extends AbstractProgressValued implements OBOAdapter {
	
	protected boolean cancelled = false;

	public void cancel() {
		cancelled = true;
	}
}

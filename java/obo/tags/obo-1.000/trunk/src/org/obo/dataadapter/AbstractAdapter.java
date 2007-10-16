package org.obo.dataadapter;

import org.bbop.util.AbstractProgressValued;

public abstract class AbstractAdapter extends AbstractProgressValued implements OBOEditAdapter {
	
	protected boolean cancelled = false;

	public void cancel() {
		cancelled = true;
	}
}

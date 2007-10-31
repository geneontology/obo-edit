package org.oboedit.gui.filter;

public interface RenderSpec extends Cloneable {

	public RenderSpec merge(RenderSpec spec);

	public void clear();

	public Object clone();
}

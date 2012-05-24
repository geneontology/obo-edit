package org.oboedit.gui.filter;

import org.oboedit.gui.FilteredRenderable;

public interface RenderSpec extends Cloneable {

	public RenderSpec merge(FilteredRenderable fr, RenderSpec spec, Object o);

	public void clear();

	public Object clone();
}

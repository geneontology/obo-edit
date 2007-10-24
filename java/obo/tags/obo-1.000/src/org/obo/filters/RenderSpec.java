package org.obo.filters;

public interface RenderSpec extends Cloneable {

	public void merge(RenderSpec spec);

	public void clear();

	public Object clone();
}

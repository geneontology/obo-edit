package org.oboedit.gui.filter;

import org.obo.filters.Filter;

public class RenderedFilter implements Cloneable {
	protected Filter filter;
	protected RenderSpec spec;

	public RenderedFilter() {
		
	}

	public RenderedFilter(RenderSpec spec) {
		this(Filter.ALWAYS_TRUE, spec);
	}
	
	public RenderedFilter(Filter filter, RenderSpec spec) {
		super();
		this.filter = filter;
		this.spec = spec;
	}

	public Filter getFilter() {
		return filter;
	}

	public void setFilter(Filter filter) {
		this.filter = filter;
	}

	public RenderSpec getSpec() {
		return spec;
	}

	public void setSpec(RenderSpec spec) {
		this.spec = spec;
	}

	@Override
	public Object clone() {
		return new RenderedFilter((Filter) filter.clone(), (RenderSpec) spec
				.clone());
	}
	
	@Override
	public String toString() {
		return spec+" where matches "+filter;
	}

}

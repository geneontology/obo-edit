package org.obo.filters;

public class RenderedFilter implements Cloneable {
	protected Filter filter;
	protected RenderSpec spec;

	public RenderedFilter() {
		
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

}

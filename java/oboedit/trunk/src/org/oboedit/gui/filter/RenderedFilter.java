package org.oboedit.gui.filter;

import org.bbop.util.ObjectUtil;
import org.obo.filters.Filter;

import org.apache.log4j.*;

public class RenderedFilter implements Cloneable {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(RenderedFilter.class);
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

	public int hashCode() {
		return spec.hashCode() + filter.hashCode();
	}

	public boolean equals(Object o) {
		if (o instanceof RenderedFilter) {
			RenderedFilter rf = (RenderedFilter) o;
			return ObjectUtil.equals(filter, rf.getFilter())
					&& ObjectUtil.equals(spec, rf.getSpec());
		} else
			return false;
	}

	@Override
	public Object clone() {
		return new RenderedFilter((Filter) filter.clone(), (RenderSpec) spec
				.clone());
	}

	@Override
	public String toString() {
		return spec + " where matches " + filter;
	}

}

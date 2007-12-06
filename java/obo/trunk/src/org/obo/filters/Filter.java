package org.obo.filters;

import org.bbop.expression.JexlContext;
import org.bbop.util.*;
import org.obo.reasoner.ReasonedLinkDatabase;

public interface Filter<T> extends Cloneable, VectorFilter<T> {

	public static final Filter<?> ALWAYS_TRUE = new Filter<Object>() {

		public boolean satisfies(Object o) {
			return true;
		}

		public void setContext(JexlContext context) {			
		}
		
		@Override
		public Object clone() {
			return this;
		}
		
		public void setReasoner(ReasonedLinkDatabase reasoner) {
		}
	};
	
	public void setContext(JexlContext context);

	public boolean satisfies(T o);

	public Object clone();
	
	public void setReasoner(ReasonedLinkDatabase reasoner);
}

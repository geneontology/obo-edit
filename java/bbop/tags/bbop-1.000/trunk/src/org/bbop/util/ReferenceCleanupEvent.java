package org.bbop.util;

import java.lang.ref.Reference;
import java.util.EventObject;

public class ReferenceCleanupEvent<T> extends EventObject {

	protected Reference<? extends T> ref;

	public ReferenceCleanupEvent(Object source, Reference<? extends T> ref) {
		super(source);
		this.ref = ref;
	}
	
	public Reference<? extends T> getReference() {
		return ref;
	}
}

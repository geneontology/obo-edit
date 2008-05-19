package org.bbop.util;

import java.lang.ref.Reference;
import java.util.EventObject;

import org.apache.log4j.*;

public class ReferenceCleanupEvent<T> extends EventObject {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ReferenceCleanupEvent.class);

	protected Reference<? extends T> ref;

	public ReferenceCleanupEvent(Object source, Reference<? extends T> ref) {
		super(source);
		this.ref = ref;
	}
	
	public Reference<? extends T> getReference() {
		return ref;
	}
}

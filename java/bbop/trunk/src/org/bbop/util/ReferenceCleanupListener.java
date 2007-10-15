package org.bbop.util;

public interface ReferenceCleanupListener <T> {
	
	public void cleanup(ReferenceCleanupEvent<? extends T> event);

}

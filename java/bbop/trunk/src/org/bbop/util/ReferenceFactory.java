package org.bbop.util;

import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;

public interface ReferenceFactory<T> {

	public Reference<T> createReference(ReferenceQueue queue, T toMe);
}

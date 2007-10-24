package org.obo.datamodel;

import java.io.Serializable;

public interface Value<T> extends Cloneable, Serializable {

	public Type<T> getType();
}

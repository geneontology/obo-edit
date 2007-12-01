package org.obo.query.impl;

import org.obo.datamodel.FieldPath;

public class BasicSearchHit<T> implements SearchHit<T> {

	protected T hit;
	
	public BasicSearchHit(T hit) {
		this.hit = hit;
	}
	
	public T getHit() {
		return hit;
	}
}

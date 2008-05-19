package org.obo.query.impl;

import org.obo.datamodel.FieldPath;

import org.apache.log4j.*;

public class BasicSearchHit<T> implements SearchHit<T> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(BasicSearchHit.class);

	protected T hit;
	
	public BasicSearchHit(T hit) {
		this.hit = hit;
	}
	
	public T getHit() {
		return hit;
	}
}

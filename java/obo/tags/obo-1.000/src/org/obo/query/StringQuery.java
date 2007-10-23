package org.obo.query;

public interface StringQuery<IN_TYPE, OUT_TYPE> extends Query<IN_TYPE, OUT_TYPE> {
	
	public void setSearchString(String s);
}

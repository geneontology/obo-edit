package org.obo.query;

import java.util.Collection;
import java.util.Comparator;

import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;

public interface Query<IN_TYPE, OUT_TYPE> {
	
	public void setFieldPath(FieldPath path);
	
	public OUT_TYPE matches(IN_TYPE a);
	
	public IN_TYPE convertToInputType(OUT_TYPE original);
		
	public OUT_TYPE convertToOutputType(IN_TYPE original);
	
	public Comparator<? super OUT_TYPE> getComparator();
	
	public Collection<OUT_TYPE> createResultHolder();
	
	public Class<? super IN_TYPE> getInputType();
	
	public Collection<FieldPathSpec> getInputPaths();
}

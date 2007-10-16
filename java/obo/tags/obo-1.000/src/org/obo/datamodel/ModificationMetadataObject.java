package org.obo.datamodel;

import java.util.Date;

public interface ModificationMetadataObject {

	public String getCreatedBy();
	public void setCreatedBy(String username);
	public NestedValue getCreatedByExtension();
	public void setCreatedByExtension(NestedValue nv);
	
	public Date getCreationDate();
	public void setCreationDate(Date date);
	public NestedValue getCreationDateExtension();
	public void setCreationDateExtension(NestedValue nv);
	
	public String getModifiedBy();
	public void setModifiedBy(String username);
	public NestedValue getModifiedByExtension();
	public void setModifiedByExtension(NestedValue nv);
	
	public Date getModificationDate();
	public void setModificationDate(Date date);
	public NestedValue getModificationDateExtension();
	public void setModificationDateExtension(NestedValue nv);
}

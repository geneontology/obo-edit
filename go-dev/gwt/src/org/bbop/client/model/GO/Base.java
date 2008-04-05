package org.bbop.client.model.GO;

import java.util.ArrayList;
import java.util.List;


import com.google.gwt.user.client.rpc.IsSerializable;


public class Base implements IsSerializable {

	protected String id;
	protected String label;
	protected String sourceId;
	protected boolean isAnonymous;

	//
	public Base() {
		this.id = "null_base";
	}
	
	//
	public Base(String id) {
		this.id = id;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String name) {
		this.label = name;
	}	
	
	public String getSourceId() {
		return sourceId;
	}
	
	public void setSourceId(String sourceId) {
		this.sourceId = sourceId;
	}

	public boolean isAnonymous() {
		return isAnonymous;
	}

	public void setAnonymous(boolean isAnonymous) {
		this.isAnonymous = isAnonymous;
	}

	//
	public String toString() {
		String s = id + " \""+label+"\"";
		if (isAnonymous)
			s = " ANON:"+id;
		if (sourceId != null)
			s = s + " src:"+sourceId;
		return s;
	}
	
	public int hashCode() {
		return id.hashCode();
	}
	
}

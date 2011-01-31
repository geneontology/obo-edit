package org.bbop.client.model;

import java.util.ArrayList;
import java.util.List;


import com.google.gwt.user.client.rpc.IsSerializable;


public class GOBase implements IsSerializable {

	protected String id;
	protected String label;
	protected String sourceId;
	protected boolean isAnonymous;

	//
	public GOBase() {
		id = "null_base";
	}
	
	//
	public GOBase(String in_id) {
		id = in_id;
	}

	public String getId() {
		return id;
	}

	public void setId(String in_id) {
		id = in_id;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String name) {
		label = name;
	}	
	
	public String getSourceId() {
		return sourceId;
	}
	
	public void setSourceId(String in_sourceId) {
		sourceId = in_sourceId;
	}

	public boolean isAnonymous() {
		return isAnonymous;
	}

	public void setAnonymous(boolean in_isAnonymous) {
		isAnonymous = in_isAnonymous;
	}

	//
	public String toString() {
		String s = id + " \"" + label + "\"";
		if (isAnonymous)
			s = " ANON:" + id;
		if (sourceId != null)
			s = s + " src:" + sourceId;
		return s;
	}
	
	public int hashCode() {
		return id.hashCode();
	}
	
}

package org.bbop.client.model.samples;

import java.util.Date;

import com.extjs.gxt.ui.client.data.BaseModel;

public class TargetList extends BaseModel{
	
	public TargetList() {
		
	}
	
	public TargetList(String name, String id, String uniprot) {
		
		set("name", name);
		set("id", id);
		set("uniprot", uniprot);
		set("completed", new Date());
		
	}

}

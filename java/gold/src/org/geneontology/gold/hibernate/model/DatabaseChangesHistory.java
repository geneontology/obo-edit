package org.geneontology.gold.hibernate.model;

import java.io.Serializable;
import java.util.Calendar;
import java.util.Date;

public class DatabaseChangesHistory extends GOModel implements Serializable {

	private String objectId;
	private Date changeTime;
	
	public DatabaseChangesHistory(){
		String[] keys={"objectId", "changeTime"};
		this.initUniqueConstraintFields(DatabaseChangesHistory.class, keys);
		this.changeTime = Calendar.getInstance().getTime();
	}
	
	public DatabaseChangesHistory(String objectId, Date changeTime) {
		this();
		this.objectId = objectId;
		this.changeTime = changeTime;
	}

	public String getObjectId() {
		return objectId;
	}

	public Date getChangeTime() {
		return changeTime;
	}

	public void setObjectId(String objectId) {
		this.objectId = objectId;
	}

	public void setChangeTime(Date changeTime) {
		this.changeTime = changeTime;
	}
	
	
	
	
}

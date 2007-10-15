package org.bbop.util;

public abstract class AbstractProgressValued implements ProgressValued {
	
	protected String progressString;
	protected Number progressValue;
	
	public String getProgressString() {
		return progressString;
	}
	
	protected void setProgressString(String progressString) {
		this.progressString = progressString;
	}
	
	public Number getProgressValue() {
		return progressValue;
	}
	
	protected void setProgressValue(Number progressValue) {
		this.progressValue = progressValue;
	}


}

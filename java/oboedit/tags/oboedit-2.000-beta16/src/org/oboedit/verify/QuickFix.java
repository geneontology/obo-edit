package org.oboedit.verify;

public interface QuickFix {
	
	public static enum ReloadLevel { TERM, GLOBAL};

	public String getDesc();
	public ReloadLevel getLevel();
	public CheckWarning getWarning();
	public void setWarning(CheckWarning warning);
}

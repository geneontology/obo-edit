package org.oboedit.verify.impl;

import org.obo.history.HistoryItem;
import org.oboedit.verify.CheckWarning;
import org.oboedit.verify.HistoryQuickFix;
import org.oboedit.verify.QuickFix.ReloadLevel;

public class DefaultHistoryQuickFix implements HistoryQuickFix {

	protected String desc;
	protected HistoryItem item;
	protected CheckWarning warning;
	
	public String getDesc() {
		return desc;
	}
	
	public HistoryItem getItem() {
		return item;
	}
	
	public DefaultHistoryQuickFix(String desc, HistoryItem item) {
		this(desc, null, item);
	}

	public DefaultHistoryQuickFix(String desc, CheckWarning warning, HistoryItem item) {
		super();
		this.desc = desc;
		this.item = item;
		this.warning = warning;
	}
	
	public CheckWarning getWarning() {
		return warning;
	}
	
	public ReloadLevel getLevel() {
		return ReloadLevel.TERM;
	}
	
	public void setWarning(CheckWarning warning) {
		this.warning = warning;
	}
}

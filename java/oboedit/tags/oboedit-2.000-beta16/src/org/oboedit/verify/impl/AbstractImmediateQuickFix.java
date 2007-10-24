package org.oboedit.verify.impl;

import org.oboedit.verify.CheckWarning;
import org.oboedit.verify.ImmediateQuickFix;
import org.oboedit.verify.QuickFix.ReloadLevel;

public abstract class AbstractImmediateQuickFix implements ImmediateQuickFix {

	protected String desc;

	protected ReloadLevel level;

	protected CheckWarning warning;

	public AbstractImmediateQuickFix(String desc) {
		this(desc, ReloadLevel.TERM);
	}
	
	public AbstractImmediateQuickFix(String desc, ReloadLevel level) {
		this.desc = desc;
		this.level = level;
	}

	public String getDesc() {
		return desc;
	}

	public ReloadLevel getLevel() {
		return level;
	}
	
	public CheckWarning getWarning() {
		return warning;
	}
	
	public void setWarning(CheckWarning warning) {
		this.warning = warning;
	}
}

package org.oboedit.verify;

import java.util.*;
import javax.swing.*;

import org.bbop.util.*;
import org.obo.datamodel.*;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.oboedit.controller.VerificationManager;

public abstract class AbstractCheck implements Check {

	protected ReasonedLinkDatabase linkDatabase;
	protected CheckConfiguration configuration = createConfiguration();

	protected String progressString = "";
	protected int progressValue = -1;
	protected boolean cancelled;

	public void cancel() {
		cancelled = true;
	}

	public boolean isCancelled() {
		return cancelled;
	}

	{
		initConfiguration();
	}

	protected CheckConfiguration createConfiguration() {
		return new CheckConfiguration();
	}

	protected void initConfiguration() {
		configuration
				.setCondition((byte) (VerificationManager.SAVE | VerificationManager.MANUAL));
	}

	public String getDescription() {
		return "";
	}

	public boolean needsReasoner() {
		return false;
	}

	public void setReasoner(ReasonedLinkDatabase linkDatabase) {
		this.linkDatabase = linkDatabase;
	}

	public JComponent getConfigurationPanel() {
		return null;
	}

	public CheckConfiguration getConfiguration() {
		return configuration;
	}

	public void setConfiguration(CheckConfiguration configuration) {
		/*
		 * if (configuration == null) configuration = createConfiguration();
		 */
		this.configuration = configuration;
	}

	@Override
	public String toString() {
		return getID() + ": " + configuration;
	}

	public void setProgressString(String progressString) {
		this.progressString = progressString;
	}

	public void setProgressValue(int progressValue) {
		this.progressValue = progressValue;
	}

	public String getProgressString() {
		return progressString;
	}

	public Number getProgressValue() {
		return progressValue;
	}
}

package org.oboedit.verify;

import javax.swing.JComponent;

import org.bbop.util.*;
import org.obo.datamodel.*;
import org.obo.reasoner.ReasonedLinkDatabase;

public interface Check extends ProgressValued {

	public String getID();

	public String getDescription();

	public boolean needsReasoner();

	public void setReasoner(ReasonedLinkDatabase linkDatabase);

	/**
	 * Can return null if no configuration is possible.
	 */
	public JComponent getConfigurationPanel();

	public CheckConfiguration getConfiguration();

	public void setConfiguration(CheckConfiguration config);
	
	public void cancel();
}

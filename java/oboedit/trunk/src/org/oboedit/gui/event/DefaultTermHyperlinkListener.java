package org.oboedit.gui.event;

import javax.swing.JComponent;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.oboedit.controller.SelectionManager;

public class DefaultTermHyperlinkListener extends TermHyperlinkListener {

	protected JComponent source;

	public DefaultTermHyperlinkListener(JComponent source,
			LinkDatabase linkDatabase) {
		super(linkDatabase);
		this.source = source;
	}

	@Override
	public void actionSelected(String linkParam, String val) {
	}

	@Override
	public void linkSelected(String linkParam, Link link) {
		SelectionManager.getManager().select(source, link, false);
	}

	@Override
	public void termSelected(String linkParam, IdentifiedObject io) {
		if (io instanceof LinkedObject)
			SelectionManager.getManager().select(source, (LinkedObject) io);
	}

}

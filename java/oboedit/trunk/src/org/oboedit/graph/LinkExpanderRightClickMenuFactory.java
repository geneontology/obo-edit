package org.oboedit.graph;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;

import javax.swing.JMenuItem;

import org.obo.datamodel.Link;
import org.obo.datamodel.PathCapable;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;
import org.oboedit.gui.Selection;
import org.oboedit.gui.components.LinkDatabaseCanvas;

import edu.umd.cs.piccolo.event.PInputEvent;

import org.apache.log4j.*;

public class LinkExpanderRightClickMenuFactory implements RightClickMenuFactory {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(LinkExpanderRightClickMenuFactory.class);

	public List<JMenuItem> getMenuItems(final LinkDatabaseCanvas canvas,
			final PInputEvent e) {
		if (!(canvas.getLinkDatabase() instanceof ReasonedLinkDatabase)) {
			return null;
		}
		Selection tempSelection = canvas.getPickerSelection(e);
		if (tempSelection == null)
			return null;
		final Collection<Link> impliedLinks = new LinkedList<Link>();
		for (Link link : tempSelection.getLinks()) {
			if (TermUtil.isImplied(link)) {
				impliedLinks.add(link);
			}
		}
		if (impliedLinks.size() == 0)
			return null;
		JMenuItem expandAllItem = new JMenuItem("Expand all supporting links");
		expandAllItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Collection<PathCapable> supportingLinks = new HashSet<PathCapable>();
				for(Link link : impliedLinks) {
					supportingLinks.addAll(ReasonerUtil
						.getGivenSupportingLinks((ReasonedLinkDatabase) canvas
								.getLinkDatabase(), link));
				}
				canvas.addVisibleObjects(supportingLinks);
			}
		});

		JMenuItem expandImmediateItem = new JMenuItem(
				"Expand immediate supporting links");
		expandImmediateItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Collection<PathCapable> supportingLinks = new HashSet<PathCapable>();
				for(Link link : impliedLinks) {
					supportingLinks.addAll(ReasonerUtil
						.getImmediateSupportingLinks((ReasonedLinkDatabase) canvas
								.getLinkDatabase(), link));
				}
				canvas.addVisibleObjects(supportingLinks);
			}
		});

		LinkedList<JMenuItem> out = new LinkedList<JMenuItem>();
		out.add(expandAllItem);
		out.add(expandImmediateItem);
		return out;
	}
}

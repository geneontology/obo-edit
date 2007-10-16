package org.oboedit.gui.components;

import org.bbop.framework.AbstractGUIComponent;
import org.obo.datamodel.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.gui.event.*;

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedList;
import java.util.Set;
import java.net.URL;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

public class CrossProductInfoComponent extends AbstractGUIComponent {

	/**
	 * 
	 */
	private static final long serialVersionUID = -7919246476674947971L;

	protected JEditorPane crossProductPane = new JEditorPane("text/html",
			"<html></html>");

	protected JEditorPane referencePane = new JEditorPane("text/html",
			"<html></html>");

	protected HyperlinkListener linkListener = new HyperlinkListener() {
		public void hyperlinkUpdate(HyperlinkEvent e) {
			if (e.getEventType().equals(HyperlinkEvent.EventType.ACTIVATED)) {
				selectTerm(e.getURL(), SessionManager.getManager().getSession());
			}
		}
	};

	protected SelectionManager selectionManager = SelectionManager.getManager();

	protected SelectionListener termSelectListener = new SelectionListener() {
		public void selectionChanged(SelectionEvent e) {
			update();
		}
	};

	public void selectTerm(URL url, OBOSession history) {
		String id = url.getPath();
		OBOClass t = (OBOClass) history.getObject(id);
		List<LinkedObject> terms = new LinkedList<LinkedObject>();
		terms.add(t);
		SelectionManager.setGlobalSelection(SelectionManager
				.createSelectionFromTerms(null, terms, null, true));
	}

	public CrossProductInfoComponent(String id) {
		super(id);
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

		TitledBorder rborder = new TitledBorder("Referenced by");
		referencePane.setBorder(rborder);
		referencePane.setEditable(false);
		referencePane.setOpaque(false);

		TitledBorder cborder = new TitledBorder("Cross product defs");
		crossProductPane.setBorder(cborder);
		crossProductPane.setEditable(false);
		crossProductPane.setOpaque(false);

		referencePane.addHyperlinkListener(linkListener);
		crossProductPane.addHyperlinkListener(linkListener);
	}

	@Override
	public void init() {
		selectionManager.addSelectionListener(termSelectListener);
		update();
	}

	protected void update() {
		removeAll();
		IdentifiedObject ss = SelectionManager.getGlobalSelection()
				.getTermSubSelection();
		if (!(ss instanceof LinkedObject))
			return;
		LinkedObject obj = (LinkedObject) ss;
		Iterator it;
		Set<LinkedObject> genus = new HashSet<LinkedObject>();
		Set<Link> diff = new HashSet<Link>();
		Set<Link> pdiff = new HashSet<Link>();
		IdentifiedObject genusTerm = null;

		it = obj.getParents().iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (TermUtil.isIntersection(link)) {
				if (link.getType().equals(OBOProperty.IS_A)) {
					genusTerm = link.getParent();
				} else {
					pdiff.add(link);
				}
			}
		}

		boolean showCrossProductPanel = genusTerm != null || pdiff.size() > 0;
		if (showCrossProductPanel) {
			int fontsize = getFont().getSize();
			String fontname = getFont().getFamily();
			StringBuffer out = new StringBuffer();
			out.append("<html>\n");
			out.append("<head>\n");
			out.append("<style type='text/css'>\n");
			out.append("  * { font-size: " + fontsize + "pt; "
					+ "font-family: " + fontname + "; margin-top: 0; "
					+ "padding-top: 0; }\n");
			out.append("  body { font-size: " + fontsize + "pt; "
					+ "font-family: " + fontname + "; }\n");
			out.append("</style>\n");
			out.append("</head>\n");
			out.append("<body>\n");
			if (genusTerm != null) {
				out.append("<b>Genus</b> <a href='file:" + genusTerm.getID()
						+ "'>" + genusTerm + " (" + genusTerm.getID()
						+ ")</a><br><br>");
			} else {
				out.append("<b>No genus term defined!</b><br>");
				out.append("<i>It is legal to define a cross product with no "
						+ "genus (is_a) intersection defined in OBO-Edit, "
						+ "but it is not recommended, and your ontology will"
						+ "be incompatible with some tools.</i>");
			}
			if (pdiff.size() > 0) {
				out.append("<b>Discriminating Properties</b>\n");
				it = pdiff.iterator();
				out.append("<ul>");
				while (it.hasNext()) {
					Link link = (Link) it.next();
					IdentifiedObject io = link.getParent();
					out.append("<li><i><a href='file:" + link.getType().getID()
							+ "'>" + link.getType().getID()
							+ "</a></i><br><a href='file:" + io.getID() + "'>"
							+ io + " (" + io.getID() + ")</a>");
				}
				out.append("</ul>");
			}
			out.append("</body></html>");
			crossProductPane.setText(out.toString());
			add(crossProductPane);
		}

		it = obj.getChildren().iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (TermUtil.isIntersection(link)) {
				if (link.getType().equals(OBOProperty.IS_A)) {
					genus.add(link.getChild());
				} else
					diff.add(link);
			}
		}
		boolean showReferencePanel = genus.size() > 0 || diff.size() > 0;

		if (showReferencePanel) {
			int fontsize = getFont().getSize();
			String fontname = getFont().getFamily();
			StringBuffer out = new StringBuffer();
			out.append("<html>\n");
			out.append("<head>\n");
			out.append("<style type='text/css'>\n");
			out.append("  * { font-size: " + fontsize + "pt; "
					+ "font-family: " + fontname + "; margin-top: 0;"
					+ "padding-top: 0; }\n");
			out.append("  body { font-size: " + fontsize + "pt; "
					+ "font-family: " + fontname + "; }\n");
			out.append("</style>\n");
			out.append("</head>\n");
			out.append("<body>\n");
			if (genus.size() > 0) {
				out.append("<b>Genus of</b>\n");
				it = genus.iterator();
				out.append("<ul>");
				while (it.hasNext()) {
					IdentifiedObject io = (IdentifiedObject) it.next();
					out.append("<li><a href='file:" + io.getID() + "'>" + io
							+ " (" + io.getID() + ")</a>");
				}
				out.append("</ul>");
			}
			if (diff.size() > 0) {
				out.append("<b>Differentiates</b>\n");
				it = diff.iterator();
				out.append("<ul>");
				while (it.hasNext()) {
					Link link = (Link) it.next();
					IdentifiedObject io = link.getChild();
					out.append("<li><a href='file:" + io.getID() + "'>" + io
							+ " (" + io.getID() + ")</a> by <a href='file:"
							+ link.getType().getID() + "'>"
							+ link.getType().getID() + "</a>");
				}
				out.append("</ul>");
			}
			out.append("</body></html>");
			referencePane.setText(out.toString());
			add(referencePane);
		}

		if (!showCrossProductPanel && !showReferencePanel) {
			int fontsize = getFont().getSize();
			String fontname = getFont().getFamily();
			StringBuffer out = new StringBuffer();
			out.append("<html>\n");
			out.append("<head>\n");
			out.append("<style type='text/css'>\n");
			out.append("  * { font-size: " + fontsize + "pt; "
					+ "font-family: " + fontname + "; margin-top: 0;"
					+ "padding-top: 0; }\n");
			out.append("  body { font-size: " + fontsize + "pt; "
					+ "font-family: " + fontname + "; }\n");
			out.append("</style>\n");
			out.append("</head>\n");
			out.append("<body>\n");
			out.append("<b>No cross product definitions or references</b>");
			out.append("</body></html>");
			JLabel noDataLabel = new JLabel(out.toString());
			add(noDataLabel);
		}

		validate();
		repaint();
	}

	@Override
	public void cleanup() {
		selectionManager.removeSelectionListener(termSelectListener);
	}

	@Override
	public String getName() {
		return "Cross Product Info Plugin";
	}
}

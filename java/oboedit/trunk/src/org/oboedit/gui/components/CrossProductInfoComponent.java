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
		Selection gs = SelectionManager.getGlobalSelection();

		// objs that have xp definitions
		TinySet<LinkedObject> xpObjs = new TinySet<LinkedObject>();

		// objs that form the genus of other xp definitions
		TinySet<LinkedObject> xpGenusObjs = new TinySet<LinkedObject>();
		HashMap<LinkedObject, LinkedObject> objByGenus = new HashMap<LinkedObject, LinkedObject>();

		// objs that form the differentium obj of other xp definitions
		// we also include *potential* differentia
		// (for making new xps)
		// any selected obj (that is not itself an xp?) goes here
		TinySet<LinkedObject> xpDiffObjs = new TinySet<LinkedObject>();
		HashMap<LinkedObject, LinkedObject> objByDiff = new HashMap<LinkedObject, LinkedObject>();

		// build sets
		for (PathCapable io : gs.getAllSelectedObjects()) {
			System.out.println("selected: " + io + " ? "+
					(io instanceof LinkedObject));

			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject)io;
				if (TermUtil.isIntersection(lo)) {
//					TermUtil.isGenusDifferentia(lo)) {
					xpObjs.add(lo);
					xpGenusObjs.add((LinkedObject)(ReasonerUtil.getGenus((OBOClass)lo)));
					for (Link linkUp : lo.getChildren()) {
						if (TermUtil.isIntersection(linkUp)) {
							if (linkUp.getType().equals(OBOProperty.IS_A)) {
								xpGenusObjs.add(linkUp.getChild());
								objByGenus.put(linkUp.getChild(),lo);
							}
							else {
								xpDiffObjs.add(linkUp.getChild());
								objByDiff.put(linkUp.getChild(),lo);
							}
						}
					}
				}
				else {
					// treat as *potential* differentium
					// TODO: decide, do this for all?
					// we can have recursive differentia
					xpDiffObjs.add(lo);
				}
			}
		}

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

		boolean showCrossProductPanel = xpObjs.size() > 0;
		if (showCrossProductPanel) {

			System.out.println("about to write xps.."+xpObjs);

			for (LinkedObject xp : xpObjs) {

				System.out.println("xp = "+xp);
				IdentifiedObject xpGenus = ReasonerUtil.getGenus((OBOClass)xp);
				System.out.println("genus = "+xpGenus);
				Collection<Link> xpDiffs = ReasonerUtil.getDifferentia((OBOClass)xp);
				System.out.println("diffs = "+xpDiffs);
				out.append("<h3>" + xp + "</h3>");
				out.append("<b>Genus:</b> " + objectHref(xpGenus)
						+ "<br><br>");
				out.append("<b>Discriminating Properties</b>\n");
				out.append("<ul>");

				for (Link xpDiff : xpDiffs) {
					LinkedObject io = xpDiff.getParent();

					xpDiffObjs.add(io);
					out.append("<li><i><a href='file:" + xpDiff.getType().getID()
							+ "'>" + xpDiff.getType().getID()
							+ "</a></i><br><a href='file:" + io.getID() + "'>"
							+ io + " (" + io.getID() + ")</a>");
				}
				out.append("</ul>");
			}
		}
		else {
			out.append("no xps");
		}

		out.append("<h3>In</h3>");
		out.append("<ul>");

		// show xps in which this is a genus or differentium
		for (PathCapable io : gs.getAllSelectedObjects()) {
			System.out.println("selected: " + io + " ? "+
					(io instanceof LinkedObject));
			if (io instanceof LinkedObject) {
				LinkedObject xo = (LinkedObject)io;
				out.append("<li>");
				out.append(objectHref(xo));

				out.append("</li>");
			}

		}
		out.append("</ul>");

		out.append("</body></html>");

		crossProductPane.setText(out.toString());
		//add(crossProductPane);
		add(new JScrollPane(crossProductPane,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED), "Center");

		validate();
		repaint();
	}

	public String objectHref(IdentifiedObject io) {
		return "<a href='file:" + io.getID()
		+ "'>" + io + " (" + io.getID()
		+ ")</a>";
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
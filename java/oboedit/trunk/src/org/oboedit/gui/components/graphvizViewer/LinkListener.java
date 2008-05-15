package org.oboedit.gui.components.graphvizViewer;

import java.util.Collection;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;

public class LinkListener implements HyperlinkListener {	
	
	private JComponent graphvizCanvas;

	public LinkListener(GraphvizCanvas graphvizCanvas) {
		this.graphvizCanvas = graphvizCanvas;
	}
	
	
	
	public void hyperlinkUpdate(HyperlinkEvent e) {
		if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
			JEditorPane pane = (JEditorPane) e.getSource();
			LinkedObject term = (LinkedObject) SessionManager.getManager().getSession().getObject(e.getURL().getPath());
			Collection<LinkedObject> v = new Vector();
			Iterator<Link> it = term.getParents().iterator();
			while (it.hasNext()) {
				v.add(it.next().getParent());
			}
			v.add(term);
			SelectionManager.getManager().selectTerms(graphvizCanvas, v);
		}
	}
}


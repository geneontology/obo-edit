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

import org.apache.log4j.*;

public class LinkListener implements HyperlinkListener {	

	//initialize logger
	protected final static Logger logger = Logger.getLogger(LinkListener.class);
	
	private JComponent graphvizCanvas;

	public LinkListener(GraphvizCanvas graphvizCanvas) {
		this.graphvizCanvas = graphvizCanvas;
	}
	
	
	
	public void hyperlinkUpdate(HyperlinkEvent e) {
		if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
			logger.debug("LinkListener: hyperlinkUpdate: e.getURL() = " + e.getURL() );
			JEditorPane pane = (JEditorPane) e.getSource();
			LinkedObject term = (LinkedObject) SessionManager.getManager().getSession().getObject(e.getURL().getPath());
			
			Collection<LinkedObject> v = new Vector();
			Iterator<Link> it = term.getParents().iterator();
			
			
			// Removed so that when a term in the graphviz canvas is 
			//clicked only the term clicked is shown in the OTE and not the parent. 
//			while (it.hasNext()) {					
//				v.add(it.next().getParent());  
//			}
			v.add(term);
			SelectionManager.getManager().selectTerms(graphvizCanvas, v);
		}
	}
}


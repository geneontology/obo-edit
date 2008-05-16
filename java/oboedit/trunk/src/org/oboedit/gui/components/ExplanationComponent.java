package org.oboedit.gui.components;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.util.ObjectUtil;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.ExplanationUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.gui.event.*;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JCheckBox;
import javax.swing.JEditorPane;
import javax.swing.JScrollPane;
import java.util.Iterator;

import org.apache.log4j.*;

public class ExplanationComponent extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ExplanationComponent.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected JEditorPane field = new JEditorPane("text/html", "<html></html>");
	protected JScrollPane scroller = new JScrollPane(field,
			JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

	protected SelectionListener termSelectListener = new SelectionListener() {
		public void selectionChanged(SelectionEvent e) {
			update(true);
		}
	};

	protected SelectionManager selectionManager = SelectionManager.getManager();

	TermHyperlinkListener listener = new TermHyperlinkListener() {

		@Override
		public void linkSelected(String param, Link link) {
			if (ObjectUtil.equals(param, "EXPAND")) {
				subExplanations.add(link);
				Collection links = SelectionManager.getGlobalSelection()
						.getLinks();
				field.setText(getExplanation(links, subExplanations));
				field.scrollToReference(link.getChild().getID().replace(':',
						'_')
						+ "-"
						+ link.getType().getID().replace(':', '_')
						+ "-"
						+ link.getParent().getID().replace(':', '_'));
			} else if (ObjectUtil.equals(param, "HIDE")) {
				subExplanations.remove(link);
				Collection links = SelectionManager.getGlobalSelection()
						.getLinks();
				field.setText(getExplanation(links, subExplanations));
				field.scrollToReference(link.getChild().getID().replace(':',
						'_')
						+ "-"
						+ link.getType().getID().replace(':', '_')
						+ "-"
						+ link.getParent().getID().replace(':', '_'));
			} else {
				SelectionManager.selectLink(ExplanationComponent.this, link,
						true);
			}
		}

		@Override
		public void termSelected(String param, IdentifiedObject io) {
			if (io instanceof LinkedObject) {
				SelectionManager.selectTerm(ExplanationComponent.this,
						(LinkedObject) io, true);
			}
		}

		@Override
		public void actionSelected(String param, String val) {
			// do nothing
		}

	};

	protected Collection<Link> subExplanations;

	protected JCheckBox documentationCheckbox = new JCheckBox(
			"Show verbose explanations");

	public ExplanationComponent(String id) {
		super(id);
		setLayout(new BorderLayout());
		field.setEditable(false);
		field.addHyperlinkListener(listener);
		add(scroller, "Center");
		add(documentationCheckbox, "South");
		documentationCheckbox.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				update(false);
			}

		});
	}

	protected void update(boolean resetExpansionState) {
		listener.setLinkDatabase(SessionManager.getManager().getReasoner());
		if (resetExpansionState)
			subExplanations = new LinkedList<Link>();
		Collection links = SelectionManager.getGlobalSelection().getLinks();
		field.setText(getExplanation(links, subExplanations));
	}

	protected String getExplanation(Collection links, Collection subExplanations) {
		StringBuffer out = new StringBuffer();
		ReasonedLinkDatabase reasoner = SessionManager.getManager()
				.getReasoner();
		Map cache = new HashMap();
		Iterator it = links.iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			out
					.append(ExplanationUtil.getDescriptionReasoned(reasoner,
							SelectionManager.getGlobalSelection().getLinkDatabase(),
							link, subExplanations, cache, documentationCheckbox
									.isSelected()));
		}
		return out.toString();
	}

	@Override
	public void init() {
		selectionManager.addSelectionListener(termSelectListener);
		update(true);
	}

	@Override
	public void cleanup() {
		selectionManager.removeSelectionListener(termSelectListener);
	}

	@Override
	public String getName() {
		return "Explanation Plugin";
	}
}

package org.oboedit.gui.widget;

import java.awt.Color;
import java.awt.Component;
import java.awt.KeyboardFocusManager;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.ComboBoxEditor;
import javax.swing.ComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.ListCellRenderer;
import javax.swing.ToolTipManager;
import javax.swing.border.Border;
import javax.swing.border.LineBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.plaf.basic.BasicComboBoxRenderer;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;

import org.bbop.util.ObjectUtil;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.filters.SearchCriterion;
import org.obo.query.Query;
import org.obo.query.QueryEngine;
import org.obo.query.StringQuery;
import org.obo.query.impl.NamespaceQuery;
import org.obo.query.impl.ScoredCriterionHit;
import org.obo.query.impl.ScoredCriterionQuery;
import org.obo.query.impl.ScoredQueryHit;
import org.obo.query.impl.ScoredStringHit;
import org.obo.query.impl.SearchHit;
import org.obo.query.impl.TextQuery;
import org.obo.query.impl.TextSearchHit;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.DropUtil;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.Selection;

public class SessionAutocompleteBox extends JComboBox {

	protected class SessionAutoTextField extends JTextField implements
			ComboBoxEditor {

		protected boolean updating = false;

		/*
		 * DocumentListener docListener = new DocumentListener() {
		 * 
		 * public void changedUpdate(DocumentEvent e) { update(); }
		 * 
		 * public void insertUpdate(DocumentEvent e) { update(); }
		 * 
		 * public void removeUpdate(DocumentEvent e) { update(); } };
		 */
		KeyListener keyListener = new KeyListener() {

			public void keyPressed(KeyEvent e) {
				if (!e.isActionKey())
					update();
			}

			public void keyReleased(KeyEvent e) {
				if (!e.isActionKey())
					update();
			}

			public void keyTyped(KeyEvent e) {
				update();
			}

		};

		@Override
		public synchronized void addActionListener(ActionListener l) {
		}

		protected FocusListener focusListener = new FocusListener() {
			public void focusLost(FocusEvent e) {
				if (SessionAutocompleteBox.this.getSelectedItem() == null
						&& getText().length() >= getMinMatchLength()
						&& getItemCount() > 0) {
					setSelectedIndex(0);
					commit();
				} else if (getSelectedItem() != null) {
					String s = ((SearchHit<IdentifiedObject>) getSelectedItem())
							.getHit().getName();
					setText(s);
				}
			}

			public void focusGained(FocusEvent e) {
			}
		};

		public SessionAutoTextField() {
			// getDocument().addDocumentListener(docListener);
			addKeyListener(keyListener);
			addFocusListener(focusListener);
			getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(
					KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "commit");
			getActionMap().put("commit", new AbstractAction() {

				public void actionPerformed(ActionEvent e) {
					commit();
				}
			});
		}

		public void commit() {
			/*
			 * if (getText().length() == 0) setSelectedItem(null); else if
			 * (lastHits.size() > 0) setSelectedItem(lastHits.get(0));
			 */
			ActionEvent e = new ActionEvent(SessionAutocompleteBox.this,
					(int) (Math.random() * Integer.MAX_VALUE), "commit");
			for (ActionListener listener : commitListeners) {
				listener.actionPerformed(e);
			}
		}

		protected void update() {
			updating = true;
			removeKeyListener(keyListener);
			// getDocument().removeDocumentListener(docListener);
			try {
				String text = getDocument().getText(0,
						getDocument().getLength());
				updateSearchText(text, true);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
			addKeyListener(keyListener);
			// getDocument().addDocumentListener(docListener);
			updating = false;
		}

		public Component getEditorComponent() {
			return this;
		}

		public Object getItem() {
			Object out = lastHits.size() == 0 ? null : lastHits.get(0);
			System.err.println("called getItem(): returned " + out);
			return out;
		}

		@Override
		public void setText(String t) {
			if (updating)
				return;
			updating = true;
			super.setText(t);
			updating = false;
		}

		public void setItem(Object anObject) {
			if (updating)
				return;
			if (anObject == null)
				setText("");
			else
				setText(((SearchHit<IdentifiedObject>) anObject).getHit()
						.getName());
		}

	}

	/*
	 * protected class AutoTextFieldEditor extends BasicComboBoxEditor {
	 * 
	 * private SessionAutoTextField getAutoTextFieldEditor() { return
	 * (SessionAutoTextField) editor; }
	 * 
	 * AutoTextFieldEditor() { editor = new SessionAutoTextField(); } }
	 * 
	 * protected class SessionAutoTextField extends JTextField {
	 * 
	 * protected boolean isCaseSensitive;
	 * 
	 * 
	 * public SessionAutoTextField() { isCaseSensitive = false;
	 * addKeyListener(new KeyAdapter() { @Override public void
	 * keyPressed(KeyEvent e) { if (e.getKeyCode() == KeyEvent.VK_ENTER) {
	 * commit(); } } }); addFocusListener(new FocusAdapter() { @Override public
	 * void focusLost(FocusEvent e) { commit(); } }); init(); }
	 * 
	 * public void commit() { if (lastHits.size() > 0 &&
	 * !ObjectUtil.equals(lastHits.get(0), getSelectedItem())) {
	 * setText(lastHits.get(0).toString()); setSelectedItem(lastHits.get(0)); } //
	 * setSelectedIndex(0); }
	 * 
	 * public void setText(String text) { if (!changing) super.setText(text); }
	 * 
	 * private void init() { getDocument().addDocumentListener(new
	 * DocumentListener() {
	 * 
	 * protected void update(DocumentEvent e) { if (changing ||
	 * ignoreDocumentUpdates) return; changing = true; try { String text =
	 * e.getDocument().getText(0, e.getDocument().getLength());
	 * updateSearchText(text, true); } catch (Exception ex) {
	 * ex.printStackTrace(); } changing = false; }
	 * 
	 * public void changedUpdate(DocumentEvent e) { update(e); }
	 * 
	 * public void insertUpdate(DocumentEvent e) { update(e); }
	 * 
	 * public void removeUpdate(DocumentEvent e) { update(e); } }); setText(""); } }
	 */

	protected FocusListener focusListener = new FocusListener() {

		public void focusGained(FocusEvent e) {
		}

		public void focusLost(FocusEvent e) {
			hidePopup();
		}

	};

	protected String formatHTML(SearchHit hit) {
		if (hit instanceof TextSearchHit)
			return formatHTML((TextSearchHit) hit);
		else if (hit instanceof ScoredQueryHit)
			return formatHTML((ScoredQueryHit) hit);
		else
			return null;
	}

	protected String formatHTML(TextSearchHit hit) {
		StringBuffer hitText = new StringBuffer(hit.getHitText());
		Document d = ((JTextComponent) getEditor()).getDocument();
		String searchText;
		try {
			searchText = d.getText(0, d.getLength());
		} catch (BadLocationException e) {
			searchText = "ERROR";
		}
		int finalPos = hit.getHitPosition() + searchText.length();
		if (finalPos >= hitText.length())
			hitText.append("</b>");
		else
			hitText.insert(finalPos, "</b>");
		hitText.insert(hit.getHitPosition(), "<b>");
		if (hit.getHitType() == TextQuery.HitType.NAME)
			return hitText.toString();
		else
			return "<i>" + hit.getHit().getName() + "</i>";
	}

	protected ListCellRenderer renderer = new BasicComboBoxRenderer() {

		protected JLabel label = new JLabel();

		@Override
		public Component getListCellRendererComponent(JList list, Object value,
				int index, boolean isSelected, boolean cellHasFocus) {
			if (isSelected) {
				label.setOpaque(true);
				label.setBackground(Preferences.defaultSelectionColor());
			} else
				label.setOpaque(false);
			SearchHit hit = (SearchHit) value;
			String str = formatHTML(hit);
			label.setText("<html>" + str + "</html>");
			label
					.setToolTipText("<html>" + formatDetailedHTML(hit)
							+ "</html>");
			return label;
		}

	};

	private String formatDetailedHTML(SearchHit hit) {
		if (hit instanceof ScoredQueryHit
				&& query instanceof ScoredCriterionQuery) {
			ScoredCriterionQuery scq = (ScoredCriterionQuery) query;
			ScoredQueryHit sqhit = (ScoredQueryHit) hit;
			List<SearchCriterion<IdentifiedObject, String>> criteria = new ArrayList<SearchCriterion<IdentifiedObject, String>>(
					scq.getCriteria());
			Collections.sort(criteria, scq.getWeightComparator());
			StringBuffer out = new StringBuffer();
			out.append("<b>Matched "+hit.getHit()+"</b><br><br>\n");
			out.append("<table border=0 cellspacing=0>\n");
			for (SearchCriterion criterion : criteria) {
				Collection<ScoredCriterionHit> schits = sqhit
						.getHitsForCriterion(criterion);
				out.append("<tr valign=top><td><b>"
						+ criterion.getID() + "</b></td><td>");
				for (ScoredCriterionHit schit : schits) {
					out.append(formatHTML(schit)+"<br>");
				}
				out.append("</td>\n");
			}
			out.append("</table>\n");
			return out.toString();
		} else
			return null;
	}

	protected String formatHTML(ScoredCriterionHit nameHit) {
		StringBuffer buffer = new StringBuffer(nameHit.getSearchString());
		List<ScoredStringHit> hitList = new ArrayList<ScoredStringHit>(nameHit
				.getHits());
		Collections.sort(hitList, new Comparator<ScoredStringHit>() {
			public int compare(ScoredStringHit o1, ScoredStringHit o2) {
				return o2.getHitPos() - o1.getHitPos();
			}
		});
		for (ScoredStringHit strHit : hitList) {
			int startIndex = strHit.getHitPos();
			int endIndex = startIndex + strHit.getUserString().length();
			if (endIndex >= buffer.length())
				buffer.append("</b>");
			else
				buffer.insert(endIndex, "</b>");
			buffer.insert(startIndex, "<b>");
		}
		return buffer.toString();

	}

	protected String formatHTML(ScoredQueryHit hit) {
		Collection<ScoredCriterionHit> chits = hit.getHitsForCriterion("name");
		if (chits.size() == 0) {
			return "<i>" + hit.getHit().getName() + "</i>";
		} else {
			ScoredCriterionHit nameHit = chits.iterator().next();
			return formatHTML(nameHit);
		}
	}

	protected DropTargetListener dropListener = new DropTargetListener() {
		LineBorder border = new LineBorder(Color.black, 2);

		Border oldBorder;

		public void dragEnter(DropTargetDragEvent dtde) {
			if (!allowDrop(DropUtil.getSelection(dtde))) {
				oldBorder = null;
				dtde.rejectDrag();
			}
			oldBorder = getBorder();
			setBorder(border);
		}

		public void dragExit(DropTargetEvent dte) {
			if (oldBorder != null)
				setBorder(oldBorder);
		}

		public void dragOver(DropTargetDragEvent dtde) {
			if (!allowDrop(DropUtil.getSelection(dtde))) {
				oldBorder = null;
				dtde.rejectDrag();
			}
		}

		public void drop(DropTargetDropEvent dtde) {
			LinkedObject term = DropUtil.getSelection(dtde)
					.getTermSubSelection();
			if (term != null) {
				setTerm(term);
				setBorder(oldBorder);
			}
		}

		public void dropActionChanged(DropTargetDragEvent dtde) {
		}

	};

	protected Collection<ActionListener> commitListeners = new LinkedList<ActionListener>();

	protected int minMatchLength = 1;

	protected int maxResults = 10;

	public void addCommitListener(ActionListener listener) {
		commitListeners.add(listener);
	}

	public void removeCommitListener(ActionListener listener) {
		commitListeners.remove(listener);
	}

	protected boolean allowDrop(Selection selection) {
		return selection.getTermSubSelection() != null
				&& !ObjectUtil.equals(selection.getTermSubSelection(),
						getSelectedItem());
	}

	public void setQuery(
			StringQuery<IdentifiedObject, SearchHit<IdentifiedObject>> q) {
		this.query = q;
	}

	public void setTerm(LinkedObject lo) {
		if (lo == null)
			setSelectedItem(null);
		else if (lo instanceof OBOClass) {
			OBOClass oboClass = (OBOClass) lo;
			setSelectedItem(query.convertToOutputType(oboClass));
		}

	}

	public LinkedObject getTerm() {
		SearchHit searchHit = (SearchHit) getSelectedItem();
		if (searchHit == null)
			return null;
		else
			return (LinkedObject) searchHit.getHit();
	}

	@Override
	public void setSelectedItem(Object anObject) {
		if (anObject == null || !(anObject instanceof SearchHit)) {
			super.setSelectedItem(null);
		} else {
			SearchHit<IdentifiedObject> hit = (SearchHit<IdentifiedObject>) anObject;
			setPrototypeDisplayValue(hit);
			if (searchSet.contains(hit.getHit())) {
				super.setSelectedItem(hit);
			}
		}
		if (isEditable() && getEditor() != null) {
			configureEditor(getEditor(), getSelectedItem());
		}
	}

	protected StringQuery query;

	protected List<IdentifiedObject> searchSet;
	
	public SessionAutocompleteBox() {
		this(null);
	}

	public SessionAutocompleteBox(QueryEngine engine) {
		this.engine = engine;
		refreshSearchSet();
		setEditor(new SessionAutoTextField());
		setModel(new QueryListModel());
		createDefaultQuery();
		setEditable(true);
		setRenderer(renderer);
		setTerm(null);
		setDropTarget(new DropTarget(this, dropListener));
		addFocusListener(focusListener);
	}

	protected void createDefaultQuery() {
		/*
		 * query = new TextQuery();
		 * 
		 * ((TextQuery) query).setSearchTypes(TextQuery.HitType.values());
		 */
		query = new ScoredCriterionQuery(true);
		// TODO Auto-generated method stub

	}

	@Override
	public void setFocusTraversalKeysEnabled(boolean focusTraversalKeysEnabled) {
		super.setFocusTraversalKeysEnabled(focusTraversalKeysEnabled);
		if (getEditor() instanceof Component)
			((Component) getEditor())
					.setFocusTraversalKeysEnabled(focusTraversalKeysEnabled);
	}

	protected List<SearchHit<IdentifiedObject>> getHits(String s) {
		List<SearchHit<IdentifiedObject>> out;
		if (s.length() < getMinMatchLength()) {
			out = new LinkedList<SearchHit<IdentifiedObject>>();
			for (IdentifiedObject io : searchSet) {
				out.add((SearchHit<IdentifiedObject>) query
						.convertToOutputType(io));
			}
		} else {
			query.setSearchString(s);
			out = (List<SearchHit<IdentifiedObject>>) getEngine().query(searchSet,
					query);
		}
		return out;
	}

	public void refreshSearchSet() {
		NamespaceQuery query = new NamespaceQuery();
		QueryEngine engine = getEngine();
		Collection<OBOClass> searchSet = engine.query(query);
		setSearchSet(searchSet);
	}

	public void setSearchSet(Collection<OBOClass> searchSet) {
		this.searchSet = new LinkedList<IdentifiedObject>(searchSet);
		if (searchSet.size() == 0)
			System.err.println("EMPTY SEARCH SET!!!");
	}

	protected List<SearchHit<IdentifiedObject>> lastHits = new LinkedList<SearchHit<IdentifiedObject>>();

	protected void updateSearchText(String str, boolean showPopup) {
		lastHits = getHits(str);
		((QueryListModel) getModel()).update();
		if (lastHits.size() == 0)
			super.setSelectedItem(null);
		else {
			super.setSelectedItem(lastHits.get(0));
			if (isShowing() && showPopup && hasMeaningfulFocus()) {
				hidePopup();
				showPopup();
			}
		}
	}

	public boolean hasMeaningfulFocus() {
		return isFocusOwner() || ((JComponent) getEditor()).isFocusOwner();
	}

	protected QueryEngine engine;
	
	protected QueryEngine getEngine() {
		if (engine != null)
			return engine;
		else
			return SessionManager.getManager().getQueryEngine();
	}

	protected boolean changing = false;

	protected class QueryListModel implements ComboBoxModel {

		protected Collection<ListDataListener> listeners = new LinkedList<ListDataListener>();

		protected Object selected;

		public void addListDataListener(ListDataListener l) {
			listeners.add(l);
		}

		public void update() {
			ListDataEvent event = new ListDataEvent(
					SessionAutocompleteBox.this,
					ListDataEvent.CONTENTS_CHANGED, 0, getSize());
			for (ListDataListener listener : listeners) {
				listener.contentsChanged(event);
			}
		}

		public Object getElementAt(int index) {
			return lastHits.get(index);
		}

		public int getSize() {
			if (getMaxResults() < 0)
				return lastHits.size();
			else
				return Math.min(getMaxResults(), lastHits.size());
		}

		public void removeListDataListener(ListDataListener l) {
			listeners.remove(l);
		}

		public Object getSelectedItem() {
			return selected;
		}

		public void setSelectedItem(Object anItem) {
			selected = anItem;
		}

	}

	public int getMaxResults() {
		return maxResults;
	}

	public void setMaxResults(int maxResults) {
		this.maxResults = maxResults;
	}

	public int getMinMatchLength() {
		return minMatchLength;
	}

	public void setMinMatchLength(int minMatchLength) {
		this.minMatchLength = minMatchLength;
	}
}

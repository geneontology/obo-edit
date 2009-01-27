package org.oboedit.gui.components;

import java.awt.*;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.tree.*;

import org.bbop.swing.*;
import org.bbop.swing.widget.AutocompleteBox;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.history.*;
import org.obo.postcomp.PostcompUtil;
import org.obo.query.QueryEngine;
import org.obo.util.HistoryUtil;
import org.obo.util.IDUtil;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.IDManager;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.AbstractTextEditComponent;
import org.oboedit.gui.DropUtil;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.Selection;
import org.oboedit.gui.TermAutocompleteModel;
import org.oboedit.gui.event.RootChangeEvent;
import org.oboedit.gui.event.RootChangeListener;

import org.apache.log4j.*;

public class CrossProductEditorComponent extends AbstractTextEditComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CrossProductEditorComponent.class);

	protected class IntersectionPanelFocusPolicy extends
	LayoutFocusTraversalPolicy {
		@Override
		public boolean accept(Component aComponent) {
			if (aComponent instanceof JComboBox
					|| SwingUtilities.getAncestorOfClass(
							AutocompleteBox.class, aComponent) != null)
				return super.accept(aComponent);
			else
				return false;
		}

		@Override
		public Component getDefaultComponent(Container aContainer) {
			return genusField;
		}
	}

	protected class RelationshipLinePanel extends JPanel {

		//trash can icon
		protected Icon deleteIcon = Preferences
		.loadLibraryIcon("trashcan.gif");
		protected JButton deleteButton = new JButton(deleteIcon);


		//cotton ball icon - select genus term
		protected Icon selectIcon = Preferences
		.loadLibraryIcon("selector.gif");
		JButton selectButton = new JButton(selectIcon);

		protected AutocompleteBox<IdentifiedObject> parentBox = new AutocompleteBox<IdentifiedObject>(
				new TermAutocompleteModel());

		protected JComboBox propertyBox = new JComboBox();

		protected ActionListener selectActionListener;

		public RelationshipLinePanel() {
			getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(
					KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, false),
					"escapeLine");
			getActionMap().put("escapeLine", new AbstractAction() {
				public void actionPerformed(ActionEvent e) {
					if (getProperty() == null || getParentTerm() == null) {
						Container parent = RelationshipLinePanel.this
						.getParent();
						removeLine(RelationshipLinePanel.this);
						Component lastComp = getLastRelationshipLine();
						if (lastComp != null)
							lastComp.requestFocus();
					}
				}

			});
			setOpaque(false);
			propertyBox.getInputMap().put(
					KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "commit");
			propertyBox.getActionMap().put("commit", commitListener);
			parentBox.setFocusTraversalKeysEnabled(false);
			parentBox.addCommitListener(commitListener);


			deleteButton.setPreferredSize(new Dimension(20, 20));
			selectButton.setPreferredSize(new Dimension(20, 20));

			deleteButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					removeLine(RelationshipLinePanel.this);
				}
			});

			setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
			for (OBOProperty property : TermUtil
					.getRelationshipTypes(SessionManager.getManager()
							.getSession())) {
				propertyBox.addItem(property);
			}
			add(Box.createHorizontalStrut(30));
			add(propertyBox);
			add(Box.createHorizontalStrut(10));
			add(parentBox);
			add(Box.createHorizontalStrut(10));
			selectButton.setToolTipText("Select genus term");
			add(selectButton);
			deleteButton.setToolTipText("Delete relation");
			add(deleteButton);
		}

		public OBOClass getParentTerm() {
			return (OBOClass) parentBox.getValue();
		}

		public OBOProperty getProperty() {
			return (OBOProperty) propertyBox.getSelectedItem();
		}

		protected void removeLine(RelationshipLinePanel panel) {
			Container parent = getParent();
			parent.remove(panel);
			parent.repaint();
		}

		public void setParentTerm(final OBOClass parentTerm) {
//			logger.debug("CrossProductEditorComponent.setParentTerm parentTerm: " + parentTerm);
			parentBox.setValue(parentTerm);
			if (selectActionListener != null)
				selectButton.removeActionListener(selectActionListener);
			selectActionListener = new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					SelectionManager.selectTerm(CrossProductEditorComponent.this,
							parentTerm);
				}
			};
			selectButton.addActionListener(selectActionListener);
		}

		public void setProperty(OBOProperty property) {
			propertyBox.setSelectedItem(property);
		}

		public void tabIn() {
			propertyBox.requestFocus();
		}
	}



	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	// protected OBOClass genusTerm;

	protected Collection<ActionListener> actionListeners = new LinkedList<ActionListener>();

	protected JButton dropButton = new JButton(
	"Click (or drop a term) here to add new differentia");

	protected DropTargetListener dropDiscriminatingListener = new DropTargetListener() {
		protected LineBorder border = new LineBorder(Color.black, 2);

		protected Border oldBorder;

		public void dragEnter(DropTargetDragEvent dtde) {
			if (!allowDrop(DropUtil.getSelection(dtde))) {
				oldBorder = null;
				dtde.rejectDrag();
			}
			oldBorder = dropButton.getBorder();
			dropButton.setBorder(border);
		}

		public void dragExit(DropTargetEvent dte) {
			if (oldBorder != null)
				dropButton.setBorder(oldBorder);
		}

		public void dragOver(DropTargetDragEvent dtde) {
			if (!allowDrop(DropUtil.getSelection(dtde))) {
				oldBorder = null;
				dtde.rejectDrag();
			}
		}

		public void drop(DropTargetDropEvent dtde) {
			JPopupMenu menu = new JPopupMenu();

			final LinkedObject term = DropUtil.getSelection(dtde)
			.getTermSubSelection();
			OBOSession session = SessionManager.getManager().getSession();

			JMenuItem genusItem = new JMenuItem("Set genus term");
			genusItem.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					setGenus((OBOClass) term);
				}
			});
			menu.add(genusItem);

			menu.addSeparator();

			Iterator it = TermUtil.getRelationshipTypes(session).iterator();
			while (it.hasNext()) {
				final OBOProperty prop = (OBOProperty) it.next();

				if (prop.equals(OBOProperty.IS_A))
					continue;
				JMenuItem item = new JMenuItem(
						"Add discriminating term with type " + prop.getID());
				item.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						addDiscriminating((OBOClass) term, prop);
					}
				});
				menu.add(item);
			}
			menu.show(dropButton, (int) dtde.getLocation().getX(), (int) dtde
					.getLocation().getY());
			dropButton.setBorder(oldBorder);

		}

		public void dropActionChanged(DropTargetDragEvent dtde) {
		}

	};

	protected JPanel editorPanel = new JPanel();

	protected JScrollPane editorScroller = new JScrollPane(editorPanel,
			JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

	protected IntersectionPanelFocusPolicy focusPolicy = new IntersectionPanelFocusPolicy();

	protected AutocompleteBox<IdentifiedObject> genusField = new AutocompleteBox<IdentifiedObject>(
			new TermAutocompleteModel());

	protected JLabel genusLabel = new JLabel("Intersection Genus");

	// protected JButton genusButton = new JButton("<no genus specified>");

	protected JPanel genusPanel = new JPanel();

	protected JPanel linkListPanel = new JPanel();

	protected JLabel notLoadedLabel = new JLabel("No term selected");

	protected OBOClass oboClass;

	protected JPanel relPanel = new JPanel();

	protected JTextField nameField = new JTextField();

	protected JLabel idField = new JLabel();

	protected JCheckBox anonymousCheckbox = new JCheckBox("anonymous");

	protected boolean showNameFields;

	protected Action commitListener = new AbstractAction() {
		public void actionPerformed(ActionEvent e) {
//			tabToNext();

//			if (e.getSource() instanceof Component) { Component next =
//			focusPolicy.getComponentAfter( IntersectionPanel.this,
//			FocusManager .getCurrentKeyboardFocusManager() .getFocusOwner());
//			next.requestFocus(); }

		}
	};

	protected boolean createNewObject = false;

	public CrossProductEditorComponent() {
		this(false);
	}

	public void setCreateNewObject(boolean createNewObject) {
		this.createNewObject = createNewObject;
		anonymousCheckbox.setEnabled(createNewObject);
	}

	public CrossProductEditorComponent(boolean showNameFields) {
		this.showNameFields = showNameFields;
		setCreateNewObject(false);
		genusField.setFocusTraversalKeysEnabled(false);
		genusField.addCommitListener(commitListener);
		addFocusListener(new FocusAdapter() {
			public void focusGained(FocusEvent e) {
				Component firstComponent = focusPolicy
				.getFirstComponent(CrossProductEditorComponent.this);
				if (firstComponent != null)
					firstComponent.requestFocus();
			}
		});
		// setFocusTraversalPolicyProvider(true);
		setFocusTraversalPolicy(focusPolicy);
		getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(
				KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "tabForward");
		getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(
				KeyStroke.getKeyStroke(KeyEvent.VK_TAB, 0), "tabForward");
		getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(
				KeyStroke.getKeyStroke(KeyEvent.VK_TAB,
						KeyEvent.SHIFT_DOWN_MASK), "tabBackward");
		getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(
				KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, Toolkit
						.getDefaultToolkit().getMenuShortcutKeyMask()),
		"commit");

		getActionMap().put("tabForward", new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				tabToNext();
			}
		});
		getActionMap().put("commit", new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				commit();
			}
		});
	}

	public void addActionListener(ActionListener actionListener) {
		actionListeners.add(actionListener);
	}

	protected void addDiscriminating(OBOClass discriminatingTerm,
			OBOProperty prop) {
		/*
		 * OBORestriction or = new OBORestrictionImpl(oboClass, prop,
		 * discriminatingTerm); or.setCompletes(true); relationshipList.add(or);
		 * buildInterface();
		 */
		RelationshipLinePanel panel = new RelationshipLinePanel();
		panel.setProperty(prop);
		panel.setParentTerm(discriminatingTerm);
		linkListPanel.add(panel);
		validate();
		repaint();
		panel.tabIn();
	}

	protected boolean allowDrop(Selection selection) {
		return selection.getTermSubSelection() != null
		&& !selection.getTermSubSelection().equals(oboClass);
	}

	public void commit() {
		ActionEvent e = new ActionEvent(this, (int) Math.random()
				* Integer.MAX_VALUE, "commit");
		for (ActionListener listener : actionListeners) {
			listener.actionPerformed(e);
		}
	}

	public java.util.List getChanges() {
		java.util.List<HistoryItem> historyList = new LinkedList<HistoryItem>();

		if (currentObject instanceof LinkedObject) {
			//get existing differentia
			Collection<Link> differentia = ReasonerUtil
			.getDifferentia((OBOClass) currentObject);

			// Find any intersection links that have been deleted
			Iterator it = ((LinkedObject) currentObject).getParents()
			.iterator();
			while (it.hasNext()) {
				Link link = (Link) it.next();

//				logger.debug("link: " + link.getType().getName() + "  " + link.getParent());

				if (!TermUtil.isIntersection(link))
					continue;

				boolean found = false;
				Iterator it2 = getRelationshipList().iterator();
				while (it2.hasNext()) {
					Link completeDefLink = (Link) it2.next();
					if (completeDefLink.equals(link)) {
						found = true;
						break;
					} 
//					else {
//					logger.info("   " + completeDefLink + " != "
//					+ link);
//					}
				}
				if (!found) {
					historyList.add(new DeleteLinkHistoryItem(link));
				}
			}
			it = getRelationshipList().iterator();
			while (it.hasNext()) {
				OBORestriction completeDefLink = (OBORestriction) it.next();
//				logger.debug("completeDefLink: " + completeDefLink);
				Link matchLink = HistoryUtil.findParentRel(completeDefLink,
						(LinkedObject) currentObject);
				if (matchLink == null) {
					// set completes to false because new links are
					// always created with completes=false by default
					// we'll reset the completes flag in a moment
					completeDefLink.setCompletes(false);
					historyList.add(new CreateLinkHistoryItem(completeDefLink));
				}

				if (matchLink == null || !TermUtil.isIntersection(matchLink)) {
					completeDefLink.setCompletes(false);
					historyList.add(new CompletesHistoryItem(completeDefLink));
				}
				// reset the completes flag to true
				completeDefLink.setCompletes(true);
			}
		}
		return historyList;
	}

	public String getID() {
		return "CROSSPRODUCT_EDITOR";
	}

	protected RelationshipLinePanel getLastRelationshipLine() {
		Component lastComp = null;
		for (Component c : linkListPanel.getComponents()) {
			if (c instanceof RelationshipLinePanel)
				lastComp = c;
		}
		return (RelationshipLinePanel) lastComp;
	}

	public Collection<Link> getRelationshipList() {
//		logger.debug("CrossProductEditorComponent.getRelationshipList");
		LinkedList<Link> out = new LinkedList<Link>();
		Object intersectionGenus = genusField.getValue();
//		logger.debug("intersectionGenus: " + intersectionGenus);
		if ( intersectionGenus!= null
				&& intersectionGenus instanceof LinkedObject) {
			OBORestriction isaLink = new OBORestrictionImpl(oboClass,
					OBOProperty.IS_A, (LinkedObject) intersectionGenus);
			isaLink.setCompletes(true);
			out.add(isaLink);
//			logger.debug("CrossProductEditorComponent.getRelationshipList -- isaLink: " + isaLink);
		}

		for (int i = 0; i < linkListPanel.getComponentCount(); i++) {
			if (linkListPanel.getComponent(i) instanceof RelationshipLinePanel) {
				RelationshipLinePanel panel = (RelationshipLinePanel) linkListPanel
				.getComponent(i);
				if (panel.getParentTerm() == null
						|| panel.getProperty() == null)
					continue;
				OBORestriction discLink = new OBORestrictionImpl(oboClass,
						panel.getProperty(), panel.getParentTerm());
				discLink.setCompletes(true);
				out.add(discLink);
//				logger.debug("CrossProductEditorComponent.getRelationshipList -- discLink: " + discLink);
			}
		}
		return out;
	}


	@Override
	protected void initializeGUI() {

		JPanel linkWrapperPanel = new JPanel();
		linkWrapperPanel.setOpaque(false);
		linkWrapperPanel.setLayout(new BorderLayout());

		setLayout(new GridLayout(1, 1));
		editorPanel.setLayout(new BorderLayout());

		setBorder(new EmptyBorder(5, 5, 5, 5));

		genusPanel.setLayout(new BorderLayout());
		linkListPanel.setLayout(new BoxLayout(linkListPanel, BoxLayout.Y_AXIS));

		genusPanel.setOpaque(false);
		linkListPanel.setOpaque(false);

		TitledBorder titledBorder = new TitledBorder("Discriminating relations");
		Border cBorder = new CompoundBorder(new EmptyBorder(20, 0, 5, 0),
				titledBorder);
		linkWrapperPanel.setBorder(cBorder);

		Font font = getFont();

		genusLabel.setFont(font);

		dropButton.setFont(font);
		dropButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addDiscriminating(null, null);
			}
		});

		genusField.setMinimumSize(new Dimension(0, font.getSize() + 5));
		genusField.setFont(font);

		JPanel labelBox = new JPanel();
		labelBox.setOpaque(false);
		labelBox.add(genusLabel);
		labelBox.add(Box.createHorizontalStrut(10));
		/*
		 * genusPanel.add(labelBox, "West"); //
		 * genusPanel.add(Box.createHorizontalStrut(10));
		 * genusPanel.add(genusField, "Center");
		 * genusPanel.add(genusSelectButton, "East");
		 * genusPanel.setOpaque(false);
		 */

		linkWrapperPanel.add(linkListPanel, "North");
		linkWrapperPanel.add(dropButton, "South");

		JPanel northPanel = new JPanel();
		northPanel.setOpaque(false);
		northPanel.setLayout(new SpringLayout());

		northPanel.add(genusLabel);
		northPanel.add(genusField);
		SpringUtilities.makeCompactGrid(northPanel, -1, 2, // rows, cols
				6, 6, // initX, initY
				6, 0);

		editorPanel.add(northPanel, "North");
		editorPanel.add(linkWrapperPanel, "Center");
		editorPanel.setOpaque(false);
		/*
		 * editorPanel.add(relPanel, "North"); editorPanel.add(dropButton,
		 * "Center");
		 */
	}

	@Override
	public void installListeners() {
		dropButton.setDropTarget(new DropTarget(dropButton,
				dropDiscriminatingListener));
	}

	/*
	 * protected class TabFocusListener extends KeyAdapter { protected Component
	 * comp;
	 * 
	 * public TabFocusListener(Component comp) { this.comp = comp; }
	 * 
	 * @Override public void keyPressed(KeyEvent e) { if (e.getKeyCode() ==
	 * KeyEvent.VK_TAB || e.getKeyCode() == KeyEvent.VK_ENTER) {
	 * logger.info("Got keypress from genus field"); tabToNext(comp); } } }
	 */
	protected boolean isTabFocusable(Component c) {
		return focusPolicy.accept(c);
	}

	@Override
	protected void loadGUI() {
		removeAll();
		if (currentObject != null && currentObject instanceof OBOClass) {
			add(editorScroller);
			setClass((OBOClass) currentObject);

		} else {
			add(notLoadedLabel);
		}
		validate();
		repaint();
	}

	public void populateFields(IdentifiedObject currentObject) {
		if (!(currentObject instanceof OBOClass))
			return;
		Iterator it = getRelationshipList().iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (!((OBOClass) currentObject).getParents().contains(link)) {
				((OBOClass) currentObject).atomicAddParent(link);
			}
		}
	}

	public void removeActionListener(ActionListener actionListener) {
		actionListeners.remove(actionListener);
	}

	@Override
	public Component resolveName(String id, Properties props, String xml) {
		return null;
	}

	public void setClass(OBOClass oboClass) {
		this.oboClass = oboClass;
		genusField.setValue(null);
		linkListPanel.removeAll();

//		logger.info("IntersectionPanel.setClass: parents of " + oboClass + " = "
//		+ oboClass.getParents());

		Iterator it = oboClass.getParents().iterator();
		while (it.hasNext()) {
			OBORestriction link = (OBORestriction) it.next();
			if (!link.completes())
				continue;
			LinkedObject parent = link.getParent();
			if (TermUtil.isDangling(parent)) {
				parent = new DanglingClassImpl(parent.getID());
			}
			if (link.getType().equals(OBOProperty.IS_A)) {
				genusField.setValue(parent);
			} else {
				addDiscriminating((OBOClass) parent, link.getType());
			}
		}

		repaint();
	}

	protected void setGenus(OBOClass genusTerm) {
		genusField.setValue(genusTerm);
	}

	protected void tabToNext() {
		Component lastComponent = focusPolicy.getLastComponent(this);
		Component focused = FocusManager.getCurrentKeyboardFocusManager()
		.getFocusOwner();
		if (SwingUtilities.isDescendingFrom(focused, lastComponent)) {
			RelationshipLinePanel last = getLastRelationshipLine();
			if (last != null
					&& (last.getParentTerm() == null || last.getProperty() == null)) {
				commit();
			} else
				addDiscriminating(null, null);
		} else
			focused.transferFocus();
	}

	protected void tabToPrevious(Component comp) {
		comp.transferFocusBackward();
	}

//	if cotton ball does not select genus in OTE use genusSelectListener and install and uninstallListeners	
//	@Override
//	public void uninstallListeners() {
//	genusSelectButton.removeActionListener(genusSelectListener);
//	}

	@Override
	public boolean useSubLayout() {
		return false;
	}

}

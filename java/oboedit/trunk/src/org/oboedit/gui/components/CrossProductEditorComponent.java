package org.oboedit.gui.components;

import java.awt.*;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.event.*;
import java.util.*;
import java.util.List;

import javax.swing.*;
import javax.swing.border.*;

import org.bbop.swing.*;
import org.bbop.swing.widget.AutocompleteBox;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.history.*;
import org.obo.util.HistoryUtil;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.AbstractTextEditComponent;
import org.oboedit.gui.DropUtil;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.Selection;
import org.oboedit.gui.TermAutocompleteModel;


import org.apache.log4j.*;

/**
 * Text Editor -> Cross Product Tab interface
 * */
public class CrossProductEditorComponent extends AbstractTextEditComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CrossProductEditorComponent.class);

	protected JPanel editorPanel = new JPanel();
	protected JPanel genusPanel = new JPanel();
	protected JPanel linkListPanel = new JPanel();
	protected JPanel relPanel = new JPanel();

	protected JScrollPane editorScroller = new JScrollPane(editorPanel,
			JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

	protected JLabel genusLabel = new JLabel("Intersection Genus");
	protected JLabel notLoadedLabel = new JLabel("No term selected");
	protected JTextField nameField = new JTextField();
	protected JLabel idField = new JLabel();
	protected JCheckBox anonymousCheckbox = new JCheckBox("anonymous");

	protected OBOClass oboClass;
	protected boolean showNameFields;
	protected boolean createNewObject = false;

	protected JButton selectGenusButton;
	protected ActionListener selectGenusActionListener;	

	protected AutocompleteBox<IdentifiedObject> genusField = new AutocompleteBox<IdentifiedObject>(new TermAutocompleteModel());

	protected CrossProductPanelFocusPolicy focusPolicy = new CrossProductPanelFocusPolicy();

	protected class CrossProductPanelFocusPolicy extends LayoutFocusTraversalPolicy {
		public boolean accept(Component aComponent) {
			if (aComponent instanceof JComboBox
					|| SwingUtilities.getAncestorOfClass(
							AutocompleteBox.class, aComponent) != null)
				return super.accept(aComponent);
			else
				return false;
		}

		public Component getDefaultComponent(Container aContainer) {
			return genusField;
		}
	}

	public CrossProductEditorComponent() {
		this(false);
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
						.getDefaultToolkit().getMenuShortcutKeyMask()),"commit");

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


	/** 
	 * Relationship Line Panel
	 * */
	protected class RelationshipLinePanel extends JPanel {
		private static final long serialVersionUID = 1L;

		//trash can icon - delete relation
		protected Icon deleteRelationIcon = Preferences.loadLibraryIcon("trashcan.gif");
		protected JButton deleteRelationButton = new JButton(deleteRelationIcon);

		//cotton ball icon - go to differentia term
		protected Icon selectDifferentiaIcon = Preferences.loadLibraryIcon("selector.gif");
		JButton selectDifferentiaButton = new JButton(selectDifferentiaIcon);

		protected AutocompleteBox<IdentifiedObject> parentBox = new AutocompleteBox<IdentifiedObject>(
				new TermAutocompleteModel());
		protected JComboBox propertyBox = new JComboBox();
		protected ActionListener selectDifferentiaActionListener;

		public RelationshipLinePanel() {
			getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(
					KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, false), "escapeLine");
			getActionMap().put("escapeLine", new AbstractAction() {
				public void actionPerformed(ActionEvent e) {
					if (getProperty() == null || getParentTerm() == null) {
						Container parent = RelationshipLinePanel.this.getParent();
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

			deleteRelationButton.setPreferredSize(new Dimension(20, 18));
			deleteRelationButton.setToolTipText("Delete relation");

			selectDifferentiaButton.setPreferredSize(new Dimension(20, 20));
			selectDifferentiaButton.setToolTipText("Go to differentia term");

			// delete relation button
			deleteRelationButton.addActionListener(new ActionListener() {
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
			add(selectDifferentiaButton);
			add(deleteRelationButton);
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

		/**
		 * setParentTerm: setting value for parent term and action listener for -> Go to differentia term
		 * */
		public void setParentTerm(final OBOClass parentTerm) {
			parentBox.setValue(parentTerm);
			if (selectDifferentiaActionListener != null)
				selectDifferentiaButton.removeActionListener(selectDifferentiaActionListener);
			selectDifferentiaActionListener = new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					SelectionManager.selectTerm(CrossProductEditorComponent.this, parentTerm);
				}
			};
			selectDifferentiaButton.addActionListener(selectDifferentiaActionListener);
		}

		public void setProperty(OBOProperty property) {
			propertyBox.setSelectedItem(property);
		}

		public void tabIn() {
			propertyBox.requestFocus();
		}
	}//RelationshipLine Panel


	protected Collection<ActionListener> actionListeners = new LinkedList<ActionListener>();
	protected JButton dropButton = new JButton("Click (or drop a term) here to add new differentia");

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

			final LinkedObject term = DropUtil.getSelection(dtde).getTermSubSelection();
			OBOSession session = SessionManager.getManager().getSession();

			JMenuItem genusItem = new JMenuItem("Set genus term");
			genusItem.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					setGenusTerm((OBOClass) term);
				}
			});
			menu.add(genusItem);

			menu.addSeparator();

			for(final OBOProperty prop : TermUtil.getRelationshipTypes(session)){
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
		} //drop

		public void dropActionChanged(DropTargetDragEvent dtde) {
		}

	};

	/**
	 * addDiscriminating: add new differentia
	 * called by dropButton action listener
	 * */
	protected void addDiscriminating(OBOClass discriminatingTerm, OBOProperty prop) {
		RelationshipLinePanel relationshipPanel = new RelationshipLinePanel();
		relationshipPanel.setProperty(prop);
		relationshipPanel.setParentTerm(discriminatingTerm);
		linkListPanel.add(relationshipPanel);
		validate();
		repaint();
		relationshipPanel.tabIn();
	}


	protected Action commitListener = new AbstractAction() {
		public void actionPerformed(ActionEvent e) {
			logger.debug(">> CrossProductEditorComponent commitListener");
			//			tabToNext();
			//			if (e.getSource() instanceof Component) { Component next =
			//			focusPolicy.getComponentAfter( IntersectionPanel.this,
			//			FocusManager .getCurrentKeyboardFocusManager() .getFocusOwner());
			//			next.requestFocus(); }
		}
	};

	public void setCreateNewObject(boolean createNewObject) {
		this.createNewObject = createNewObject;
		anonymousCheckbox.setEnabled(createNewObject);
	}

	public void addActionListener(ActionListener actionListener) {
		actionListeners.add(actionListener);
	}

	protected boolean allowDrop(Selection selection) {
		return selection.getTermSubSelection() != null
		&& !selection.getTermSubSelection().equals(oboClass);
	}

	public void commit() {
		ActionEvent e = new ActionEvent(this, (int) Math.random() * Integer.MAX_VALUE, "commit");
		for (ActionListener listener : actionListeners) {
			listener.actionPerformed(e);
		}
	}

	public List getChanges() {
		List<HistoryItem> historyList = new LinkedList<HistoryItem>();

		if (currentObject instanceof LinkedObject && !(currentObject.getClass().getSimpleName().toString().equalsIgnoreCase("OBOPropertyImpl")) ) {
			//get existing differentia
			Collection<Link> differentia = ReasonerUtil.getDifferentia((OBOClass) currentObject);
			
			//get the list of discriminating relations associated with this object to compute additions and deletions to the relations list
  			Collection<Link> relations = getRelationshipList();
			
			// Find intersection links that have been deleted
			for(Link link : ((LinkedObject) currentObject).getParents()){
				if (!TermUtil.isIntersection(link))
					continue;
			
				// only if relations exist do the check to see if they have been deleted.
				// this has been added due to false positive results being generated for links being deleted.
				// (found is returning true after links have been committed)
				if(relations.size() >= 1){
					boolean found = false;

					for(Object o : relations){
						OBORestriction completeDefLink = (OBORestriction) o;
//						logger.debug("CPEC.getChanges -- relations -- completeDefLink: " + completeDefLink);
						
						// check for deleted links
						if (completeDefLink.equals(link)) {
							found = true;
							break;
						}
						
					}
					if (!found){
						logger.debug("CPEC - deleting link: " + link);
						historyList.add(new DeleteLinkHistoryItem(link));			
					}
				}
			}

			//Find intersection links that have been added
			for(Object o : relations){
				OBORestriction completeDefLink = (OBORestriction) o;
				//logger.debug("completeDefLink: " + completeDefLink);
				Link matchLink = HistoryUtil.findParentRel(completeDefLink, (LinkedObject) currentObject);
				if (matchLink == null) {
					// set completes to false because new links are
					// always created with completes=false by default
					// we'll reset the completes flag in a moment
					completeDefLink.setCompletes(true);
					logger.debug("CPEC - adding link: " + completeDefLink);
					historyList.add(new CreateIntersectionLinkHistoryItem(completeDefLink));
				}
			}

		}
		return historyList;
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
		LinkedList<Link> out = new LinkedList<Link>();
		Object intersectionGenus = genusField.getValue();
		//compute and add genus link to relations list
		if ( intersectionGenus!= null && intersectionGenus instanceof LinkedObject) {
			OBORestriction isaLink = new OBORestrictionImpl(oboClass,
					OBOProperty.IS_A, (LinkedObject) intersectionGenus);
			isaLink.setCompletes(true);
			out.add(isaLink);
		}
		//get discriminating relations 
		for (int i = 0; i < linkListPanel.getComponentCount(); i++) {
			if (linkListPanel.getComponent(i) instanceof RelationshipLinePanel) {
				RelationshipLinePanel panel = (RelationshipLinePanel) linkListPanel.getComponent(i);
//				logger.debug("CPEC: panel.getParentTerm(): " + panel.getParentTerm());
				if (panel.getParentTerm() == null || panel.getProperty() == null)
					continue;
				OBORestriction discLink = new OBORestrictionImpl(oboClass, panel.getProperty(), panel.getParentTerm());
				discLink.setCompletes(true);
				out.add(discLink);
			}
		}
		return out;
	}


	@Override
	protected void initializeGUI() {
		JPanel linkWrapperPanel = new JPanel();
		linkWrapperPanel.setOpaque(false);
		linkWrapperPanel.setLayout(new BorderLayout());

		//northPanel houses the genus label, text box and selection button
		JPanel northPanel = new JPanel();
		JPanel labelBox = new JPanel();
		labelBox.setOpaque(false);

		editorPanel.setLayout(new BorderLayout());
		genusPanel.setLayout(new BorderLayout());
		linkListPanel.setLayout(new BoxLayout(linkListPanel, BoxLayout.Y_AXIS));
		genusPanel.setOpaque(false);
		linkListPanel.setOpaque(false);
		setLayout(new GridLayout(1, 1));
		setBorder(new EmptyBorder(5, 5, 5, 5));

		TitledBorder titledBorder = new TitledBorder("Discriminating Relations");
		Border cBorder = new CompoundBorder(new EmptyBorder(20, 0, 5, 0), titledBorder);
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

		//cotton ball - button to go to genus term
		Icon selectGenusIcon = Preferences.loadLibraryIcon("selector.gif");
		selectGenusButton = new JButton(selectGenusIcon);
		selectGenusButton.setPreferredSize(new Dimension(20, 20));
		selectGenusButton.setToolTipText("Go to genus term");
		selectGenusButton.addActionListener(selectGenusActionListener);


		labelBox.add(genusLabel);
		labelBox.add(Box.createHorizontalStrut(10));

		linkWrapperPanel.add(linkListPanel, "North");
		linkWrapperPanel.add(dropButton, "South");


		northPanel.setOpaque(false);
		northPanel.setLayout(new SpringLayout());

		northPanel.add(genusLabel);
		northPanel.add(genusField);
		northPanel.add(selectGenusButton);
		SpringUtilities.makeCompactGrid(northPanel, -1, 3, // rows, cols
				6, 6, // initX, initY
				6, 0);

		editorPanel.add(northPanel, "North");
		editorPanel.add(linkWrapperPanel, "Center");
		editorPanel.setOpaque(false);
	} //initializeGUI

	@Override
	public void installListeners() {
		dropButton.setDropTarget(new DropTarget(dropButton,
				dropDiscriminatingListener));
	}

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
		for(Link link : getRelationshipList()){
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

	/**
	 * setClass: current object
	 * */
	public void setClass(OBOClass oboClass) {
		this.oboClass = oboClass;
		genusField.setValue(null);
		linkListPanel.removeAll();
		for(Object o : oboClass.getParents()){
			OBORestriction link = (OBORestriction) o;
			if (!link.getCompletes())
				continue;
			LinkedObject parent = link.getParent();
			if (TermUtil.isDangling(parent)) {
				parent = new DanglingClassImpl(parent.getID());
			}

			final OBOClass genusTerm = (OBOClass) parent;
			//			logger.debug("genusTerm: " +  genusTerm);
			if (link.getType().equals(OBOProperty.IS_A)) {
				genusField.setValue(parent);

				selectGenusButton.addActionListener(new ActionListener(){
					public void actionPerformed(ActionEvent e) {
						logger.debug("genusTerm: " + genusTerm);
						SelectionManager.selectTerm(CrossProductEditorComponent.this, genusTerm);
					}

				});

			} else {
				addDiscriminating((OBOClass) parent, link.getType());
			}
		}
		repaint();
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

	@Override
	public boolean useSubLayout() {
		return false;
	}

	/**
	 * setGenusTerm
	 * called while dropping term on drop term button */
	protected void setGenusTerm(final OBOClass genusTerm) {
		genusField.setValue(genusTerm);
	}

	public String getID() {
		return "CROSSPRODUCT_EDITOR";
	}

}
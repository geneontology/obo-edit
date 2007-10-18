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
import javax.swing.tree.*;

import org.bbop.swing.*;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.history.*;
import org.obo.util.HistoryUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.AbstractTextEditComponent;
import org.oboedit.gui.DropUtil;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.Selection;

public class CompleteDefPanel extends AbstractTextEditComponent {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected static Icon deleteIcon = Preferences
			.loadLibraryIcon("trashcan.gif");

	protected static Icon selectIcon = Preferences
			.loadLibraryIcon("selector.gif");

	protected OBOClass oboClass;

	protected OBOClass genusTerm;

	protected java.util.List<Link> relationshipList = new LinkedList<Link>();

	protected JPanel editorPanel = new JPanel();

	protected JScrollPane editorScroller = new JScrollPane(editorPanel,
			JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

	protected JPanel relPanel = new JPanel();

	protected JPanel genusPanel = new JPanel();

	protected JPanel linkListPanel = new JPanel();

	protected JLabel notLoadedLabel = new JLabel("No term selected");

	protected JButton dropButton = new JButton("<drop new terms here>");

	protected JLabel genusLabel = new JLabel("Genus");

	protected JButton genusButton = new JButton("<no genus specified>");

	protected JButton genusSelectButton = new JButton(selectIcon);

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

	protected boolean allowDrop(Selection selection) {
		return selection.getTermSubSelection() != null
				&& !selection.getTermSubSelection().equals(oboClass);
	}

	protected DropTargetListener dropGenusListener = new DropTargetListener() {
		LineBorder border = new LineBorder(Color.black, 2);

		Border oldBorder;

		public void dragEnter(DropTargetDragEvent dtde) {
			if (!allowDrop(DropUtil.getSelection(dtde))) {
				oldBorder = null;
				dtde.rejectDrag();
			}
			oldBorder = dropButton.getBorder();
			genusButton.setBorder(border);
		}

		public void dragExit(DropTargetEvent dte) {
			if (oldBorder != null)
				genusButton.setBorder(oldBorder);
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
				setGenus((OBOClass) term);
				genusButton.setBorder(oldBorder);
			}
		}

		public void dropActionChanged(DropTargetDragEvent dtde) {
		}

	};

	/*
	 * protected DropListener dropGenusListener = new DropListener() {
	 * LineBorder border = new LineBorder(Color.black, 2);
	 * 
	 * Border oldBorder;
	 * 
	 * public boolean allowDrop(DragEvent e) { if (e.getData() instanceof
	 * TreePath[] && ((TreePath[]) e.getData()).length > 0) { TreePath[] paths =
	 * (TreePath[]) e.getData(); if (paths.length != 1) return false; if
	 * (!(paths[0].getLastPathComponent() instanceof OBORestriction)) return
	 * false; OBORestriction or = (OBORestriction) paths[0]
	 * .getLastPathComponent(); if (or.getChild().equals(oboClass)) return
	 * false; return true; } else return false; }
	 * 
	 * public void dragEnter(DragEvent e) { oldBorder = genusButton.getBorder();
	 * 
	 * if (allowDrop(e)) genusButton.setBorder(border); }
	 * 
	 * public void dragExit(DragEvent e) { genusButton.setBorder(oldBorder); }
	 * 
	 * public void drop(DragEvent e) { TreePath[] paths = (TreePath[])
	 * e.getData(); if (paths.length == 1) { Link link = (Link)
	 * paths[0].getLastPathComponent(); setGenus((OBOClass) link.getChild()); }
	 * genusButton.setBorder(oldBorder); }
	 * 
	 * public void draggedOver(DragEvent e) { } };
	 */
	public CompleteDefPanel() {
	}

	@Override
	public Component resolveName(String id, Properties props, String xml) {
		return null;
	}

	@Override
	public boolean useSubLayout() {
		return false;
	}

	@Override
	protected void initializeGUI() {
		setLayout(new GridLayout(1, 1));
		editorPanel.setLayout(new BorderLayout());

		setBorder(new EmptyBorder(5, 5, 5, 5));

		relPanel.setLayout(new BorderLayout());
		genusPanel.setLayout(new BorderLayout());
		linkListPanel.setLayout(new BoxLayout(linkListPanel, BoxLayout.Y_AXIS));

		relPanel.setOpaque(false);
		genusPanel.setOpaque(false);
		linkListPanel.setOpaque(false);

		TitledBorder titledBorder = new TitledBorder("Discriminating relations");
		Border cBorder = new CompoundBorder(new EmptyBorder(20, 0, 5, 0),
				titledBorder);
		linkListPanel.setBorder(cBorder);

		Font font = getFont().deriveFont(Font.BOLD);

		genusButton.setMinimumSize(new Dimension(0, font.getSize() + 5));
		genusButton.setFont(font);
		// genusButton.setBackground(null);
		// genusButton.setBorder(null);

		genusSelectButton.setPreferredSize(new Dimension(20, 20));
		genusSelectButton.setToolTipText("Select genus term");

		JPanel labelBox = new JPanel();
		labelBox.setOpaque(false);
		labelBox.add(genusLabel);
		labelBox.add(Box.createHorizontalStrut(10));

		genusPanel.add(labelBox, "West");
		// genusPanel.add(Box.createHorizontalStrut(10));
		genusPanel.add(genusButton, "Center");
		genusPanel.add(genusSelectButton, "East");
		genusPanel.setOpaque(false);

		relPanel.add(genusPanel, "North");
		relPanel.add(linkListPanel, "Center");
		relPanel.setOpaque(false);

		editorPanel.add(relPanel, "North");
		editorPanel.add(dropButton, "Center");
	}

	@Override
	protected void loadGUI() {
		removeAll();
		if (currentObject != null && currentObject instanceof OBOClass) {
			add(editorScroller);
			setClass((OBOClass) currentObject);
			boolean enable = !TermUtil.isObsolete(currentObject);
			genusButton.setEnabled(enable);
			dropButton.setEnabled(enable);

		} else {
			add(notLoadedLabel);
		}
		validate();
		repaint();
	}

	public Collection<Link> getRelationshipList() {
		LinkedList<Link> out = new LinkedList<Link>();
		if (genusTerm != null) {
			OBORestriction isaLink = new OBORestrictionImpl(oboClass,
					OBOProperty.IS_A, genusTerm);
			isaLink.setCompletes(true);
			out.add(isaLink);
		}

		out.addAll(relationshipList);
		return out;
	}

	protected void setGenus(OBOClass genusTerm) {
		this.genusTerm = genusTerm;
		buildInterface();
	}

	protected void addDiscriminating(OBOClass discriminatingTerm,
			OBOProperty prop) {
		OBORestriction or = new OBORestrictionImpl(oboClass, prop,
				discriminatingTerm);
		or.setCompletes(true);
		relationshipList.add(or);
		buildInterface();
	}

	protected ActionListener genusButtonListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			setGenus(null);
		}
	};

	protected ActionListener genusSelectListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			if (genusTerm != null)
				SelectionManager.selectTerm(CompleteDefPanel.this, genusTerm);
		}
	};

	@Override
	public void installListeners() {
		genusButton.addActionListener(genusButtonListener);
		genusSelectButton.addActionListener(genusSelectListener);
		genusButton
				.setDropTarget(new DropTarget(genusButton, dropGenusListener));
		dropButton.setDropTarget(new DropTarget(dropButton,
				dropDiscriminatingListener));
	}

	@Override
	public void uninstallListeners() {
		genusButton.removeActionListener(genusButtonListener);
		genusSelectButton.removeActionListener(genusSelectListener);
	}

	public java.util.List getChanges() {
		java.util.List<HistoryItem> historyList = new LinkedList<HistoryItem>();
		if (currentObject instanceof LinkedObject) {
			// Find any intersection links that have been deleted
			Iterator it = ((LinkedObject) currentObject).getParents()
					.iterator();
			while (it.hasNext()) {
				Link link = (Link) it.next();

				if (!TermUtil.isIntersection(link))
					continue;

				boolean found = false;
				Iterator it2 = getRelationshipList().iterator();
				while (it2.hasNext()) {
					Link completeDefLink = (Link) it2.next();
					if (completeDefLink.equals(link)) {
						found = true;
						break;
					} else {
						System.err.println("   " + completeDefLink + " != "
								+ link);
					}
				}

				if (!found) {
					historyList.add(new DeleteLinkHistoryItem(link));
				}
			}
			System.err.println("relationshipList = " + getRelationshipList());
			// Find any intersection links that have been added
			it = getRelationshipList().iterator();
			while (it.hasNext()) {
				OBORestriction completeDefLink = (OBORestriction) it.next();

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
		System.err.println("historyList = " + historyList);
		return historyList;
	}

	public void setClass(OBOClass oboClass) {
		this.oboClass = oboClass;

		relationshipList.clear();
		genusTerm = null;

		System.err.println("parents of " + oboClass + " = "
				+ oboClass.getParents());

		Iterator it = oboClass.getParents().iterator();
		while (it.hasNext()) {
			OBORestriction link = (OBORestriction) it.next();
			if (!link.completes())
				continue;
			if (link.getType().equals(OBOProperty.IS_A)) {
				genusTerm = (OBOClass) link.getParent();
			} else {
				relationshipList.add(link);
			}
		}

		buildInterface();
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

	protected void buildInterface() {
		boolean enabled = !TermUtil.isObsolete(currentObject);

		if (genusTerm != null)
			genusButton.setText(genusTerm.getName());
		else
			genusButton.setText("<drop a genus term>");

		linkListPanel.removeAll();

		if (relationshipList.size() == 0) {
			JLabel propertyLabel = new JLabel(
					"<drop a discriminating link below>");
			propertyLabel.setEnabled(enabled);
			linkListPanel.add(propertyLabel);
		}

		Iterator it = relationshipList.iterator();
		while (it.hasNext()) {
			final OBORestriction tr = (OBORestriction) it.next();

			JPanel relationshipLinePanel = new JPanel();
			relationshipLinePanel.setOpaque(false);

			relationshipLinePanel.setLayout(new BoxLayout(
					relationshipLinePanel, BoxLayout.X_AXIS));
			relationshipLinePanel.add(Box.createHorizontalStrut(20));

			JComboBox typeBox = new JComboBox();

			Iterator it2 = TermUtil.getRelationshipTypes(
					SessionManager.getManager().getSession()).iterator();
			while (it2.hasNext()) {
				OBOProperty property = (OBOProperty) it2.next();
				typeBox.addItem(property.getID());
			}
			typeBox.setSelectedItem(tr.getType().getID());

			JButton parentButton = new JButton(tr.getParent().getName());

			JButton deleteButton = new JButton(deleteIcon);
			deleteButton.setPreferredSize(new Dimension(20, 20));

			deleteButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					relationshipList.remove(tr);
					buildInterface();
				}
			});

			JButton selectButton = new JButton(selectIcon);
			selectButton.setPreferredSize(new Dimension(20, 20));
			selectButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					SelectionManager.selectTerm(CompleteDefPanel.this, tr
							.getParent());
				}
			});

			typeBox.setEnabled(enabled);
			parentButton.setEnabled(enabled);
			deleteButton.setEnabled(enabled);

			relationshipLinePanel.add(Box.createHorizontalStrut(30));
			relationshipLinePanel.add(typeBox);
			relationshipLinePanel.add(Box.createHorizontalStrut(10));
			relationshipLinePanel.add(parentButton);
			relationshipLinePanel.add(Box.createHorizontalStrut(10));
			relationshipLinePanel.add(selectButton);
			relationshipLinePanel.add(Box.createHorizontalStrut(10));
			relationshipLinePanel.add(deleteButton);

			linkListPanel.add(relationshipLinePanel);
		}
		validate();
		repaint();
	}

	public String getID() {
		return "COMPLETE_DEF_EDITOR";
	}

}
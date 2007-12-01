package org.oboedit.gui.components;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.util.ObjectUtil;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.history.*;
import org.obo.util.HistoryUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.gui.event.*;
import org.oboedit.util.GUIUtil;
import org.oboedit.util.PathUtil;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Rectangle;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListCellRenderer;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.Scrollable;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import javax.swing.tree.TreePath;

public class ParentEditor extends AbstractGUIComponent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected SelectionListener selectionListener = new SelectionListener() {
		public void selectionChanged(SelectionEvent e) {
			loadTerm(SelectionManager.getGlobalSelection()
					.getTermSubSelection());
		}
	};

	ReloadListener reloadListener = new ReloadListener() {
		public void reload(ReloadEvent e) {
			loadTerm(SelectionManager.getGlobalSelection()
					.getTermSubSelection());
		}
	};

	protected class RowScrollPanel extends JPanel implements Scrollable {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public Dimension getPreferredScrollableViewportSize() {
			return getPreferredSize();
		}

		public int getScrollableUnitIncrement(Rectangle visibleRect,
				int orientation, int direction) {
			return getFont().getSize();
		}

		public int getScrollableBlockIncrement(Rectangle visibleRect,
				int orientation, int direction) {
			return getFont().getSize() * 4;
		}

		public boolean getScrollableTracksViewportWidth() {
			return true;
		}

		public boolean getScrollableTracksViewportHeight() {
			return false;
		}
	}

	protected LinkedObject currentObject;

	protected JLabel label = new JLabel("<no selection>");

	protected JPanel outerPanel = new RowScrollPanel();

	protected JPanel buttonPanel = new JPanel();

	protected JButton dropButton = new JButton("Drop terms here to add new "
			+ "parents");

	protected EmptyBorder emptyBorder = new EmptyBorder(2, 2, 2, 2);

	protected Icon nec_icon;

	protected Icon completes_icon;

	protected Icon inv_icon;

	protected static final String[] cardinalityChoices = { "Single value",
			"Range" };

	protected JTextField cardinalityField = new JTextField(3);

	protected JTextField minCardinalityField = new JTextField(3);

	protected JTextField maxCardinalityField = new JTextField(3);

	protected JLabel cardinalityLabel = new JLabel("Cardinality");

	protected JLabel cardinalityRangeLabel = new JLabel("Cardinality range");

	protected JPanel cardinalityPanel = new JPanel();

	protected JButton cardinalityCommitButton = new JButton("Commit");

	protected JCheckBox showImpliedCheckbox = new JCheckBox("Show implied");

	protected SessionManager sessionManager = SessionManager.getManager();

	protected DropTargetListener dropListener = new DropTargetListener() {
		public boolean allowDrop(DropTargetDragEvent e) {
			Selection s = DropUtil.getSelection(e);
			return s != null && s.getTerms().size() > 0;
		}

		public void dragEnter(DropTargetDragEvent e) {
			if (!allowDrop(e)) {
				e.rejectDrag();
				return;
			} else {
				e.acceptDrag(DnDConstants.ACTION_COPY_OR_MOVE);
			}
		}

		public void dragExit(DropTargetEvent dte) {
			// TODO Auto-generated method stub

		}

		public void dragOver(DropTargetDragEvent dtde) {
			// TODO Auto-generated method stub

		}

		public void drop(DropTargetDropEvent e) {
			Selection s = DropUtil.getSelection(e);
			LinkedObject target = SelectionManager.getManager().getSelection()
					.getTermSubSelection();

			TermMacroHistoryItem item = new TermMacroHistoryItem(
					"Added parents");
			Collection<Link> newLinks = new ArrayList<Link>();
			for (LinkedObject parent : s.getTerms()) {
				CreateLinkHistoryItem citem = new CreateLinkHistoryItem(target,
						OBOProperty.IS_A, parent);
				OBORestriction link = new OBORestrictionImpl(target,
						OBOProperty.IS_A, parent);
				newLinks.add(link);
				item.addItem(citem);
			}
			GUIUtil.setSelections(item, SelectionManager.getManager()
					.getSelection(), SelectionManager.createSelectionFromLinks(
					ParentEditor.this, newLinks, null, false));
			e.dropComplete(true);
			SessionManager.getManager().apply(item);
		}

		public void dropActionChanged(DropTargetDragEvent dtde) {
		}

	};

	protected DropTarget dropTarget = new DropTarget(dropButton, dropListener);

	protected Comparator<Link> parentComparator = new Comparator<Link>() {
		public int compare(Link a, Link b) {
			Link tr = a;
			Link tr2 = b;
			int compVal = tr.getParent().getName().compareTo(
					tr2.getParent().getName());
			if (compVal < 0)
				return -1;
			else if (compVal > 0)
				return 1;
			else
				return 0;
		}
	};

	public ParentEditor(String id) {
		super(id);

		// cardinalityPanel.setPreferredSize(new Dimension(200, 30));
		buttonPanel.setLayout(new BorderLayout());
		buttonPanel.add(dropButton, "Center");
		buttonPanel.add(showImpliedCheckbox, "East");
		showImpliedCheckbox.setOpaque(false);

		showImpliedCheckbox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				reload();
			}
		});
	}

	@Override
	public String getName() {
		return "Parent Plugin";
	}

	public void reload() {
		System.err.println("RELOADING");
		loadTerm(currentObject);
	}

	protected static class TypeRenderer extends DefaultListCellRenderer {

		public Component getListCellRendererComponent(JList list, Object value,
				int index, boolean isSelected, boolean cellHasFocus) {
			return super.getListCellRendererComponent(list,
					((IdentifiedObject) value).getName(), index, isSelected,
					cellHasFocus);
		}

	}

	protected static TypeRenderer typeRenderer = new TypeRenderer();

	public void loadTerm(LinkedObject t) {
		currentObject = t;
		outerPanel.removeAll();
		// panel = new JPanel();
		if (t == null) {
			outerPanel.add(label);
			remove(buttonPanel);
		} else {
			add(buttonPanel, "South");
			final TreePath[] oldpaths = PathUtil.getPaths(t);
			List<Link> v = new ArrayList<Link>();
			if (showImpliedCheckbox.isSelected()
					&& SessionManager.getManager().getUseReasoner()) {
				v.addAll(SessionManager.getManager().getReasoner()
						.getParents(t));
			}
			v.addAll(t.getParents());

			Collections.sort(v, parentComparator);
			boolean first = true;
			for (int i = 0; i < v.size(); i++) {
				final OBORestriction tr = (OBORestriction) v.get(i);
				// final TreePath [] oldpaths = tr.getPaths();

				Font font = getFont();
				boolean enabled = true;

				if (TermUtil.isImplied(tr)) {
					enabled = false;
					if (!font.isItalic())
						font = font.deriveFont(Font.ITALIC);
					else
						font = font.deriveFont(Font.PLAIN);
				}

				JPanel panel = new JPanel();
				JPanel controlsPanel = new JPanel();
				controlsPanel.setLayout(new BoxLayout(controlsPanel,
						BoxLayout.Y_AXIS));
				panel.setLayout(new BorderLayout());
				panel.add(controlsPanel, "West");
				final JComboBox typeBox = new JComboBox();
				typeBox.setFont(font);
				typeBox.setEnabled(enabled);

				Iterator it = TermUtil.getRelationshipTypes(
						SessionManager.getManager().getSession()).iterator();
				while (it.hasNext()) {
					OBOProperty type = (OBOProperty) it.next();
					if (TermUtil.isLegalRelationship(tr.getChild(), type, tr
							.getParent()))
						typeBox.addItem(type);
				}
				typeBox.setSelectedItem(tr.getType());
				typeBox.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						OBOProperty type = (OBOProperty) typeBox
								.getSelectedItem();
						HistoryItem item = new LinkTypeHistoryItem(tr, type);
						TreePath[] paths = SelectionManager
								.getGlobalSelection().getPaths();
						GUIUtil.setPreSelection(item, SelectionManager
								.getGlobalSelection());
						TreePath[] outpaths = new TreePath[paths.length];
						for (int i = 0; i < outpaths.length; i++) {
							OBORestriction tr2 = SessionManager.getManager()
									.getSession().getObjectFactory()
									.createOBORestriction(tr.getChild(), type,
											tr.getParent(), false);

							outpaths[i] = paths[i].getParentPath()
									.pathByAddingChild(tr2);
						}
						GUIUtil.setPostSelection(item, SelectionManager
								.createSelectionFromPaths(ParentEditor.this,
										outpaths, null, SessionManager
												.getManager()
												.getCurrentLinkDatabase(),
										RootAlgorithm.GREEDY, true));
						SessionManager.getManager().apply(item);
					}
				});
				typeBox.setRenderer(typeRenderer);

				JButton idButton = new JButton(tr.getParent().getID());
				idButton.setBorder(null);
				if (TermUtil.isDangling(tr.getParent()))
					idButton.setForeground(Color.red);
				else {
					idButton.setForeground(Color.blue);
					idButton.addActionListener(new ActionListener() {
						public void actionPerformed(ActionEvent e) {
							SelectionManager.selectTerm(ParentEditor.this, tr
									.getParent());
						}
					});
				}
				idButton.setFont(font);
				idButton.setOpaque(false);

				JButton field = new JButton();
				field.setOpaque(false);
				field.setBorderPainted(false);
				field.setText("<html>" + tr.getParent().getName() + "</html>");
				field.setToolTipText(tr.getParent().getName());
				field.setFont(font);
				field.setMinimumSize(new Dimension(0, (int) field
						.getMinimumSize().getHeight()));
				// JTextField field = new JTextField();
				// field.setOpaque(false);
				// field.setBorder(null);
				// field.setText(tr.getParent().getName());
				// field.setCaretPosition(0);
				// field.setToolTipText(tr.getParent().getName());
				// field.setEditable(false);
				// field.setFont(font);

				final JButton trashButton = new JButton(Preferences
						.loadLibraryIcon("trashcan.gif"));
				trashButton.setPreferredSize(new Dimension(20, 18));
				trashButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						DeleteLinkHistoryItem ditem = new DeleteLinkHistoryItem(
								tr);
						GUIUtil.setPreSelection(ditem, SelectionManager
								.getGlobalSelection());

						List<TreePath> temp = new ArrayList<TreePath>();
						for (int i = 0; i < oldpaths.length; i++) {
							if (!oldpaths[i].getLastPathComponent().equals(tr)) {
								temp.add(oldpaths[i]);
							}
						}
						TreePath[] paths = temp.toArray(new TreePath[temp
								.size()]);

						GUIUtil.setPostSelection(ditem, SelectionManager
								.createSelectionFromPaths(ParentEditor.this,
										paths, null, SessionManager
												.getManager()
												.getCurrentLinkDatabase(),
										RootAlgorithm.GREEDY, true));
						SessionManager.getManager().apply(ditem);
					}
				});
				trashButton.setEnabled(enabled
						&& tr.getChild().getParents().size() > 1);
				Box topBox = Box.createHorizontalBox();
				topBox.add(trashButton);
				topBox.add(Box.createHorizontalStrut(5));
				topBox.add(typeBox);
				topBox.add(Box.createHorizontalStrut(5));
				topBox.add(idButton);
				Box bottomBox = Box.createHorizontalBox();

				controlsPanel.add(topBox);
				panel.add(field, "Center");
				if (!TermUtil.isProperty(tr.getParent())) {

					JButton cardinalityButton = new JButton();
					cardinalityButton.setFont(font);
					cardinalityButton.addActionListener(new ActionListener() {
						public void actionPerformed(ActionEvent e) {
							editCardinality(tr);
						}
					});
					cardinalityButton.setEnabled(enabled);

					String label = null;
					if (tr.getCardinality() != null) {
						label = tr.getCardinality() + "";
					} else if (tr.getMinCardinality() == null
							&& tr.getMaxCardinality() == null) {
						label = "[*,*]";
					} else {
						if (tr.getMinCardinality() != null) {
							label = "[" + tr.getMinCardinality() + ",";
						}

						if (tr.getMaxCardinality() != null) {
							if (label == null)
								label = "[*," + tr.getMaxCardinality() + "]";
							else
								label = label + tr.getMaxCardinality() + "]";
						} else if (label != null) {
							label = label + "*]";
						}
					}
					cardinalityButton.setText(label);
					bottomBox.add(cardinalityButton);
					bottomBox.add(Box.createHorizontalStrut(5));
				}
				// completesBox.setSelected(tr.isNecessarilyTrue());
				// completesBox.setSelected(tr.isInverseNecessarilyTrue());

				final JComboBox namespaceBox = new JComboBox();
				namespaceBox.setFont(font);
				namespaceBox.setEnabled(enabled);
				it = SessionManager.getManager().getSession().getNamespaces()
						.iterator();
				namespaceBox.addItem("<no namespace>");
				while (it.hasNext()) {
					Namespace ns = (Namespace) it.next();
					namespaceBox.addItem(ns);
				}
				if (tr.getNamespace() == null)
					namespaceBox.setSelectedIndex(0);
				else
					namespaceBox.setSelectedItem(tr.getNamespace());
				namespaceBox.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						Namespace ns = null;
						if (namespaceBox.getSelectedIndex() > 0)
							ns = (Namespace) namespaceBox.getSelectedItem();
						TRNamespaceHistoryItem item = new TRNamespaceHistoryItem(
								tr, ns);
						GUIUtil.setSelections(item, SelectionManager
								.getGlobalSelection(), SelectionManager
								.getGlobalSelection());
						SessionManager.getManager().apply(item);
					}
				});
				bottomBox.add(namespaceBox);
				controlsPanel.add(bottomBox);

				panel.setOpaque(false);
				if (first) {
					outerPanel.add(new JSeparator());
				}
				outerPanel.add(panel);
			}
			outerPanel.add(Box.createVerticalStrut(10));
		}
		validate();
		repaint();
	}

	protected void changeCardinalitySetting(int index) {
		cardinalityPanel.removeAll();
		if (index == 0) {
			cardinalityPanel.add(cardinalityLabel);
			cardinalityPanel.add(Box.createHorizontalStrut(5));
			cardinalityPanel.add(cardinalityField);
		} else {
			cardinalityPanel.add(cardinalityRangeLabel);
			cardinalityPanel.add(Box.createHorizontalStrut(5));
			cardinalityPanel.add(minCardinalityField);
			cardinalityPanel.add(Box.createHorizontalStrut(5));
			cardinalityPanel.add(maxCardinalityField);
		}
		cardinalityPanel.validate();
	}

	protected String toString(Integer a) {
		if (a == null)
			return "";
		else
			return a.toString();
	}

	protected void editCardinality(final OBORestriction tr) {
		final JComboBox cardinalityChooser = new JComboBox(cardinalityChoices);

		final JDialog dialog = new JDialog((Frame) null,
				"Cardinality settings", true);

		JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

		TitledBorder border = new TitledBorder("");
		cardinalityPanel.setBorder(border);
		cardinalityPanel.setLayout(new BoxLayout(cardinalityPanel,
				BoxLayout.X_AXIS));

		cardinalityChooser.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				changeCardinalitySetting(cardinalityChooser.getSelectedIndex());
			}
		});

		minCardinalityField.setText(toString(tr.getMinCardinality()));
		maxCardinalityField.setText(toString(tr.getMaxCardinality()));
		cardinalityField.setText(toString(tr.getCardinality()));

		if (tr.getCardinality() != null)
			cardinalityChooser.setSelectedIndex(0);
		else
			cardinalityChooser.setSelectedIndex(1);

		JButton commit = new JButton("Commit");
		commit.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				List<HistoryItem> changes = new ArrayList<HistoryItem>();
				if (cardinalityChooser.getSelectedIndex() == 0) {
					Integer cardinality = null;
					try {
						cardinality = new Integer(cardinalityField.getText());
					} catch (NumberFormatException ex) {
					}
					if (!ObjectUtil.equals(cardinality, tr.getCardinality())) {
						changes
								.add(new CardinalityHistoryItem(tr, cardinality));
					}
					if (tr.getMinCardinality() != null)
						changes.add(new MinCardinalityHistoryItem(tr, null));
					if (tr.getMaxCardinality() != null)
						changes.add(new MaxCardinalityHistoryItem(tr, null));
				} else {
					Integer maxCardinality = null;
					Integer minCardinality = null;
					try {
						minCardinality = new Integer(minCardinalityField
								.getText());
					} catch (NumberFormatException ex) {
					}
					try {
						maxCardinality = new Integer(maxCardinalityField
								.getText());
					} catch (NumberFormatException ex) {
					}

					if (!ObjectUtil.equals(minCardinality, tr
							.getMinCardinality())) {
						changes.add(new MinCardinalityHistoryItem(tr,
								minCardinality));
					}
					if (!ObjectUtil.equals(maxCardinality, tr
							.getMaxCardinality())) {
						changes.add(new MaxCardinalityHistoryItem(tr,
								maxCardinality));
					}

					if (tr.getCardinality() != null)
						changes.add(new CardinalityHistoryItem(tr, null));
				}
				HistoryItem item = null;
				if (changes.size() == 1) {
					item = changes.get(0);
				} else if (changes.size() > 0) {
					item = new TermMacroHistoryItem("Cardinality change");
					for (int i = 0; i < changes.size(); i++)
						((TermMacroHistoryItem) item).addItem(changes.get(i));
				}
				if (item != null) {
					GUIUtil.setSelections(item, SelectionManager
							.getGlobalSelection(), SelectionManager
							.getGlobalSelection());
					SessionManager.getManager().apply(item);
				}
				dialog.setVisible(false);
				dialog.dispose();
			}
		});
		Box commitBox = new Box(BoxLayout.X_AXIS);
		commitBox.add(Box.createHorizontalGlue());
		commitBox.add(commit);
		commitBox.add(Box.createHorizontalGlue());

		panel.add(Box.createVerticalStrut(10));
		panel.add(cardinalityChooser);
		panel.add(cardinalityPanel);
		panel.add(commitBox);
		panel.add(Box.createVerticalGlue());
		panel.validate();
		dialog.setContentPane(panel);
		dialog.pack();
		dialog.setVisible(true);
	}

	@Override
	public void init() {

		inv_icon = Preferences.loadLibraryIcon("inv_icon.gif");
		nec_icon = Preferences.loadLibraryIcon("nec_icon.gif");
		completes_icon = Preferences.loadLibraryIcon("completes.gif");

		setLayout(new BorderLayout());
		attachListeners();
		setBorder(emptyBorder);
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		loadTerm(SelectionManager.getGlobalSelection().getTermSubSelection());
		outerPanel.setLayout(new BoxLayout(outerPanel, BoxLayout.Y_AXIS));
		dropButton.setBorderPainted(false);
		dropButton.setBackground(Preferences.defaultButtonColor());
		JScrollPane pane = new JScrollPane(outerPanel,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		pane.setOpaque(false);
		removeAll();
		add(pane, "Center");
		add(buttonPanel, "South");
	}

	protected void attachListeners() {
		SelectionManager.getManager().addSelectionListener(selectionListener);
		GUIUtil.addReloadListener(reloadListener);
	}

	protected void removeListeners() {
		SelectionManager.getManager()
				.removeSelectionListener(selectionListener);
		GUIUtil.removeReloadListener(reloadListener);
	}

	@Override
	public void cleanup() {
		removeListeners();
	}
}

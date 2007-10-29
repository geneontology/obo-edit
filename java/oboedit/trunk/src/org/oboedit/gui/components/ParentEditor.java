package org.oboedit.gui.components;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.swing.*;
import org.bbop.util.*;
import org.obo.datamodel.*;
import org.obo.history.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.gui.event.*;
import org.oboedit.util.GUIUtil;
import org.oboedit.util.PathUtil;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.tree.*;
import java.net.URL;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

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

	HistoryListener historyListener = new HistoryListener() {
		public void applied(HistoryAppliedEvent e) {
			loadTerm(SelectionManager.getGlobalSelection()
					.getTermSubSelection());
		}

		public void reversed(HistoryAppliedEvent e) {
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

	protected JPanel panel = new JPanel();

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

	/*
	 * protected DropListener dropListener = new DropListener() { LineBorder
	 * border = new LineBorder(Color.black, 2);
	 * 
	 * public boolean allowDrop(DragEvent e) { if (e.getData() instanceof
	 * TreePath[] && ((TreePath[]) e.getData()).length > 0) { TreePath[] paths =
	 * (TreePath[]) e.getData(); for (int i = 0; i < paths.length; i++) if
	 * (!(paths[i].getLastPathComponent() instanceof OBORestriction)) return
	 * false; return true; } else return false; }
	 * 
	 * public void dragEnter(DragEvent e) { if (allowDrop(e)) setBorder(border); }
	 * 
	 * public void dragExit(DragEvent e) { setBorder(emptyBorder); }
	 * 
	 * public void drop(DragEvent e) { LinkedObject lo =
	 * SelectionManager.getSelection().getTermSubSelection();
	 * 
	 * TreePath[] paths = (TreePath[]) e.getData(); TermMacroHistoryItem item =
	 * new TermMacroHistoryItem( "Added parents"); TreePath[] tpaths = new
	 * TreePath[paths.length]; for (int i = 0; i < paths.length; i++) {
	 * OBORestriction tr = (OBORestriction) paths[i] .getLastPathComponent();
	 * TermCopyHistoryItem citem = new TermCopyHistoryItem(tr .getChild(), lo,
	 * OBOProperty.IS_A); item.addHistoryItem(citem); OBORestriction tr2 =
	 * controller.getSession().getObjectFactory() .createOBORestriction(lo,
	 * OBOProperty.IS_A, tr.getChild(), false, null); TreePath path =
	 * paths[i].pathByAddingChild(tr2); tpaths[i] = path; }
	 * item.setPreSelection(SelectionManager.createSelectionFromPaths(ParentPlugin.this,
	 * paths, null, true));
	 * item.setPostSelection(SelectionManager.createSelectionFromPaths(ParentPlugin.this,
	 * tpaths, null, true)); controller.apply(item); setBorder(emptyBorder); }
	 * 
	 * public void draggedOver(DragEvent e) { } };
	 * 
	 * protected DropTarget dropTarget = new DropTarget(dropButton,
	 * dropListener);
	 */
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

	public void loadTerm(LinkedObject t) {
		currentObject = t;
		outerPanel.removeAll();
		// panel = new JPanel();
		if (t == null) {
			outerPanel.add(label);
			panel.remove(buttonPanel);
		} else {
			panel.add(buttonPanel, "Center");
			final TreePath[] oldpaths = PathUtil.getPaths(t);
			Vector<Link> v = new Vector<Link>();
			if (showImpliedCheckbox.isSelected()
					&& SessionManager.getManager().getUseReasoner()) {
				v.addAll(SessionManager.getManager().getReasoner()
						.getParents(t));
			}
			v.addAll(t.getParents());

			Collections.sort(v, parentComparator);
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
				panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
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

				JTextField field = new JTextField();
				field.setOpaque(false);
				field.setBorder(null);
				field.setText(tr.getParent().getName());
				field.setCaretPosition(0);
				field.setToolTipText(tr.getParent().getName());
				field.setEditable(false);
				field.setFont(font);

				final JButton trashButton = new JButton(Preferences
						.loadLibraryIcon("trashcan.gif"));
				trashButton.setPreferredSize(new Dimension(20, 18));
				trashButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						DeleteLinkHistoryItem ditem = new DeleteLinkHistoryItem(
								tr);
						GUIUtil.setPreSelection(ditem, SelectionManager
								.getGlobalSelection());

						Vector<TreePath> temp = new Vector<TreePath>();
						for (int i = 0; i < oldpaths.length; i++) {
							if (!oldpaths[i].getLastPathComponent().equals(tr)) {
								temp.add(oldpaths[i]);
							}
						}
						TreePath[] paths = new TreePath[temp.size()];
						temp.copyInto(paths);

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
				panel.add(trashButton);
				panel.add(Box.createHorizontalStrut(5));
				panel.add(typeBox);
				panel.add(Box.createHorizontalStrut(5));
				panel.add(idButton);
				panel.add(Box.createHorizontalStrut(5));
				panel.add(field);
				panel.add(Box.createHorizontalGlue());
				if (!TermUtil.isProperty(tr.getParent())) {
//					JLabel completesLabel = new JLabel(completes_icon);
//					final JCheckBox completesBox = new JCheckBox("");
//					completesBox.setSelected(tr.completes());
//					completesBox.setFont(font);
//					completesBox.setToolTipText("Completes");
//					completesBox.setEnabled(enabled);
//					panel.add(completesLabel);
//					panel.add(completesBox);
//					panel.add(Box.createHorizontalStrut(5));
//					completesBox.setOpaque(false);
//					completesBox.addActionListener(new ActionListener() {
//						public void actionPerformed(ActionEvent e) {
//							CompletesHistoryItem item = new CompletesHistoryItem(
//									tr);
//							GUIUtil.setSelections(item, SelectionManager
//									.getGlobalSelection(), SelectionManager
//									.getGlobalSelection());
//							SessionManager.getManager().apply(item);
//						}
//					});
//
//					JLabel necLabel = new JLabel(nec_icon);
//					final JCheckBox necessaryBox = new JCheckBox("");
//					necessaryBox.setSelected(tr.isNecessarilyTrue());
//					necessaryBox.setFont(font);
//					necessaryBox.setToolTipText("Necessarily true");
//					necessaryBox.setEnabled(enabled);
//					panel.add(necLabel);
//					panel.add(necessaryBox);
//					panel.add(Box.createHorizontalStrut(5));
//					necessaryBox.setOpaque(false);
//					necessaryBox.setEnabled(enabled);
//					necessaryBox.addActionListener(new ActionListener() {
//						public void actionPerformed(ActionEvent e) {
//							NecessarilyTrueHistoryItem item = new NecessarilyTrueHistoryItem(
//									tr);
//							GUIUtil.setSelections(item, SelectionManager
//									.getGlobalSelection(), SelectionManager
//									.getGlobalSelection());
//							SessionManager.getManager().apply(item);
//						}
//					});
//
//					JLabel invLabel = new JLabel(inv_icon);
//					JCheckBox invNecessaryBox = new JCheckBox("");
//					invNecessaryBox.setSelected(tr.isInverseNecessarilyTrue());
//					invNecessaryBox.setFont(font);
//					invNecessaryBox.setToolTipText("Inverse necessarily true");
//					invNecessaryBox.setEnabled(enabled);
//					panel.add(invLabel);
//					panel.add(invNecessaryBox);
//					panel.add(Box.createHorizontalStrut(5));
//					invNecessaryBox.setOpaque(false);
//					invNecessaryBox.addActionListener(new ActionListener() {
//						public void actionPerformed(ActionEvent e) {
//							InverseNecHistoryItem item = new InverseNecHistoryItem(
//									tr);
//							GUIUtil.setSelections(item, SelectionManager
//									.getGlobalSelection(), SelectionManager
//									.getGlobalSelection());
//							SessionManager.getManager().apply(item);
//						}
//					});

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
					panel.add(cardinalityButton);
					panel.add(Box.createHorizontalStrut(5));
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
				panel.add(namespaceBox);
				panel.add(Box.createHorizontalStrut(5));

				panel.setOpaque(false);
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
				Vector<HistoryItem> changes = new Vector<HistoryItem>();
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
		dropButton.setBorder(null);
		panel.setOpaque(true);
		panel.setLayout(new BorderLayout());
		JScrollPane pane = new JScrollPane(panel,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		panel.removeAll();
		removeAll();
		panel.add(outerPanel, "North");
		panel.add(buttonPanel, "Center");
		add(pane, "Center");
	}

	protected void attachListeners() {
		SelectionManager.getManager().addSelectionListener(selectionListener);
		sessionManager.addHistoryListener(historyListener);
	}

	protected void removeListeners() {
		SelectionManager.getManager()
				.removeSelectionListener(selectionListener);
		sessionManager.removeHistoryListener(historyListener);
	}

	@Override
	public void cleanup() {
		removeListeners();
	}
}

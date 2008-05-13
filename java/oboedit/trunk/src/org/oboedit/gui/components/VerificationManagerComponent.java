package org.oboedit.gui.components;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.GUIManager;
import org.bbop.util.*;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOSession;
import org.oboedit.controller.SessionManager;
import org.oboedit.controller.VerificationManager;
import org.oboedit.gui.*;
import org.oboedit.gui.event.*;
import org.oboedit.gui.widget.CheckWarningComponent;
import org.oboedit.verify.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.table.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

public class VerificationManagerComponent extends AbstractGUIComponent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected JTable table = new JTable() {
		// Override this method so that it returns the preferred
		// size of the JTable instead of the default fixed size
		@Override
		public Dimension getPreferredScrollableViewportSize() {
			return getPreferredSize();
		}
	};

	protected JButton checkButton = new JButton("Run manual check now");

	protected JButton addCheckButton = new JButton("Add check");

	protected JButton removeCheckButton = new JButton("Remove check");

	protected JTabbedPane tabbedPane = new JTabbedPane();

	protected JPanel configPanel = new JPanel();

	protected CheckWarningComponent warningComponent = new CheckWarningComponent();

	protected JScrollPane warningScroller = new JScrollPane(warningComponent,
			JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

	protected JProgressBar progressBar = new JProgressBar();

	protected JPanel progressPanel = new JPanel();

	protected java.util.List checkList = new ArrayList();

	protected VerificationListener verificationListener = new VerificationListener() {
		public void verificationComplete(VerificationEvent e) {
			if (!VerificationManager.isTextThreadCondition(e.getCondition()))
				setLastResults(e.getWarnings(), e.getSession(), e
						.getCurrentObject(), e.getCondition());
		}

		public void verificationStarting(VerificationEvent e) {
		}
	};

	protected class VerificationTableModel extends AbstractTableModel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		protected int sortCol = 0;

		protected boolean reverseOrder = false;

		protected Comparator comparator = new Comparator() {
			public int compare(Object a, Object b) {
				Object aval = getValue((Check) a, sortCol);
				Object bval = getValue((Check) b, sortCol);
				int compVal = 0;
				if (aval instanceof Comparable && bval instanceof Comparable) {
					compVal = ((Comparable) aval).compareTo(bval);
				}
				if (reverseOrder) {
					compVal *= -1;
				}
				return compVal;
			}
		};

		public VerificationTableModel() {
		}

		public void setData(Collection checks) {
			checkList.clear();
			checkList.addAll(checks);
			doSort();
			fireTableStructureChanged();
		}

		public Check getCheck(int row) {
			return (Check) checkList.get(row);
		}

		public void removeCheck(int row) {
			checkList.remove(row);
			fireTableStructureChanged();
		}

		public void setSortColumn(int sortCol) {
			if (this.sortCol == sortCol)
				reverseOrder = !reverseOrder;
			else {
				this.sortCol = sortCol;
				reverseOrder = true;
			}
			doSort();
		}

		protected void doSort() {
			Collections.sort(checkList, comparator);
		}

		protected Object getValue(Check check, int col) {
			if (col == 0)
				return check.getDescription();
			else if (col == 1)
				return wrapBoolean(VerificationManager
						.isTextCommitCondition(check));
			else if (col == 2)
				return wrapBoolean(VerificationManager
						.isTextThreadCondition(check));
			else if (col == 3)
				return wrapBoolean(VerificationManager
						.isReasonerActivatedCondition(check));
			else if (col == 4)
				return wrapBoolean(VerificationManager.isSaveCondition(check));
			else if (col == 5)
				return wrapBoolean(VerificationManager.isLoadCondition(check));
			else if (col == 6)
				return wrapBoolean(VerificationManager.isManualCondition(check));
			else if (col == 7)
				return check.getConfiguration();
			else
				return null;
		}

		protected void setValue(Check check, int col, Object o) {
			if (col == 1) {
				VerificationManager.setConditionAtField(check,
						VerificationManager.TEXT_EDIT_COMMIT, ((Boolean) o)
								.booleanValue());
			} else if (col == 2) {
				VerificationManager.setConditionAtField(check,
						VerificationManager.TEXT_EDIT_THREAD, ((Boolean) o)
								.booleanValue());
			} else if (col == 3) {
				VerificationManager.setConditionAtField(check,
						VerificationManager.REASONER_ACTIVATED, ((Boolean) o)
								.booleanValue());
			} else if (col == 4) {
				VerificationManager.setConditionAtField(check,
						VerificationManager.SAVE, ((Boolean) o).booleanValue());
			} else if (col == 5) {
				VerificationManager.setConditionAtField(check,
						VerificationManager.LOAD, ((Boolean) o).booleanValue());
			} else if (col == 6) {
				VerificationManager.setConditionAtField(check,
						VerificationManager.MANUAL, ((Boolean) o)
								.booleanValue());
			}
		}

		protected Boolean wrapBoolean(boolean b) {
			if (b)
				return Boolean.TRUE;
			else
				return Boolean.FALSE;
		}

		@Override
		public Class getColumnClass(int col) {
			if (col == 0) {
				return String.class;
			} else if (col == 1) {
				return Boolean.class;
			} else if (col == 2) {
				return Boolean.class;
			} else if (col == 3) {
				return Boolean.class;
			} else if (col == 4) {
				return Boolean.class;
			} else if (col == 5) {
				return Boolean.class;
			} else if (col == 6) {
				return Boolean.class;
			} else if (col == 7) {
				return CheckConfiguration.class;
			} else
				return Object.class;

		}

		public int getColumnCount() {
			return 8;
		}

		@Override
		public String getColumnName(int col) {
			if (col == 0)
				return "Check Name";
			else if (col == 1)
				return "On text commit?";
			else if (col == 2)
				return "On text edit?";
			else if (col == 3)
				return "On reasoner?";
			else if (col == 4)
				return "On save?";
			else if (col == 5)
				return "On load?";
			else if (col == 6)
				return "On manual?";
			else if (col == 7)
				return "Configure";
			else
				return "";
		}

		public int getRowCount() {
			return checkList.size();
		}

		public Object getValueAt(int row, int column) {
			Check check = (Check) checkList.get(row);
			return getValue(check, column);
		}

		@Override
		public boolean isCellEditable(int row, int col) {
			return (col > 0);
		}

		@Override
		public void setValueAt(Object val, int row, int col) {
			Check check = (Check) checkList.get(row);
			setValue(check, col, val);
		}
	}

	protected class ButtonRenderer extends JButton implements TableCellRenderer {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public ButtonRenderer() {
			super("Configure");
		}

		public Component getTableCellRendererComponent(JTable table,
				Object value, boolean isSelected, boolean hasFocus, int row,
				int column) {
			Check check = (Check) checkList.get(row);
			setEnabled(check.getConfigurationPanel() != null);
			return this;
		}
	}

	protected class ButtonEditor extends AbstractCellEditor implements
			TableCellEditor {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public Component getTableCellEditorComponent(JTable table,
				Object value, boolean isSelected, int row, int column) {
			final Check check = (Check) checkList.get(row);
			JButton configButton = new JButton("Configure");
			configButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					configureCheck(check);
				}
			});
			configButton.setEnabled(check.getConfigurationPanel() != null);
			return configButton;
		}

		public Object getCellEditorValue() {
			return null;
		}
	}

	protected ButtonRenderer buttonRenderer = new ButtonRenderer();

	protected ButtonEditor buttonEditor = new ButtonEditor();

	protected VerificationTableModel tableModel = new VerificationTableModel();

	protected JCheckBox textEditWarningCheckbox = new JCheckBox(
			"On text edits?");

	protected JCheckBox reasonerWarningCheckbox = new JCheckBox("On reasoner?");

	protected JCheckBox saveWarningCheckbox = new JCheckBox("On save?");

	protected JCheckBox checkObsoletesCheckbox = new JCheckBox(
			"Check obsoletes?");

	protected JCheckBox loadWarningCheckbox = new JCheckBox("On load?");

	protected JCheckBox manualWarningCheckbox = new JCheckBox("On manual?");

	protected ActionListener warningCheckboxListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			updateEngineSettings();
		}
	};

	public VerificationManagerComponent(String id) {
		super(id);
		setLayout(new GridLayout(1, 1));

		warningComponent.setHyperlinksEnabled(true);
		tabbedPane.addTab("Configuration", configPanel);
		tabbedPane.addTab("Verification Results", warningScroller);
		table.setModel(tableModel);
		TableColumn column = null;
		for (int i = 0; i < 5; i++) {
			column = table.getColumnModel().getColumn(i);
			if (i > 0) {
				column.setPreferredWidth(20); // sport column is bigger
			}
			column.setHeaderValue(tableModel.getColumnName(i));
		}
		checkButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				runChecks();
			}
		});
		addCheckButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addCheck();
			}
		});
		removeCheckButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				removeCheck();
			}
		});
		textEditWarningCheckbox.addActionListener(warningCheckboxListener);
		reasonerWarningCheckbox.addActionListener(warningCheckboxListener);
		saveWarningCheckbox.addActionListener(warningCheckboxListener);
		checkObsoletesCheckbox.addActionListener(warningCheckboxListener);

		loadWarningCheckbox.addActionListener(warningCheckboxListener);
		manualWarningCheckbox.addActionListener(warningCheckboxListener);

		progressPanel.setLayout(new BorderLayout());
		progressBar.setStringPainted(true);
		progressPanel.add(progressBar, "North");
		Box commitButtonBox = new Box(BoxLayout.X_AXIS);
		commitButtonBox.add(checkButton);

		Box buttonBox = new Box(BoxLayout.X_AXIS);
		buttonBox.add(Box.createHorizontalGlue());
		buttonBox.add(addCheckButton);
		buttonBox.add(Box.createHorizontalStrut(10));
		buttonBox.add(removeCheckButton);
		buttonBox.add(Box.createHorizontalGlue());

		JPanel checkboxPanel = new JPanel();
		checkboxPanel.add(textEditWarningCheckbox);
		checkboxPanel.add(reasonerWarningCheckbox);
		checkboxPanel.add(saveWarningCheckbox);
		checkboxPanel.add(loadWarningCheckbox);
		checkboxPanel.add(manualWarningCheckbox);
		TitledBorder titledBorder = new TitledBorder("Show warnings");
		checkboxPanel.setBorder(titledBorder);

		Box otherOptionsPanel = new Box(BoxLayout.X_AXIS);
		otherOptionsPanel.add(Box.createHorizontalGlue());
		otherOptionsPanel.add(checkObsoletesCheckbox);
		otherOptionsPanel.add(Box.createHorizontalGlue());

		JPanel southPanel = new JPanel();
		southPanel.setLayout(new BoxLayout(southPanel, BoxLayout.Y_AXIS));
		southPanel.setOpaque(false);
		southPanel.add(checkboxPanel);
		southPanel.add(otherOptionsPanel);
		southPanel.add(buttonBox);
		southPanel.add(commitButtonBox);

		packColumns(table, 0);
		packRows(table, 0);

//		System.err
//				.println("table preferred size = " + table.getPreferredSize());

		configPanel.setLayout(new BorderLayout());
		configPanel.add(new JScrollPane(table,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER), "Center");
		// configPanel.add(table, "Center");
		configPanel.add(southPanel, "South");
		add(tabbedPane);

		table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

		ListSelectionModel rowSM = table.getSelectionModel();
		rowSM.addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				updateButtonStates();
			}
		});
	}

	public void packColumns(JTable table, int margin) {
		for (int c = 0; c < table.getColumnCount(); c++) {
			packColumn(table, c, margin);
		}
	}

	// Sets the preferred width of the visible column specified by vColIndex.
	// The column
	// will be just wide enough to show the column head and the widest cell in
	// the column.
	// margin pixels are added to the left and right
	// (resulting in an additional width of 2*margin pixels).
	public void packColumn(JTable table, int vColIndex, int margin) {
		DefaultTableColumnModel colModel = (DefaultTableColumnModel) table
				.getColumnModel();
		TableColumn col = colModel.getColumn(vColIndex);
		int width = 0;

		// Get width of column header
		TableCellRenderer renderer = col.getHeaderRenderer();
		if (renderer == null) {
			renderer = table.getTableHeader().getDefaultRenderer();
		}
		Component comp = renderer.getTableCellRendererComponent(table, col
				.getHeaderValue(), false, false, 0, 0);
		width = comp.getPreferredSize().width;

		// Get maximum width of column data
		for (int r = 0; r < table.getRowCount(); r++) {
			renderer = table.getCellRenderer(r, vColIndex);
			comp = renderer.getTableCellRendererComponent(table, table
					.getValueAt(r, vColIndex), false, false, r, vColIndex);
			width = Math.max(width, comp.getPreferredSize().width);
		}

		// Add margin
		width += 2 * margin;

		// Set the width
		col.setPreferredWidth(width);
	}

	// Returns the preferred height of a row.
	// The result is equal to the tallest cell in the row.
	public int getPreferredRowHeight(JTable table, int rowIndex, int margin) {
		// Get the current default height for all rows
		int height = table.getRowHeight();

		// Determine highest cell in the row
		for (int c = 0; c < table.getColumnCount(); c++) {
			TableCellRenderer renderer = table.getCellRenderer(rowIndex, c);
			Component comp = table.prepareRenderer(renderer, rowIndex, c);
			int h = comp.getPreferredSize().height + 2 * margin;
			height = Math.max(height, h);
		}
		return height;
	}

	// The height of each row is set to the preferred height of the
	// tallest cell in that row.
	public void packRows(JTable table, int margin) {
		packRows(table, 0, table.getRowCount(), margin);
	}

	// For each row >= start and < end, the height of a
	// row is set to the preferred height of the tallest cell
	// in that row.
	public void packRows(JTable table, int start, int end, int margin) {
		for (int r = 0; r < table.getRowCount(); r++) {
			// Get the preferred height
			int h = getPreferredRowHeight(table, r, margin);

			// Now set the row height using the preferred height
			if (table.getRowHeight(r) != h) {
				table.setRowHeight(r, h);
			}
		}
	}

	protected void updateButtonStates() {
		int selectedRow = table.getSelectedRow();
		Check check = null;
		if (selectedRow != -1) {
			check = tableModel.getCheck(selectedRow);
		}
		removeCheckButton.setEnabled(check != null
				&& check instanceof UserCheck);
	}

	protected void showProgressBar() {
		tabbedPane.setComponentAt(1, progressPanel);
		tabbedPane.setEnabledAt(1, true);
		tabbedPane.setSelectedIndex(1);
	}

	protected void setLastResults(Collection c, OBOSession session,
			IdentifiedObject currentObject, byte condition) {
		boolean showCondition = VerificationManager.getConditionAtField(
				VerificationManager.getManager().getWarningConditions(),
				condition);
		warningComponent.setRerunInfo(session, currentObject, condition);
		warningComponent.setWarnings(c, null, null, true, showCondition, true);
		tabbedPane.setComponentAt(1, warningScroller);
		tabbedPane.setEnabledAt(1, true);
		tabbedPane.setSelectedIndex(1);
		tabbedPane.revalidate();
		tabbedPane.repaint();
	}

	@Override
        // Note: if this method returns false, then this component continues
        // to run in the background even when it's not in the current layout.
        // This is probably not a major issue for this component (though it is
        // for Graph Editor).  The downside of returning true is that if the
        // user switches between layouts (e.g. Edit and Verify), the component
        // forgets what it was showing.
	public boolean teardownWhenHidden() {
		return false;
	}

	protected void runChecks() {
		CheckTask task = VerificationManager.getManager().getCheckTask(
						SessionManager.getManager().getSession(),
						(IdentifiedObject) null, VerificationManager.MANUAL);
		GUIManager.getManager().scheduleTask(task, true);
	}

	protected void addCheck() {
		Check check = new UserFilterCheck();
		VerificationManager.getManager().installCheck(check);
		tableModel.setData(VerificationManager.getManager().getChecks());
		configureCheck(check);
	}

	protected void removeCheck() {
		int selectedRow = table.getSelectedRow();
		Check check = tableModel.getCheck(selectedRow);
		VerificationManager.getManager().removeCheck(check);
		tableModel.setData(VerificationManager.getManager().getChecks());
	}

	protected void updateEngineSettings() {
		byte warningCondition = 0;
		if (textEditWarningCheckbox.isSelected())
			warningCondition ^= VerificationManager.TEXT_EDIT_COMMIT;
		if (reasonerWarningCheckbox.isSelected())
			warningCondition ^= VerificationManager.REASONER_ACTIVATED;
		if (saveWarningCheckbox.isSelected())
			warningCondition ^= VerificationManager.SAVE;
		if (loadWarningCheckbox.isSelected())
			warningCondition ^= VerificationManager.LOAD;
		if (manualWarningCheckbox.isSelected())
			warningCondition ^= VerificationManager.MANUAL;
		VerificationManager.getManager().setWarningConditions(warningCondition);
		VerificationManager.getManager().setCheckObsoletes(
				checkObsoletesCheckbox.isSelected());
	}

	protected void configureCheck(Check check) {
		final JDialog dialog = new JDialog(GUIManager.getManager().getFrame(),
				"Configuring check \"" + check.getDescription() + "\"", true);
		JComponent configurationPanel = check.getConfigurationPanel();

		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());

		JButton okButton = new JButton("Ok");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				dialog.dispose();
			}
		});
		if (configurationPanel instanceof ActionListener) {
			okButton.addActionListener((ActionListener) configurationPanel);
		}
		dialog.setContentPane(panel);
		panel.add(configurationPanel, "Center");
		panel.add(okButton, "South");
		dialog.pack();
		dialog.setVisible(true);
	}

	@Override
	public void init() {
		VerificationManager.getManager().addVerificationListener(
				verificationListener);

		tableModel.setData(VerificationManager.getManager().getChecks());

		byte warningCondition = VerificationManager.getManager()
				.getWarningConditions();
		textEditWarningCheckbox.setSelected(VerificationManager
				.getConditionAtField(warningCondition,
						VerificationManager.TEXT_EDIT_COMMIT));
		reasonerWarningCheckbox.setSelected(VerificationManager
				.getConditionAtField(warningCondition,
						VerificationManager.REASONER_ACTIVATED));
		saveWarningCheckbox
				.setSelected(VerificationManager.getConditionAtField(
						warningCondition, VerificationManager.SAVE));
		loadWarningCheckbox
				.setSelected(VerificationManager.getConditionAtField(
						warningCondition, VerificationManager.LOAD));
		manualWarningCheckbox.setSelected(VerificationManager
				.getConditionAtField(warningCondition,
						VerificationManager.MANUAL));
		checkObsoletesCheckbox.setSelected(VerificationManager.getManager()
				.getCheckObsoletes());

		table.setDefaultRenderer(CheckConfiguration.class, buttonRenderer);

		table.setDefaultEditor(CheckConfiguration.class, buttonEditor);

		tabbedPane.setEnabledAt(1, false);
	}

	@Override
	public void cleanup() {
		VerificationManager.getManager().removeVerificationListener(
				verificationListener);
	}

	@Override
	public String getName() {
		return "Verification Plugin";
	}
}

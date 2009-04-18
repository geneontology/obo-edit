package org.oboedit.gui.components;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;

import javax.swing.AbstractCellEditor;
import javax.swing.Box;
import javax.swing.DefaultCellEditor;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import org.bbop.framework.AbstractGUIComponent;

import org.obo.datamodel.Link;
import org.obo.datamodel.Namespace;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;

import org.oboedit.controller.SessionManager;
import org.oboedit.gui.AbstractAssertLinksTableModel;
import org.oboedit.gui.AssertLinksTable;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.SearchComponentFactory;

import org.apache.log4j.Logger;

public class AssertLinksComponent extends AbstractGUIComponent {

	//	initialize logger
	protected final static Logger logger = Logger.getLogger(AssertLinksComponent.class);

	protected JPanel panel = new JPanel();
	protected Box northPanel = Box.createHorizontalBox();
	protected Box southPanel = Box.createHorizontalBox();
	protected JButton assertButton = new JButton("Assert");
	protected JButton selectAllButton = new JButton("Select All");
	protected JButton clearButton = new JButton("Clear");


	protected Collection<Link> allLinks = null;
	protected Collection<Link> impliedLinks = null;

	protected SearchComponentFactory factory;

	public AssertLinksComponent(String id) {
		super(id);
		setLayout(new BorderLayout());
		northPanel.add(Box.createVerticalGlue());
		southPanel.add(Box.createGlue());

//		JLabel introText = new JLabel(
//		"The links below are implied and should be asserted "
//		+ "(unless implied links are to be asserted at the time of ontology publishing). ");
		impliedLinks = getImpliedLinks();

		displayResults(impliedLinks);

		assertButton.setToolTipText("Assert selected links");	
		assertButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				assertLinks();
			}
		});


		southPanel.add(selectAllButton);
		southPanel.add(clearButton);
		southPanel.add(assertButton);

		add(northPanel,"Center");
		add(southPanel, "South");

//		add(introText, "North");
		revalidate();
	}

	protected void assertLinks(){
		logger.debug("AssertLinksComponent.assertLinks");
		// get selected rows from table

		//assert
	}

	protected Collection<Link> getImpliedLinks(){
		ReasonedLinkDatabase reasoner = SessionManager.getManager().getReasoner();

		Iterator<Link> it = TermUtil.getAllLinks(reasoner);
		allLinks = new LinkedHashSet<Link>();
		impliedLinks = new LinkedList<Link>();
		while (it.hasNext()) {
			final Link link = it.next();
			if (TermUtil.isImplied(link)) {
//				logger.info("link: "+link);
				allLinks.add(link);
				Namespace subjNS = link.getChild().getNamespace();
				Namespace objNS = link.getParent().getNamespace();
//				logger.info("ns -- subNS " + subjNS + " -- objNS " +objNS);
				if (subjNS != null && !subjNS.equals(objNS)) {
					continue;
				}
				if (!ReasonerUtil.shouldBeTrimmed(reasoner, link) &&
						!impliedLinks.contains(link)) {
					logger.debug("Proposed new link: " + link);
					impliedLinks.add(link);
				}
			}
		}
//		logger.debug("> impliedLinks size: " +  impliedLinks.size());
		return impliedLinks;		
	}



	protected static class AssertedLinksModel extends
	AbstractAssertLinksTableModel<Link> {
		private static final long serialVersionUID = 1L;

		public AssertedLinksModel() {
			super(Link.class);
		}
		public int getColumnCount() {
			return 4;
		}
		@Override
		public Object getColumnVal(Object row, int column) {
//			logger.debug("AssertLinksComponent.getColumnVal");
			Link link = (Link) row;

			if (column == 0){
				if(link.getChild().getName() ==  null)
					logger.error("ERROR - AssertLinksComponent.getColumnVal -- link.getChild().getName() == null for link: " + link);
				return link.getChild().getName();
			}
			else if (column == 1)
				return link.getParent().getName();
			//checkbox
			else if (column == 2)
				return new Boolean(false);
			//explanation button
			else if (column == 3)
				return new Object[]{"expButton"};
			else
				throw new IllegalArgumentException("column out of range");
		}


		/*
		 * JTable uses this method to determine the default renderer/
		 * editor for each cell.  If we didn't implement this method,
		 * then the explnation column would contain text ("true"/"false"),
		 * rather than a check box.
		 */
		public Class getColumnClass(int column) {
//			logger.debug(getValueAt(0, column).getClass());
			return getValueAt(0, column).getClass();
		}

		@Override
		public boolean columnHasMaxWidth(int column) {
			if (column == 0 || column == 1)
				return true;
			else
				return false;
		}
		@Override
		public String getColumnName(int index) {
			if (index == 0)
				return "Child name";
			else if (index == 1)
				return "Parent name";
			else if (index == 2)
				return "Select";
			else if (index == 3)
				return "Explanation";
			else
				return "?!";
		}

		// only allow the column with checkboxes (#2) to be editable
		public boolean isCellEditable(int rowIndex, int columnIndex) {
			return (columnIndex == 2);
		}
	}

	public void displayResults(Collection<Link> impliedLinks){
		logger.debug("displayResults -- impliedLinks.size(): " + impliedLinks.size());
		JTable table = new AssertLinksTable(new AssertedLinksModel(), impliedLinks);

		table.getColumnModel().getColumn(2).setCellEditor(new CheckBoxCellEditor());

		table.getColumnModel().getColumn(3).setCellRenderer(new ButtonRenderer());
		table.getColumnModel().getColumn(3).setCellEditor(new ButtonEditor(new JCheckBox()));

		northPanel.add(new JScrollPane(table));
		repaint();
	}

	public String getName() {
		return "Assert Implied Links Panel";
	}

	private class ButtonRenderer extends JButton implements TableCellRenderer {
		public ButtonRenderer() {
			setOpaque(true);
		}
		public Component getTableCellRendererComponent(JTable table, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			if (isSelected) {
				setForeground(table.getSelectionForeground());
				setBackground(table.getSelectionBackground());
			} else{
				setForeground(table.getForeground());
				setBackground(UIManager.getColor("Button.background"));
			}
			return this;
		}
	}

	private class ButtonEditor extends DefaultCellEditor {
		protected JButton expButton;
		private boolean isPushed;

		public ButtonEditor(JCheckBox checkBox) {
			super(checkBox);
//			expButton = new JButton("expButton");
			expButton = new JButton(Preferences.loadLibraryIcon("info_icon.png"));
			expButton.setToolTipText("Assert selected links");
			expButton.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e){
					logger.debug(">> expButton action listener");	
				}
			});
		}

		public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
			if (isSelected) {
				expButton.setForeground(table.getSelectionForeground());
				expButton.setBackground(table.getSelectionBackground());
			} else{
				expButton.setForeground(table.getForeground());
				expButton.setBackground(table.getBackground());
			}
			isPushed = true;
			return expButton;
		}
		public Object getCellEditorValue() {
			if (isPushed)  {
				JOptionPane.showMessageDialog(expButton, ">> "  + ": expButton active");
			}
			isPushed = false;
			return new String("expButton not active");
		}
	}

	private class CheckBoxCellEditor extends AbstractCellEditor implements TableCellEditor {
		protected JCheckBox checkBox;
		public CheckBoxCellEditor() {
			checkBox = new JCheckBox();
			checkBox.setHorizontalAlignment(SwingConstants.CENTER);
			checkBox.setBackground( Color.white);
		}

		public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
			checkBox.setSelected(((Boolean) value).booleanValue());
			Component c = table.getDefaultRenderer(String.class).getTableCellRendererComponent(table, value, isSelected, false, row, column);
			if (c != null) {
				checkBox.setBackground(c.getBackground());
			}
			return checkBox;
		}
		public Object getCellEditorValue() {
			return Boolean.valueOf(checkBox.isSelected());
		}
	}

}
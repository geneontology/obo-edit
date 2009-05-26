package org.oboedit.gui.components.ontologyGeneration;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Point;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;

import org.apache.log4j.Logger;

/**
 * A popup showing the different kinds of the definition and its respective URLs.
 * 
 * @author Goetz Fabian
 * @author Marcel Hanke
 */
public class DefinitionsPopup extends JDialog
{

	private static final long serialVersionUID = -9159196480683973991L;
	protected final static Logger logger = Logger.getLogger(DefinitionsPopup.class);

	private OBOOntologyGenerationGUIComponent parent;

	private CandidateDefinition originalDefinition;
	private List<DefPosPair> candidateDefinitionList;
	private Box box;
	private JLabel label;
	private JScrollPane scrollPane;
	private DefinitionPopupTable table;
	private JButton closeButton;
	private int numberOfCallsToSummarizeDefinition;

	public DefinitionsPopup(final OBOOntologyGenerationGUIComponent parent)
	{
		this.parent = parent;
		this.setModal(true);
		this.setResizable(false);
	}

	public void initPopup(CandidateDefinition definition)
	{
		this.originalDefinition = definition;

		candidateDefinitionList = buildCandidateDefinitionList(definition);

		/*
		 * Construct objects
		 */
		Container contentPane = getContentPane();
		box = new Box(BoxLayout.Y_AXIS);
		label = new JLabel("Available Definitions:");
		table = new DefinitionPopupTable();
		scrollPane = new JScrollPane(table);
		closeButton = new JButton("Close");

		/*
		 * Adjust objects
		 */
		closeButton.addMouseListener(new MouseAdapter()
		{
			@Override
			public void mouseClicked(MouseEvent e)
			{
				setVisible(false);
				parent.repaint();
			}
		});

		label.setAlignmentX(Component.LEFT_ALIGNMENT);
		scrollPane.setAlignmentX(Component.LEFT_ALIGNMENT);
		closeButton.setAlignmentX(Component.LEFT_ALIGNMENT);

		contentPane.add(box);

		box.add(label);
		box.add(scrollPane);
		box.add(closeButton);

		box.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

		setSize(500, 400);

		// center
		Point parentAnchor = parent.getLocation();
		SwingUtilities.convertPointToScreen(parentAnchor, parent);
		int x = parentAnchor.x + (parent.getWidth() - this.getWidth()) / 2;
		int y = parentAnchor.y + (parent.getHeight() - this.getHeight()) / 2;
		setLocation(x, y);

		/*
		 * Add an listener capturing the done() in the worker
		 */
		getTable().addPropertyChangeListener(new PropertyChangeListener()
		{

			public void propertyChange(PropertyChangeEvent evt)
			{
				if (evt.getPropertyName().equals(DefinitionExtensionWorker.WAS_EXTENDED_PROPERTY)) {
					summarizeDefinitions();
				}
			}
		});

		/*
		 * Try to extend the existing definitions.
		 */
		for (DefPosPair defPosPair : candidateDefinitionList) {
			if (defPosPair.isDef()) {
				DefinitionExtensionWorker extensionWorker = new DefinitionExtensionWorker(defPosPair.def, getTable());
				extensionWorker.execute();
			}
		}
		numberOfCallsToSummarizeDefinition = 0;
	}

	private List<DefPosPair> buildCandidateDefinitionList(CandidateDefinition def)
	{

		List<DefPosPair> candDefList = new ArrayList<DefPosPair>();
		/*
		 * Fill the List: The first entry (position=0) is always for the definition itself (html-formatted). The rest of
		 * the entries (position=1 ... position=def.getUrl().size()) are the URLs.
		 * 
		 * In the first loop, the original definition is inserted, in the second loop the alternative definitions are
		 * inserted.
		 */
		for (int i = 0; i < (def.getUrl().size() + 1); i++) {
			candDefList.add(new DefPosPair(def, i));
		}
		if (def.getAlternativeDefinitions() != null) {
			for (CandidateDefinition alternativeDef : def.getAlternativeDefinitions()) {
				for (int i = 0; i < (alternativeDef.getUrl().size() + 1); i++) {
					candDefList.add(new DefPosPair(alternativeDef, i));
				}
			}
		}

		return candDefList;
	}

	public DefinitionPopupTable getTable()
	{
		return table;
	}

	/**
	 * Tries to summarize the existing definitions after extension as good as possible.
	 */
	private void summarizeDefinitions()
	{
		boolean updateTable = false;

		numberOfCallsToSummarizeDefinition++;
		if (originalDefinition.getAlternativeDefinitions() == null) {
			return;
		}
		if (numberOfCallsToSummarizeDefinition < originalDefinition.getAlternativeDefinitions().size() + 1) {
			return;
		}
		for (int i = 0; i < originalDefinition.getAlternativeDefinitions().size(); i++) {
			CandidateDefinition def = originalDefinition.getAlternativeDefinitions().get(i);
			if (def.getDefinition().equals(originalDefinition.getDefinition())) {
				for (String url : def.getUrl()) {
					boolean skipURL = false;
					for (String alreadyExistingURL : originalDefinition.getUrl()) {
						if (url.equals(alreadyExistingURL)) {
							skipURL = true;
						}
					}
					if (!skipURL) {
						// add URL
						originalDefinition.addURL(url);
						// add the corresponding cachedURL
						originalDefinition.addCachedURL(def.getCachedURL().get(def.getUrl().indexOf(url)));
						// remove alternative definition
					}
				}
				originalDefinition.getAlternativeDefinitions().remove(def);

				updateTable = true;
			}
			else {
				for (int j = originalDefinition.getAlternativeDefinitions().indexOf(def) + 1; j < originalDefinition
				    .getAlternativeDefinitions().size(); j++) {
					CandidateDefinition altDef = originalDefinition.getAlternativeDefinitions().get(j);
					if (def.getDefinition().equals(altDef.getDefinition())) {
						for (String url : altDef.getUrl()) {
							boolean skipURL = false;
							for (String alreadyExistingURL : def.getUrl()) {
								if (url.equals(alreadyExistingURL)) {
									skipURL = true;
								}
							}
							if (!skipURL) {
								// add URL
								def.addURL(url);
								// add the corresponding cachedURL
								def.addCachedURL(def.getCachedURL().get(def.getUrl().indexOf(url)));
								// remove alternative definition
							}
						}
						originalDefinition.getAlternativeDefinitions().remove(altDef);

						updateTable = true;
					}
				}
			}
		}

		if (updateTable) {
			candidateDefinitionList = buildCandidateDefinitionList(originalDefinition);

			// reset the row heights etc.
			getTable().initPopupTable();

			getTable().repaint();
		}

	}

	private class DefinitionPopupTable extends JTable
	{
		private static final long serialVersionUID = 108519703957872291L;

		public DefinitionPopupTable()
		{
			super(new DefinitionPopupTableModel());

			this.setGridColor(Color.LIGHT_GRAY);
			this.setRowHeight(this.getRowHeight() + 4);
			initPopupTable();
		}

		public void initPopupTable()
		{
			getColumnModel().getColumn(0).setMaxWidth(50);
			getColumnModel().getColumn(0).setResizable(false);
			getColumnModel().getColumn(0).setCellEditor(new ButtonEditor(new JCheckBox()));
			getColumnModel().getColumn(2).setMaxWidth(30);
			this.tableHeader.setReorderingAllowed(false);

			if (this.getMouseListeners().length > 0)
				this.removeMouseListener(this.getMouseListeners()[0]);

			this.addMouseListener(new MouseAdapter()
			{
				@Override
				public void mouseClicked(MouseEvent e)
				{
					if (!SwingUtilities.isLeftMouseButton(e)) {
						return;
					}
					Point p = e.getPoint();
					int rowIndex = rowAtPoint(p);
					int columnIndex = columnAtPoint(p);
					// The autoscroller can generate drag events outside the Table's
					// range.
					if ((columnIndex == -1) || (rowIndex == -1)) {
						return;
					}
					// CheckBox clicked
					else if (columnIndex == 0 && candidateDefinitionList.get(rowIndex).isDef()) {
						onClickAddDefinition(rowIndex, columnIndex);
					}
					// Only open web page on clicks on the (I)-Button
					else if (columnIndex == 2 && candidateDefinitionList.get(rowIndex).isURL()) {
						onClickOpenExternalDefinitionWebPage(rowIndex);
					}
				}
			});

		}

		@Override
		public DefinitionPopupTableModel getModel()
		{
			return (DefinitionPopupTableModel) super.getModel();
		}

		@Override
		public TableCellRenderer getCellRenderer(int row, int column)
		{
			/*
			 * Select the appropriate table cell renderer: If row contains a URL (position > 0), put (I)-Button in last
			 * column, Otherwise (row contains definition), use standard cell renderer.
			 */
			if (column == 2 && candidateDefinitionList.get(row).isURL()) {
				return new TableCellImageRenderer("resources/aboutIcon.png");
			}
			else if (column == 0) {
				if (candidateDefinitionList.get(row).isDef()) {
					return new ButtonRenderer();
				}
				else 
				return new DefaultTableCellRenderer()
				{
					private static final long serialVersionUID = -4293679914935943300L;

					public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
					    boolean hasFocus, int row, int column)
					{
						JLabel comp = (JLabel) super.getTableCellRendererComponent(table, value, isSelected,
						    hasFocus, row, column);
						comp.setText(null);
						return comp;
					}
				};
			}
			else if (column == 1) {
				return new DefaultTableCellRenderer()
				{
					private static final long serialVersionUID = 1L;
					
					private static final int MAX_LINE_LENGTH = 40;
					
					public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
						    boolean hasFocus, int row, int column)
						{
							// Check if the row belongs to a definition
							if (candidateDefinitionList.get(row).isDef()) {
								JLabel comp = (JLabel) super.getTableCellRendererComponent(table, value, isSelected,
								    hasFocus, row, column);
								comp.setText((String)getModel().getValueAt(row, column));
								
								// add multi-line tooltip displaying the full HTML-formatted definition.
								String htmlDef = (String)getModel().getDefinitionForRow(row).def.getDefinitionHTMLFormatted();
								
								String toolTipText = "";
								
								int lineCount = 0;
								// counts the number of characters outside HTML tags
								int numberOfCharsRead = 0;
								boolean openedTag = false;
								for (int i = 0; i < htmlDef.length(); i++) {
									if (!openedTag) {
										if (htmlDef.charAt(i) == '<') {
											openedTag = true;
										} 
										else {
											numberOfCharsRead++;
										}
									} else {
										if (htmlDef.charAt(i) == '>') {
											openedTag = false;
										}
									}
									// if the maximum line length is reached, start a new line by appending "<br/>"
									if ((numberOfCharsRead % MAX_LINE_LENGTH) == 0 && !openedTag && htmlDef.charAt(i) != '>') {
										if (numberOfCharsRead > 0) {
											int nextSpace = htmlDef.indexOf(" ", i);
											if (nextSpace != -1) {
												toolTipText += htmlDef.substring(i, nextSpace);
												i = nextSpace;
												toolTipText += "<br/>";
											}
											else {
												toolTipText += htmlDef.substring(i);
											}
											
										}
										lineCount++;
									}
									
									toolTipText += htmlDef.charAt(i);
								}
								
								comp.setToolTipText(toolTipText);
								
								return comp;
							}
							return table.getDefaultRenderer(table.getColumnClass(column)).getTableCellRendererComponent(
							    table, value, isSelected, hasFocus, row, column);
						}
				};
			}
			else {
				return super.getCellRenderer(row, column);
			}
		}

		/*
		 * CLICKLISTENER
		 */

		/**
		 * Launches web browser to show the webpage definition is fetched from
		 */
		@SuppressWarnings("unchecked")
		private void onClickOpenExternalDefinitionWebPage(int rowIndex)
		{
			String errMsg = "Error attempting to launch web browser";
			String osName = System.getProperty("os.name");
			DefPosPair defPosPair = getModel().getDefinitionForRow(rowIndex);
			String url = defPosPair.def.getCachedURL().get(defPosPair.pos - 1);
			try {
				if (osName.startsWith("Mac OS")) {
					Class fileMgr = Class.forName("com.apple.eio.FileManager");
					Method openURL = fileMgr.getDeclaredMethod("openURL", new Class[] { String.class });
					openURL.invoke(null, new Object[] { url });
				}
				else if (osName.startsWith("Windows")) {
					Runtime.getRuntime().exec("rundll32 url.dll,FileProtocolHandler " + url);
				}
				else { // assume Unix or Linux
					String[] browsers = { "firefox", "opera", "konqueror", "epiphany", "mozilla", "netscape" };
					String browser = null;
					for (int count = 0; count < browsers.length && browser == null; count++)
						if (Runtime.getRuntime().exec(new String[] { "which", browsers[count] }).waitFor() == 0)
							browser = browsers[count];
					if (browser == null)
						throw new Exception("Could not find web browser");
					else
						Runtime.getRuntime().exec(new String[] { browser, url });
				}
			}
			catch (Exception exception) {
				JOptionPane.showMessageDialog(null, errMsg + ":\n" + exception.getLocalizedMessage());
			}
		}

		/**
		 * Adds the definition in the given row to the editDefArea in the main plugin component.
		 * 
		 * @param rowIndex
		 * @param columnIndex
		 */
		private void onClickAddDefinition(int rowIndex, int columnIndex)
		{
			String definition = candidateDefinitionList.get(rowIndex).def.getDefinition();
				parent.updateEditDefArea(definition);
		}

	}

	private class DefinitionPopupTableModel extends AbstractTableModel
	{
		private static final long serialVersionUID = 7816649265967843550L;

		private static final int MAX_SHORTDEF_LENGTH = 145;
		
		public int getColumnCount()
		{
			return 3;
		}
		
		public int getRowCount()
		{
			return candidateDefinitionList.size();
		}
		
		public Object getValueAt(int rowIndex, int columnIndex)
		{
			DefPosPair defPosPair = candidateDefinitionList.get(rowIndex);
			if (columnIndex == 1) {
				// Row contains definition
				if (defPosPair.isDef()) {
					String fullDef = defPosPair.def.getDefinitionHTMLFormatted();
					if (fullDef.length() >= MAX_SHORTDEF_LENGTH) {
						String shortDef = fullDef.substring(0, MAX_SHORTDEF_LENGTH);
						int lastSpace = shortDef.lastIndexOf(" ");
						return shortDef.substring(0, lastSpace) + " ...";
					} 
					else {
						return fullDef;
					}
				}
				// row contains URL
				else {
					return defPosPair.def.getUrl().get(defPosPair.pos - 1);
				}
			}
			return null;
		}

		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex)
		{
			// Only rows displaying a definition are editable.
			return (columnIndex == 0 && candidateDefinitionList.get(rowIndex).isDef());
		}

		@Override
		public String getColumnName(int columnIndex)
		{
			return "";
		}

		@Override
		public Class<?> getColumnClass(int columnIndex)
		{

			if (columnIndex == 0) {
				return JButton.class;
			}
			else {
				return super.getColumnClass(columnIndex);
			}

		}

		public DefPosPair getDefinitionForRow(int rowIndex)
		{
			return candidateDefinitionList.get(rowIndex);
		}
	}	

	/**
	 * DefPosPair describes a pair of a definition and the position of the needed (cached-)URL in the definition
	 * (cached-)URL list. This is needed for finding the appropriate cachedURL for the displayed URL in the
	 * DefinitionPopupTable.
	 * 
	 * @author Goetz Fabian
	 */
	private class DefPosPair
	{
		public int pos;
		public CandidateDefinition def;

		public DefPosPair(CandidateDefinition def, int position)
		{
			this.def = def;
			this.pos = position;
		}

		public boolean isDef()
		{
			return (pos == 0);
		}

		public boolean isURL()
		{
			return (pos > 0);
		}
	}
}

package org.bbop.dataadapter;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.io.*;

import org.bbop.io.IOUtil;
import org.bbop.util.*;

import org.apache.log4j.*;

public class FileAdapterUI extends AbstractGraphicalUI implements ParameterUI {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FileAdapterUI.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 6457202902580260109L;

	protected JComboBox readField = new JComboBox();

	protected JComboBox writeField = new JComboBox();

	protected JButton readBrowseButton = new JButton("Browse...");

	protected JButton writeBrowseButton = new JButton("Browse...");

	protected JLabel readLabel = new JLabel("Load file");

	protected JLabel writeLabel = new JLabel("Save file");

	protected JPanel readPanel = new JPanel();

	protected JPanel writePanel = new JPanel();

	protected boolean multiSelectEnabled = true;

	protected IOOperation<?,?> readOperation;

	protected IOOperation<?,?> writeOperation;

	protected static final int MAX_HISTORY_SIZE = 5;

	public static class FileUIConfig implements UIConfiguration {
		protected java.util.List readHistory = new Vector();

		protected java.util.List writeHistory = new Vector();

		public void setReadHistory(java.util.List readHistory) {
			this.readHistory = readHistory;
		}

		public void setWriteHistory(java.util.List writeHistory) {
			this.writeHistory = writeHistory;
		}

		public java.util.List getReadHistory() {
			return readHistory;
		}

		public java.util.List getWriteHistory() {
			return writeHistory;
		}
	}

	protected boolean isReadOperation(IOOperation op) {
		return op.equals(readOperation);
	}

	protected boolean isWriteOperation(IOOperation op) {
		return op.equals(writeOperation);
	}

	public void setReadOperation(IOOperation op) {
		this.readOperation = op;
	}

	public void setWriteOperation(IOOperation op) {
		this.writeOperation = op;
	}

	protected JPanel buildFieldPanel(JLabel label, JComponent comp,
			JButton browseButton, int padding) {
		Box labelBox = new Box(BoxLayout.X_AXIS);
		labelBox.add(label);
		labelBox.add(Box.createHorizontalStrut(padding));

		Box browseBox = new Box(BoxLayout.X_AXIS);
		browseBox.add(Box.createHorizontalStrut(padding));
		browseBox.add(browseButton);

		JPanel out = new JPanel();
		out.setOpaque(false);
		out.setLayout(new BorderLayout());
		out.add(labelBox, "West");
		out.add(comp, "Center");
		out.add(browseBox, "East");
		return out;
	}

	public void setButtonColor(Color background, Color foreground) {
		readBrowseButton.setBackground(background);
		readBrowseButton.setForeground(foreground);

		writeBrowseButton.setBackground(background);
		writeBrowseButton.setForeground(foreground);

		readField.setBackground(background);
		writeField.setBackground(background);
	}

	@Override
	public void setFont(Font font) {
		setLabelFont(font);
		setButtonFont(font);
	}

	public void setLabelFont(Font labelFont) {
		if (readLabel != null)
			readLabel.setFont(labelFont);
		if (writeLabel != null)
			writeLabel.setFont(labelFont);
	}

	public void setButtonFont(Font buttonFont) {
		if (readBrowseButton != null)
			readBrowseButton.setFont(buttonFont);
		if (writeBrowseButton != null)
			writeBrowseButton.setFont(buttonFont);
		if (readField != null)
			readField.setFont(buttonFont);
		if (writeField != null)
			writeField.setFont(buttonFont);
	}

	public FileAdapterUI() {
		setOpaque(false);
		setMultiSelectEnabled(true);
		readField.setEditable(true);
		writeField.setEditable(true);
		readPanel = buildFieldPanel(readLabel, readField, readBrowseButton, 10);
		writePanel = buildFieldPanel(writeLabel, writeField, writeBrowseButton,
				10);

		uiConfig = new FileUIConfig();

		readBrowseButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
			    // Start FileChooser ("Browse...") at most recently selected directory
			    String currentPath = (String) readField.getSelectedItem();
				JFileChooser chooser = new JFileChooser(currentPath);
				chooser.setMultiSelectionEnabled(multiSelectEnabled);

				int returnVal = chooser.showOpenDialog(FileAdapterUI.this);
				if (returnVal == JFileChooser.APPROVE_OPTION) {
					File[] selected = chooser.getSelectedFiles();
					String paths = "";
					for (int i = 0; i < selected.length; i++) {
						if (i > 0)
							paths += " ";
						if (selected.length != 1) {
							paths += "\""
									+ escapePath(selected[i].getAbsolutePath(),
											true) + "\"";
						} else {
							paths += escapePath(selected[i].getAbsolutePath(),
									false);
						}
					}
					readField.setSelectedItem(paths);
				}
			}
		});
		writeBrowseButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
			    // Start FileChooser ("Browse...") at most recently selected directory
			    String currentPath = (String) writeField.getSelectedItem();
			    JFileChooser chooser = new JFileChooser(currentPath);

				int returnVal = chooser.showSaveDialog(FileAdapterUI.this);
				if (returnVal == JFileChooser.APPROVE_OPTION) {
					writeField.setSelectedItem(chooser.getSelectedFile()
							.getAbsolutePath());
				}
			}
		});
	}

	public void setMultiSelectEnabled(boolean multiSelectEnabled) {
		this.multiSelectEnabled = multiSelectEnabled;
		readLabel.setText(multiSelectEnabled ? "Load files" : "Load file");
	}

	@Override
	public void setUIConfiguration(UIConfiguration uiConfig) {
		super.setUIConfiguration(uiConfig);
		if (uiConfig == null)
			return;

		readField.removeAllItems();
		FileUIConfig config = (FileUIConfig) uiConfig;
		java.util.List readHistory = config.getReadHistory();
		Iterator it = readHistory.iterator();
		while (it.hasNext()) {
			String s = (String) it.next();
			readField.addItem(s);
		}
		if (readHistory.size() > 0)
			readField.setSelectedIndex(0);

		writeField.removeAllItems();
		java.util.List writeHistory = config.getWriteHistory();
		it = writeHistory.iterator();
		while (it.hasNext()) {
			String s = (String) it.next();
			writeField.addItem(s);
		}
		if (writeHistory.size() > 0)
			writeField.setSelectedIndex(0);
	}

	@Override
	public UIConfiguration getUIConfiguration() {
		FileUIConfig config = (FileUIConfig) uiConfig;

		if (config == null) {
			config = new FileUIConfig();
		}

		Vector readHistory = new Vector();
		Vector writeHistory = new Vector();

		readHistory.addAll(config.getReadHistory());
		if (readField.getSelectedItem() != null) {
			String fieldContents = ((String) readField.getSelectedItem())
					.trim();

			if (fieldContents.length() > 0) {
				readHistory.remove(fieldContents);
				readHistory.add(0, fieldContents);
			}
		}
		if (readHistory.size() > MAX_HISTORY_SIZE)
			readHistory.setSize(MAX_HISTORY_SIZE);
		config.setReadHistory(readHistory);

		writeHistory.addAll(config.getWriteHistory());
		if (writeField.getSelectedItem() != null) {
			String fieldContents = ((String) writeField.getSelectedItem())
					.trim();
			if (fieldContents.length() > 0) {
				writeHistory.remove(fieldContents);
				writeHistory.add(0, fieldContents);
			}
		}
		if (writeHistory.size() > MAX_HISTORY_SIZE)
			writeHistory.setSize(MAX_HISTORY_SIZE);
		config.setWriteHistory(writeHistory);

		return super.getUIConfiguration();
	}

	@Override
	public void setConfiguration(AdapterConfiguration c) {
		if (!(c instanceof FileAdapterConfiguration))
			throw new IllegalArgumentException("File adapter UIs can only "
					+ "accept file adapter " + "configuration files");
		super.setConfiguration(c);
		FileAdapterConfiguration config = (FileAdapterConfiguration) c;

		if (config.getReadPaths() != null && config.getReadPaths().size() > 0) {
			String readPath = "";
			Iterator it = config.getReadPaths().iterator();
			for (int i = 0; it.hasNext(); i++) {
				String s = (String) it.next();
				if (i > 0)
					readPath += " ";
				readPath += escapePath(s, false);
			}
			readField.setSelectedItem(readPath);
		}

		if (config.getWritePath() != null)
			writeField.setSelectedItem((config).getWritePath());
	}

	protected String escapePath(String path, boolean quoted) {
		StringBuffer out = new StringBuffer();
		for (int i = 0; i < path.length(); i++) {
			if (path.charAt(i) == '"')
				out.append("\\\"");
			else if (i + 1 < path.length() && path.charAt(i) == '\\'
					&& path.charAt(i + 1) == '"')
				out.append("\\\\");
			else if (path.charAt(i) == ' ' && !quoted) {
				out.append("\\ ");
			} else
				out.append(path.charAt(i));
		}
		return out.toString();
	}

	protected Collection getPaths() {
		Set out = new HashSet();
		String s = (String) readField.getSelectedItem();
		if (s == null)
			return out;

		s = s.trim();

		if (multiSelectEnabled) {
			boolean quotes = false;
			boolean inString = false;
			StringBuffer currentStr = new StringBuffer();
			for (int i = 0; i < s.length(); i++) {
				char c = s.charAt(i);
				if (Character.isWhitespace(c)) {
					if (inString) {
						if (!quotes) {
							out.add(currentStr.toString());
							inString = false;
							currentStr = new StringBuffer();
						} else
							currentStr.append(c);
					} else {
						inString = false;
						quotes = false;
					}
				} else if (c == '"') {
					if (inString && quotes) {
						quotes = false;
						inString = false;
						out.add(currentStr.toString());
						currentStr = new StringBuffer();
					} else if (inString && !quotes) {
						out.add(currentStr.toString());
						currentStr = new StringBuffer();
						quotes = true;
					} else {
						inString = true;
						quotes = true;
					}
				} else if (c == '\\') {
					if (s.length() > i + 1) {
						if (s.charAt(i + 1) == '"'
								|| (!quotes && s.charAt(i + 1) == ' '))
							c = s.charAt(++i);
					}
					currentStr.append(c);
					inString = true;
				} else {
					currentStr.append(c);
					inString = true;
				}
			}
			if (inString)
				out.add(currentStr.toString());
		} else {
			out.add(s);
		}
		return out;
	}

	public AdapterConfiguration createEmptyConfig() {
		return new FileAdapterConfiguration();
	}

	@Override
	public void init(AdapterWidgetI widget, IOOperation op,
			DataAdapter adapter, Object input) {
		super.init(widget, op, adapter, input);
		setLayout(new BorderLayout());
		removeAll();
		if (isReadOperation(op)) {
			add(readPanel, "North");
		} else if (isWriteOperation(op)) {
			add(writePanel, "North");
		}
		validate();
		repaint();
	}

	public void acceptComponentConfig(boolean storeonly)
			throws DataAdapterUIException {
		FileAdapterConfiguration config = (FileAdapterConfiguration) this.config;

		config.setReadPaths(getPaths());
		config.setWritePath((String) writeField.getSelectedItem());

		if (isReadOperation(op)) {
			Iterator it = config.getReadPaths().iterator();
			while (it.hasNext()) {
				String path = (String) it.next();
				if (!storeonly && !IOUtil.isURL(path)
						&& !(new File(path)).exists())
					throw new DataAdapterUIException("File " + path
							+ " does not exist.");
			}
		} else if (isWriteOperation(op)) {
			String writePath = config.getWritePath();

			if (!storeonly && writePath != null
					&& (new File(writePath)).exists()) {
				int val = JOptionPane.showConfirmDialog(this, "File "
						+ writePath + " exists. " + "Overwrite?",
						"Overwrite file?", JOptionPane.YES_NO_OPTION);
				if (val != JOptionPane.YES_OPTION)
					throw new DataAdapterUIException();
			}
		}
	}

	public TagSpec getParameterSpec() {
		TagSpec out = new TagSpec();
		TagSpec fSpec = new TagSpec("-f");
		fSpec.addArgumentSpec(null, -1);

		TagSpec oSpec = new TagSpec("-o");
		oSpec.addArgumentSpec(null, 1);

		out.addArgumentSpec(fSpec, 1);
		out.addArgumentSpec(oSpec, 1);
		out.setImpliedSpec(fSpec, 1);

		return out;
	}

	public void setParameters(Tag optionsTag) throws DataAdapterUIException {

		Iterator it = optionsTag.getArguments().iterator();
		while (it.hasNext()) {
			Tag param = (Tag) it.next();
			processParameter(param);
		}
	}

	protected void processParameter(Tag param) throws DataAdapterUIException {
		FileAdapterConfiguration config = (FileAdapterConfiguration) this.config;

		if (param.getName().equals("-o")) {
			if (!isWriteOperation(op))
				throw new DataAdapterUIException("Cannot set an output "
						+ "path when reading files");
			if (param.getArguments().size() > 0)
				throw new DataAdapterUIException("Cannot set multiple "
						+ "output files");
			config.setWritePath((String) param.getArguments().get(0));
		} else if (param.getName().equals("-f")) {
			config.getReadPaths().addAll(param.getArguments());
		}
		if (!isReadOperation(op)) {
			if (config.getReadPaths().size() > 0)
				throw new DataAdapterUIException("Cannot specify files to "
						+ "load when writing files.");
		} else {
			if (config.getReadPaths().size() < 1)
				throw new DataAdapterUIException("Must specify at least one "
						+ "file to load.");
		}
	}

	@Override
	public void cleanup() {
	}
}

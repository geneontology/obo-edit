package org.bbop.dataadapter;

import javax.swing.*;
import javax.swing.border.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;

import org.bbop.swing.*;
import org.bbop.util.*;
import java.io.*;
import java.beans.*;
import javax.swing.plaf.basic.*;
import java.net.URL;

import org.apache.log4j.*;

public class GraphicalAdapterChooser<IN, OUT> extends JPanel implements
AdapterWidgetI {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GraphicalAdapterChooser.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -1304344582277871901L;

	protected static final Class[] checkClasses = { Component.class,
		GraphicalUI.class };

	protected static final Icon rightArrowIcon = new Icon() {
		protected int height = 16;

		protected int width = 9;

		public void paintIcon(Component c, Graphics g, int x, int y) {
			Graphics2D g2 = (Graphics2D) g;
			Polygon polygon = new Polygon();
			polygon.addPoint(x, y);
			polygon.addPoint(x + width - 1, y + height / 2);
			polygon.addPoint(x, y + height);
			g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
					RenderingHints.VALUE_ANTIALIAS_ON);

			g.setColor(c.getBackground());
			g.fillPolygon(polygon);
			g.setColor(c.getForeground());
			g.drawPolygon(polygon);

		}

		public int getIconWidth() {
			return width;
		}

		public int getIconHeight() {
			return height;
		}
	};

	protected static final Icon downArrowIcon = new Icon() {
		protected int width = 16;

		protected int height = 9;

		public void paintIcon(Component c, Graphics g, int x, int y) {
			Graphics2D g2 = (Graphics2D) g;
			Polygon polygon = new Polygon();
			polygon.addPoint(x, y);
			polygon.addPoint(x + width - 1, y);
			polygon.addPoint(x + width / 2, y + height);
			g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
					RenderingHints.VALUE_ANTIALIAS_ON);

			g.setColor(c.getBackground());
			g.fillPolygon(polygon);
			g.setColor(c.getForeground());
			g.drawPolygon(polygon);

		}

		public int getIconWidth() {
			return width;
		}

		public int getIconHeight() {
			return height;
		}
	};

	protected final static URL rightArrowIconURL = ClassLoader
	.getSystemClassLoader().getResource(
			"org/geneontology/resources/blue_arrow.gif");

	protected final static URL downArrowIconURL = ClassLoader
	.getSystemClassLoader().getResource(
			"org/geneontology/resources/blue_arrow_down.gif");

	// protected static final Icon rightArrowIcon = new
	// ImageIcon(rightArrowIconURL);
	// protected static final Icon downArrowIcon = new
	// ImageIcon(downArrowIconURL);

	protected DataAdapterRegistry registry;

	protected JLabel profileChooserLabel = new JLabel("Stored adapter settings");

	protected JLabel adapterChooserLabel = new JLabel("Data adapter");

	protected JPanel uiPanel = new JPanel();

	protected JPanel profileButtonPanel = new JPanel();

	protected JButton showProfilesButton = new JButton(rightArrowIcon);

	protected JButton showAdvancedButton = new JButton("Advanced...");

	protected JButton showBasicButton = new JButton("Basic...");

	protected JComboBox adapterList = new JComboBox();

	protected JComboBox profileListChooser = new JComboBox();

	protected boolean profileListChooserVisible = false;

	protected GraphicalUI currentUI;

	protected DataAdapter currentAdapter;

	protected IOOperation<IN, OUT> op;

	protected IN input;

	protected String historyPath;

	protected AdapterChooserConfig chooserConfig = new AdapterChooserConfig();

	protected NamedAdapterConfig currentConfig;

	protected TitledBorder uiPanelBorder = new TitledBorder("Configuration");

	protected ProfileCollection profileList;

	protected boolean throwExceptions = false;

	protected boolean committed = false;

	protected JButton okButton = new JButton("  Ok  ");

	protected JButton cancelButton = new JButton("Cancel");

	protected JButton exceptionCancelButton = new JButton("Cancel");

	protected JButton tryAgainButton = new JButton("Try again");

	protected JButton plusButton = new JButton(new PlusIcon(1.5f, 8, 8));

	protected JButton minusButton = new JButton(new MinusIcon(1.5f, 8, 8));

	JDialog dialog;

	protected JPanel profileListChooserPanel = new JPanel();

	protected JPanel northPanel = new JPanel();

	protected JPanel okButtonPanel = new JPanel();

	protected JPanel exceptionButtonPanel = new JPanel();

	protected Color backgroundColor;

	protected Font labelFont;

	protected DataAdapterOperationTask<IN, OUT> loadtask;

	protected BackgroundEventQueue queue;

	protected JEditorPane exceptionEditorPane = new JEditorPane("text/html",
	"<html></html>");

	protected JScrollPane exceptionPanel = new JScrollPane(exceptionEditorPane,
			ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
			ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);

	protected ActionListener profileChooserListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			storeCurrentProfile();
			loadProfile();
		}
	};

	protected class AdapterListRenderer extends DefaultListCellRenderer {
		/**
		 * 
		 */
		private static final long serialVersionUID = 6595990724197125690L;

		@Override
		public Component getListCellRendererComponent(JList list, Object value,
				int index, boolean isSelected, boolean cellHasFocus) {
			if (value instanceof DataAdapter) {
				String name = ((DataAdapter) value).getName();
				return super.getListCellRendererComponent(list, name, index,
						isSelected, cellHasFocus);
			} else
				return super.getListCellRendererComponent(list, value, index,
						isSelected, cellHasFocus);
		}
	}

	protected class NamedConfigEditor extends BasicComboBoxEditor {
		protected Object oldValue;

		@Override
		public void setItem(Object anObject) {
			if (anObject != null && anObject instanceof NamedAdapterConfig) {
				NamedAdapterConfig nc = (NamedAdapterConfig) anObject;
				editor.setText(nc.getName());
				oldValue = anObject;
			} else {
				editor.setText("");
			}
		}

		@Override
		public Object getItem() {
			NamedAdapterConfig nc = (NamedAdapterConfig) oldValue;
			nc.setName(editor.getText());

			return nc;
		}
	}

	public static class ProfileCollection {
		protected Vector profiles;

		protected NamedAdapterConfig defaultConfig;

		protected UIConfiguration uiConfig;

		protected Map operationToAdvancedMap;

		// protected boolean showAdvanced = false;
		/*
		 * public void setShowAdvanced(boolean showAdvanced) { this.showAdvanced =
		 * showAdvanced; }
		 * 
		 * public boolean getShowAdvanced() { return showAdvanced; }
		 */
		public ProfileCollection() {
			profiles = new Vector();
			operationToAdvancedMap = new HashMap();
		}

		public void setOperationToAdvancedMap(Map operationToAdvancedMap) {
			this.operationToAdvancedMap = operationToAdvancedMap;
		}

		public Map getOperationToAdvancedMap() {
			return operationToAdvancedMap;
		}

		public void setUIConfig(UIConfiguration uiConfig) {
			this.uiConfig = uiConfig;
		}

		public UIConfiguration getUIConfig() {
			return uiConfig;
		}

		public void setProfiles(Vector profiles) {
			this.profiles = profiles;
		}

		public Vector getProfiles() {
			return profiles;
		}

		public NamedAdapterConfig getDefaultConfig() {
			return defaultConfig;
		}

		public void setDefaultConfig(NamedAdapterConfig defaultConfig) {
			this.defaultConfig = defaultConfig;
		}
	}

	public static class AdapterChooserConfig {
		protected Map profileMap;

		protected boolean showProfiles = false;

		protected Map operationToLastAdapterMap = new HashMap();

		public void setOperationToLastAdapterMap(Map map) {
			this.operationToLastAdapterMap = map;
		}

		public Map getOperationToLastAdapterMap() {
			return operationToLastAdapterMap;
		}

		public void setShowProfiles(boolean showProfiles) {
			this.showProfiles = showProfiles;
		}

		public boolean getShowProfiles() {
			return showProfiles;
		}

		public AdapterChooserConfig() {
			profileMap = new HashMap();
		}

		public Map getProfileMap() {
			return profileMap;
		}

		public void setProfileMap(Map profileMap) {
			this.profileMap = profileMap;
		}
	}

	public static class NamedAdapterConfig {
		protected String name;

		protected AdapterConfiguration configuration;

		public void setName(String name) {
			this.name = name;
		}

		public String getName() {
			return name;
		}

		public AdapterConfiguration getConfiguration() {
			return configuration;
		}

		public void setConfiguration(AdapterConfiguration configuration) {
			this.configuration = configuration;
		}

		@Override
		public String toString() {
			return getName();
		}

		@Override
		public boolean equals(Object o) {
			if (o instanceof NamedAdapterConfig) {
				return ((NamedAdapterConfig) o).getName().equals(name);
			} else
				return false;
		}
	}

	public void setHistoryPath(String path) {
		try {
			historyPath = path;
			XMLDecoder d = new XMLDecoder(new BufferedInputStream(
					new FileInputStream(path)));
			chooserConfig = (AdapterChooserConfig) d.readObject();
			d.close();
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		if (chooserConfig == null)
			chooserConfig = new AdapterChooserConfig();
		profileList = null;
		currentConfig = null;
		setProfileListVisible(chooserConfig.getShowProfiles());
		initProfiles();
		String currentAdapterID = (String) chooserConfig
		.getOperationToLastAdapterMap().get(op.getID());

		if (currentAdapterID != null) {
			for (int i = 0; i < adapterList.getItemCount(); i++) {
				DataAdapter adapter = (DataAdapter) adapterList.getItemAt(i);
				if (adapter.getID().equals(currentAdapterID)) {
					adapterList.setSelectedIndex(i);
					break;
				}
			}
		}

		if (profileList != null) {
			Boolean b = (Boolean) profileList.getOperationToAdvancedMap().get(
					op);
			setShowAdvanced((b == null ? false : b.booleanValue()));
		}
	}

	public void flushConfiguration() {
		if (historyPath == null)
			return;
		try {
			XMLEncoder e = new XMLEncoder(new BufferedOutputStream(
					new FileOutputStream(historyPath)));
			e.writeObject(chooserConfig);
			e.close();
		} catch (Exception ex) {
			ex.printStackTrace();
		}

	}

	public void setThrowException(boolean throwExceptions) {
		this.throwExceptions = throwExceptions;
	}

	@Override
	public void setFont(Font font) {
		super.setFont(font);
		setButtonFont(font);
		setLabelFont(font);
	}

	public void setButtonFont(Font buttonFont) {
		if (okButton != null)
			okButton.setFont(buttonFont);
		if (cancelButton != null)
			cancelButton.setFont(buttonFont);
		if (exceptionCancelButton != null)
			exceptionCancelButton.setFont(buttonFont);
		if (adapterList != null)
			adapterList.setFont(buttonFont);
		if (profileListChooser != null)
			profileListChooser.setFont(buttonFont);
		if (tryAgainButton != null)
			tryAgainButton.setFont(buttonFont);
		if (showAdvancedButton != null)
			showAdvancedButton.setFont(buttonFont);
		if (showBasicButton != null)
			showBasicButton.setFont(buttonFont);
	}

	public void setLabelFont(Font labelFont) {
		if (adapterChooserLabel != null)
			adapterChooserLabel.setFont(labelFont);
		if (profileChooserLabel != null)
			profileChooserLabel.setFont(labelFont);
		if (uiPanelBorder != null)
			uiPanelBorder.setTitleFont(labelFont);
	}

	public void setButtonColor(Color background, Color foreground) {
		adapterList.setBackground(background);
		adapterList.setForeground(foreground);

		plusButton.setBackground(background);
		plusButton.setForeground(foreground);

		minusButton.setBackground(background);
		minusButton.setForeground(foreground);

		profileListChooser.setBackground(background);
		profileListChooser.setForeground(foreground);

		okButton.setBackground(background);
		okButton.setForeground(foreground);

		cancelButton.setBackground(background);
		cancelButton.setForeground(foreground);

		exceptionCancelButton.setBackground(background);
		exceptionCancelButton.setForeground(foreground);

		tryAgainButton.setBackground(background);
		tryAgainButton.setForeground(foreground);

		showAdvancedButton.setBackground(background);
		showAdvancedButton.setForeground(foreground);

		showBasicButton.setBackground(background);
		showBasicButton.setForeground(foreground);

		showProfilesButton.setBackground(background);
		showProfilesButton.setForeground(foreground);
	}

	protected NamedAdapterConfig createNamedConfig() {
		NamedAdapterConfig nc = new NamedAdapterConfig();
		nc.setName("<create new profile>");
		nc.setConfiguration(currentUI.createEmptyConfig());
		return nc;
	}

	public void setMargins(int top, int left, int bottom, int right) {
		Border margins = new EmptyBorder(10, 10, 10, 10);
		setBorder(margins);
	}

	protected ActionListener adapterListListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			setCurrentAdapter((DataAdapter) adapterList.getSelectedItem());

		}
	};

	public void deleteCurrentConfig() {
		profileListChooser.removeActionListener(profileChooserListener);
		int index = profileListChooser.getSelectedIndex();
		NamedAdapterConfig nextConfig;
		if (index >= profileListChooser.getItemCount() - 1) {
			if (index > 1)
				nextConfig = (NamedAdapterConfig) profileListChooser
				.getItemAt(index - 1);
			else
				nextConfig = null;
		} else
			nextConfig = (NamedAdapterConfig) profileListChooser
			.getItemAt(index + 1);
		profileList.getProfiles().remove(currentConfig);
		if (profileList.getDefaultConfig().equals(currentConfig)) {
			profileList.setDefaultConfig(currentConfig);
		}
		storeProfileList();
		initProfiles();
		profileListChooser.addActionListener(profileChooserListener);
		if (nextConfig == null)
			profileListChooser.setSelectedIndex(0);
		else
			profileListChooser.setSelectedItem(nextConfig);
	}

	public void addNewNamedConfig() {
		if (currentConfig != null) {
			try {
				currentUI.acceptComponentConfig(true);
				currentConfig.setConfiguration(currentUI.getConfig(op,
						currentAdapter, input));
			} catch (Exception ex) {
			}
		}
		NamedAdapterConfig nac = createNamedConfig();
		nac.setName("<new profile>");
		profileListChooser.removeActionListener(profileChooserListener);
		profileListChooser.insertItemAt(nac, 1);
		profileListChooser.addActionListener(profileChooserListener);
		profileListChooser.setSelectedIndex(1);
	}

	protected void storeCurrentProfile() {
		if (currentConfig != null) {
			try {
				currentUI.acceptComponentConfig(true);
				currentConfig.setConfiguration(currentUI.getConfig(op,
						currentAdapter, input));

				if (profileListChooser.getSelectedIndex() == 0
						&& !((NamedAdapterConfig) profileListChooser
								.getSelectedItem()).getName().equals(
								"<create new profile>")) {
					profileListChooser.insertItemAt(createNamedConfig(), 0);
				}

			} catch (Exception ex) {
				logger.info("GraphicalAdapterChooser.storeCurrentProfile: did not store current config!"); // DEL
				ex.getMessage(); // DEL
			}
		}
	}

	protected void storeProfileList() {
		if (profileList != null) {
			if (profileListChooser.getSelectedIndex() != 0)
				profileList.setDefaultConfig(currentConfig);
			else
				profileList.setDefaultConfig(null);

			profileList.getProfiles().remove(0);
			chooserConfig.getProfileMap().put(currentAdapter.getID(),
					profileList);
			profileList.setUIConfig(currentUI.getUIConfiguration());
		}
	}

	protected void initProfiles() {
		profileList = new ProfileCollection();
		currentConfig = null;

		profileList.getProfiles().add(createNamedConfig());
		ProfileCollection pc = (ProfileCollection) chooserConfig
		.getProfileMap().get(currentAdapter.getID());
		UIConfiguration uiConfig = null;
		if (pc != null) {
			profileList.setOperationToAdvancedMap(new HashMap(pc
					.getOperationToAdvancedMap()));
			Vector record = pc.getProfiles();

			if (record != null)
				profileList.getProfiles().addAll(record);
			uiConfig = pc.getUIConfig();
			currentUI.setUIConfiguration(uiConfig);
		}

		profileListChooser.removeActionListener(profileChooserListener);
		profileListChooser.setModel(new DefaultComboBoxModel(profileList
				.getProfiles()));
		profileListChooser.addActionListener(profileChooserListener);

		if (pc != null && pc.getDefaultConfig() != null) {
			profileListChooser.setSelectedItem(pc.getDefaultConfig());
		} else
			profileListChooser.setSelectedIndex(0);

		Boolean b = (Boolean) profileList.getOperationToAdvancedMap().get(op);
		setShowAdvanced((b == null ? false : b.booleanValue()));
		// setShowAdvanced(profileList.getShowAdvanced());
	}

	public void setCurrentAdapter(DataAdapter adapter) {
		storeProfileList();

		this.currentAdapter = adapter;
		uiPanelBorder.setTitle(adapter.getName() + " Configuration");
		DataAdapterUI ui = DataAdapterUtil.getUI(adapter, registry,
				checkClasses);
		setAdapterUI(ui);
		initProfiles();

	}

	public void setProfileListVisible(boolean profileListChooserVisible) {
		chooserConfig.setShowProfiles(profileListChooserVisible);
		// this.profileListChooserVisible = profileListChooserVisible;
		northPanel.remove(profileListChooserPanel);

		if (chooserConfig.getShowProfiles()) {
			northPanel.add(profileListChooserPanel);
			showProfilesButton.setIcon(downArrowIcon);
			showProfilesButton.setToolTipText("Hide stored adapter "
					+ "configurations");
		} else {
			showProfilesButton.setToolTipText("Show stored adapter "
					+ "configurations");
			showProfilesButton.setIcon(rightArrowIcon);
		}

		validate();
		repaint();

		if (dialog != null)
			dialog.pack();
	}

	protected void loadProfile() {
		this.currentConfig = (NamedAdapterConfig) profileListChooser
		.getSelectedItem();
		if (currentConfig.getConfiguration() != null)
			currentUI.setConfiguration(currentConfig.getConfiguration());
		if (profileListChooser.getSelectedIndex() == 0)
			profileList.setDefaultConfig(null);
		else
			profileList.setDefaultConfig(currentConfig);
		minusButton.setEnabled(profileListChooser.getSelectedIndex() != 0);
	}

	public GraphicalAdapterChooser(DataAdapterRegistry registry,
			IOOperation<IN, OUT> op, BackgroundEventQueue queue, Frame frame,
			boolean modal, IN input) {
		setLayout(new BorderLayout());
		add(northPanel, "North");
		add(uiPanel, "Center");
		add(okButtonPanel, "South");

		adapterList.setRenderer(new AdapterListRenderer());
		profileListChooserPanel.setOpaque(false);
		showProfilesButton.setContentAreaFilled(false);
		showProfilesButton.setBorderPainted(false);

		this.queue = queue;

		CompoundBorder paddedUIBorder = new CompoundBorder(uiPanelBorder,
				new EmptyBorder(10, 10, 10, 10));
		uiPanel.setBorder(paddedUIBorder);

		profileListChooserPanel.setBorder(new EmptyBorder(0, 0, 10, 0));

		northPanel.setLayout(new BorderLayout());

		profileButtonPanel.setLayout(new BoxLayout(profileButtonPanel,
				BoxLayout.X_AXIS));
		profileButtonPanel.setOpaque(false);

		Box adapterLabelPanel = new Box(BoxLayout.X_AXIS);
		adapterLabelPanel.add(adapterChooserLabel);
		adapterLabelPanel.add(Box.createHorizontalStrut(10));

		JPanel adapterListPanel = new JPanel();
		adapterListPanel.setLayout(new BorderLayout());
		adapterListPanel.setOpaque(false);
		adapterListPanel.add(adapterLabelPanel, "West");
		adapterListPanel.add(adapterList, "Center");
		adapterListPanel.add(profileButtonPanel, "East");
		adapterListPanel.setBorder(new EmptyBorder(0, 0, 10, 0));

		northPanel.add(adapterListPanel, "North");
		northPanel.add(profileListChooserPanel, "South");
		northPanel.setOpaque(false);
		uiPanel.setOpaque(false);
		uiPanel.setLayout(new BorderLayout());

		setProfileListVisible(false);
		setShowAdvanced(false);

		Box profileButtonBox = new Box(BoxLayout.X_AXIS);
		profileButtonBox.add(Box.createHorizontalStrut(10));
		profileButtonBox.add(plusButton);
		profileButtonBox.add(Box.createHorizontalStrut(5));
		profileButtonBox.add(minusButton);

		Box profileLabelPanel = new Box(BoxLayout.X_AXIS);
		profileLabelPanel.add(profileChooserLabel);
		profileLabelPanel.add(Box.createHorizontalStrut(10));

		profileListChooserPanel.setLayout(new BorderLayout());
		profileListChooserPanel.add(profileLabelPanel, "West");
		profileListChooserPanel.add(profileListChooser, "Center");
		profileListChooserPanel.add(profileButtonBox, "East");

		// add a button to remove a profile!

		NamedConfigEditor editor = new NamedConfigEditor();
		editor.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				storeCurrentProfile();
			}
		});
		profileListChooser.setEditable(true);
		profileListChooser.setEditor(editor);

		okButtonPanel.setLayout(new BoxLayout(okButtonPanel, BoxLayout.X_AXIS));
		okButtonPanel.add(Box.createHorizontalGlue());
		okButtonPanel.add(okButton);
		okButtonPanel.add(Box.createHorizontalStrut(10));
		okButtonPanel.add(cancelButton);
		okButtonPanel.add(Box.createHorizontalGlue());
		okButtonPanel.setOpaque(false);

		exceptionButtonPanel.setLayout(new BoxLayout(exceptionButtonPanel,
				BoxLayout.X_AXIS));
		exceptionButtonPanel.add(Box.createHorizontalGlue());
		exceptionButtonPanel.add(tryAgainButton);
		exceptionButtonPanel.add(Box.createHorizontalStrut(10));
		exceptionButtonPanel.add(exceptionCancelButton);
		exceptionButtonPanel.add(Box.createHorizontalGlue());
		exceptionButtonPanel.setOpaque(false);

		profileListChooser.addActionListener(profileChooserListener);

		showProfilesButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setProfileListVisible(!chooserConfig.getShowProfiles());
			}
		});
		showAdvancedButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setShowAdvanced(true);
			}
		});
		showBasicButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setShowAdvanced(false);
			}
		});
		plusButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addNewNamedConfig();
			}
		});
		minusButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				deleteCurrentConfig();
			}
		});

		tryAgainButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				restoreGUI();
			}
		});
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				commit();
			}
		});

		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				cancel();
			}
		});

		exceptionCancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				cancel();
			}
		});

		exceptionEditorPane.setEditable(false);
		// exceptionEditorPane.setPreferredSize(new Dimension(300,300));
		exceptionPanel.setPreferredSize(new Dimension(300, 300));
		int prefHeight = (int) profileListChooser.getPreferredSize()
		.getHeight();
		plusButton.setPreferredSize(new Dimension(prefHeight, prefHeight));
		minusButton.setPreferredSize(new Dimension(prefHeight, prefHeight));

		setMargins(10, 10, 10, 10);
		setRegistry(registry, op, input);
	}

	protected void setShowAdvanced(boolean showAdvanced) {
		/*
		 * (new Exception("set showAdvanced "+showAdvanced+", currentUI = "+
		 * currentUI)).printStackTrace();
		 */
		if (currentUI == null)
			return;
		setProfileListVisible(showAdvanced);
		profileList.getOperationToAdvancedMap().put(op,
				new Boolean(showAdvanced));
		AdapterConfiguration config = null;
		try {
			currentUI.acceptComponentConfig(true);
			config = currentUI.getConfig(op, currentAdapter, input);
		} catch (DataAdapterUIException ex) {

		}
		if (showAdvanced && currentUI.getAdvancedUI() != null) {
			setPreferredSize(new Dimension(800,550));
			setAdapterUI(currentUI.getAdvancedUI());
			// Didn't work right
//			setPreferredSize(currentUI.getPreferredSize());
//			System.out.println("GraphicalAdapterchooser: for advanced ui, setPreferredSize " + getPreferredSize()); // DEL
			// Would like to set the size depending on whether we're reading or writing, but
			// BBOP doesn't know about OBO, so we can't do this
//			if (op.equals(OBOAdapter.WRITE_ONTOLOGY)) {
//			setPreferredSize(new Dimension(700,500));
//			}
//			else
//			setPreferredSize(new Dimension(700,300));
		}
		else if (!showAdvanced && currentUI.getSimpleUI() != null) {
			setPreferredSize(new Dimension(600,160));
			setAdapterUI(currentUI.getSimpleUI());
//			System.out.println("GraphicalAdapterchooser: for simple ui, setPreferredSize " + getPreferredSize()); // DEL
		}
		currentUI.setConfiguration(config);
	}

	protected void restoreGUI() {
		remove(exceptionButtonPanel);
		uiPanel.removeAll();

		uiPanel.add((Component) currentUI, "Center");
		add(okButtonPanel, "South");

		validate();
		repaint();

		if (dialog != null)
			dialog.pack();
	}

	protected void showExceptionGUI(Throwable exception) {
		remove(okButtonPanel);
		uiPanel.removeAll();
		String html = "<html>" + "<body>" + "<b>"
		+ StringUtil.escapeHTML(exception.getMessage()) + "</b><br>";

		if (exception.getCause() != null) {
			if (exception.getCause().getMessage() != null)
				html += "<i>"
					+ StringUtil.escapeHTML(exception.getCause()
							.getMessage()) + "</i><br>";
			html += "<ul>";
			for (int i = 0; i < exception.getCause().getStackTrace().length; i++)
				html += "<li>"
					+ StringUtil.escapeHTML(exception.getCause()
							.getStackTrace()[i].toString());
			html += "</ul>";
		}
		html += "</body></html>";

		exceptionEditorPane.setText(html);

		uiPanel.add(exceptionPanel, "Center");
		add(exceptionButtonPanel, "South");

		validate();
		repaint();

		if (dialog != null)
			dialog.pack();
	}

	public AdapterConfiguration getConfiguration()
	throws DataAdapterUIException {
		return currentUI.getConfig(op, currentAdapter, input);
	}

	public void commit() {
		try {
			currentUI.acceptComponentConfig(false);
			AdapterConfiguration config = getConfiguration();
//			logger.debug("GraphicalAdapterChooser.commit -- loading files -- config: " + config);	
			loadtask = new DataAdapterOperationTask<IN, OUT>(currentAdapter, op, config, input);
			loadtask.addPostExecuteRunnable(new Runnable() {
				public void run() {
					committed = true;
					chooserConfig.getOperationToLastAdapterMap().put(
							op.getID(), currentAdapter.getID());
					storeCurrentProfile();
					storeProfileList();
					flushConfiguration();
					if (dialog != null) {
						dialog.dispose();
					}
				}
			});

			loadtask.addFailedRunnable(new Runnable() {
				public void run() {
					showExceptionGUI(loadtask.getException());
				}
			});

			loadtask.addCancelledRunnable(new Runnable() {
				public void run() {
					restoreGUI();
				}
			});

			queue.scheduleTask(loadtask);
		} catch (DataAdapterUIException ex) {
			if (ex.showMessage())
				showExceptionGUI(ex);
		}
	}

	protected void cancel() {
		if (currentUI != null)
			currentUI.cleanup();
		if (dialog != null)
			dialog.dispose();
	}

	public void setRegistry(DataAdapterRegistry registry,
			IOOperation<IN, OUT> op, IN input) {
		this.registry = registry;
		this.op = op;
		this.input = input;
		DataAdapter[] adapters = DataAdapterUtil.getAdapters(registry, op,
				checkClasses);
		adapterList.removeActionListener(adapterListListener);
		adapterList.removeAllItems();
		for (int i = 0; i < adapters.length; i++) {
			adapterList.addItem(adapters[i]);
		}
		adapterList.addActionListener(adapterListListener);
		setCurrentAdapter(adapters[0]);
		// adapterList.setSelectedIndex(0);
	}

	public DataAdapter getLastAdapter() {
		return currentAdapter;
	}

	protected void configureProfileButtonPanel() {
		profileButtonPanel.removeAll();
		profileButtonPanel.add(Box.createHorizontalStrut(10));

		if (currentUI.getSimpleUI() != null) {
			profileButtonPanel.add(showBasicButton);
			profileButtonPanel.add(Box.createHorizontalStrut(10));
		}
		if (currentUI.getAdvancedUI() != null) {
			profileButtonPanel.add(showAdvancedButton);
			profileButtonPanel.add(Box.createHorizontalStrut(10));
		}
		profileButtonPanel.add(showProfilesButton);
		profileButtonPanel.validate();
	}

	public void setAdapterUI(DataAdapterUI ui) {
		if (!(ui instanceof Component) && (ui instanceof GraphicalUI))
			throw new IllegalArgumentException("GraphicalAdapterChooser can "
					+ "only display uis that extend "
					+ "java.awt.Component and " + "GraphicalUI");
		if (currentUI != null) {
			currentUI.cleanup();
		}
		this.currentUI = (GraphicalUI) ui;
		currentUI.init(this, op, currentAdapter, input);
		uiPanel.removeAll();
		configureProfileButtonPanel();
		uiPanel.add((Component) currentUI, "Center");
		uiPanel.validate();

		validate();
		if (dialog != null)
			dialog.pack();
	}

	public OUT getResult() throws DataAdapterException {
		if (loadtask.getException() != null) {
			if (loadtask.getException() instanceof DataAdapterException)
				throw (DataAdapterException) loadtask.getException();
			else
				throw new DataAdapterException(loadtask.getException());
		} else
			return loadtask.getResults();
	}

	public boolean showDialog(String title, JFrame owner) {
		dialog = new JDialog(owner);
		dialog.setContentPane(this);
		dialog.setModal(true);
		dialog.setTitle(title);
		dialog.pack();
		SwingUtil.center(owner, dialog);
		dialog.setVisible(true);
		dialog.dispose();
		return committed;
	}
}

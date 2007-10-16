package org.oboedit.gui.widget;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import java.io.*;
import java.net.*;
import java.util.*;

import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.GUIManager;
import org.bbop.io.IOUtil;
import org.obo.filters.*;

public class FilterPairEditor extends JPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public static class FilterPanelConfig implements ComponentConfiguration {
		protected boolean linkFilterVisible = false;
		protected boolean termFilterVisible = true;
		protected boolean specEditorVisible = false;
		protected boolean compoundFiltering = false;
		protected boolean keywordMode = true;
		protected FilterPair filterPair = null;

		public void setKeywordMode(boolean keywordMode) {
			this.keywordMode = keywordMode;
		}

		public boolean getKeywordMode() {
			return keywordMode;
		}

		public void setFilterPair(FilterPair filterPair) {
			this.filterPair = filterPair;
		}

		public FilterPair getFilterPair() {
			return filterPair;
		}

		public FilterPanelConfig() {
		}

		public void setCompoundFiltering(boolean compoundFiltering) {
			this.compoundFiltering = compoundFiltering;
		}

		public boolean getCompoundFiltering() {
			return compoundFiltering;
		}

		public void setTermFilterVisible(boolean termFilterVisible) {
			this.termFilterVisible = termFilterVisible;
		}

		public boolean getTermFilterVisible() {
			return termFilterVisible;
		}

		public void setSpecEditorVisible(boolean specEditorVisible) {
			this.specEditorVisible = specEditorVisible;
		}

		public boolean getSpecEditorVisible() {
			return specEditorVisible;
		}

		public boolean getLinkFilterVisible() {
			return linkFilterVisible;
		}

		public void setLinkFilterVisible(boolean linkFilterVisible) {
			this.linkFilterVisible = linkFilterVisible;
		}
	}

	protected FilterPanelConfig config = new FilterPanelConfig();

	protected FilterRenderEditor objectFilterEditor = new FilterRenderEditor();
	protected FilterRenderEditor linkFilterEditor = new FilterRenderEditor(true);

	protected java.util.List renderStatusListeners = new ArrayList();

	protected JCheckBox compoundCheckBox = new JCheckBox("Compound filtering");
	protected JCheckBox renderCheckBox = new JCheckBox("Rendering controls");
	protected JCheckBox linkCheckBox = new JCheckBox("Link filtering");
	protected JCheckBox termCheckBox = new JCheckBox("Term filtering", true);
	protected JCheckBox keywordCheckBox = new JCheckBox("Keyword filtering",
			true);

	protected URL loadIconURL = getClass().getClassLoader().getResource(
			"org/oboedit/gui/resources/icons/folder.gif");
	protected URL diskIconURL = getClass().getClassLoader().getResource(
			"org/oboedit/gui/resources/icons/floppy.gif");
	protected URL fileIconURL = getClass().getClassLoader().getResource(
			"org/oboedit/gui/resources/icons/file.gif");
	protected URL customizeIconURL = getClass()
			.getClassLoader()
			.getResource(
					"org/oboedit/gui/resources/icons/customize.gif");
	protected URL simpleIconURL = getClass()
			.getClassLoader()
			.getResource(
					"org/oboedit/gui/resources/icons/customize_simple.gif");

	protected Icon customizeIcon = new ImageIcon(customizeIconURL);
	protected Icon simpleIcon = new ImageIcon(simpleIconURL);
	protected Icon diskIcon = new ImageIcon(diskIconURL);

	protected JButton newButton = new JButton(new ImageIcon(fileIconURL));
	protected JButton loadButton = new JButton(new ImageIcon(loadIconURL));
	protected JButton saveButton = new JButton(diskIcon);

	protected JPanel objectPanel = new JPanel();
	protected JPanel linkPanel = new JPanel();
	protected JPanel buttonPanel = new JPanel();
	protected JPanel advancedPanel = new JPanel();
	protected boolean advancedMode = true;

	JTabbedPane filtersPanel = new JTabbedPane();

	protected FilterPair filterPair;

	protected java.util.List extendedTabs = new ArrayList();

	protected boolean showAdvancedTab = true;

	protected boolean rendererOptionAllowed = true;
	protected boolean keywordOptionAllowed = true;

	protected class ExtendedTab {
		protected Component c;
		protected String name;

		public ExtendedTab(Component c, String name) {
			this.c = c;
			this.name = name;
		}

		public Component getComponent() {
			return c;
		}

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}
	}

	public void addActionListener(ActionListener listener) {
		objectFilterEditor.addActionListener(listener);
		linkFilterEditor.addActionListener(listener);
	}

	public void removeActionListener(ActionListener listener) {
		objectFilterEditor.removeActionListener(listener);
		linkFilterEditor.removeActionListener(listener);
	}

	public void addRenderStatusListener(RenderStatusListener listener) {
		renderStatusListeners.add(listener);
	}

	public void removeRenderStatusListener(RenderStatusListener listener) {
		renderStatusListeners.remove(listener);
	}

	protected void fireRendererStatusChanged(RenderStatusEvent event) {
		Iterator it = renderStatusListeners.iterator();
		while (it.hasNext()) {
			RenderStatusListener listener = (RenderStatusListener) it.next();
			listener.statusChanged(event);
		}
	}

	public boolean getAdvancedMode() {
		return advancedMode;
	}

	public ComponentConfiguration getConfig() {
		acceptEdits();
		return config;
	}

	public void setShowAdvancedTab(boolean showAdvancedTab) {
		if (this.showAdvancedTab && !showAdvancedTab) {
			filtersPanel.removeTabAt(filtersPanel.getTabCount() - 1);
		}
		if (showAdvancedTab)
			filtersPanel.addTab("Advanced Options", customizeIcon,
					advancedPanel);
		this.showAdvancedTab = showAdvancedTab;
		validate();
		repaint();
	}

	public void setConfig(ComponentConfiguration c) {
		FilterPanelConfig cin = (FilterPanelConfig) c;
//		this.config = config;
		setKeywordMode(cin.getKeywordMode());
		setShowCompoundFilter(cin.getCompoundFiltering());
		setLinkFilterVisible(cin.getLinkFilterVisible());
		setSpecEditorVisible(cin.getSpecEditorVisible());
		setTermFilterVisible(cin.getTermFilterVisible());
		setFilterPair(cin.getFilterPair());
		filtersPanel.setSelectedIndex(0);
		this.config = cin;
	}

	public void addExtendedTab(Component c, String name, Icon icon) {
		extendedTabs.add(new ExtendedTab(c, name));
		filtersPanel.addTab(name, icon, c);
		filtersPanel.setSelectedComponent(c);
	}

	public void setTabName(Component c, String name) {
		int index = filtersPanel.indexOfComponent(c);
		if (index > 0)
			filtersPanel.setTitleAt(index, name);
		Iterator it = extendedTabs.iterator();
		while(it.hasNext()) {
			ExtendedTab et = (ExtendedTab) it.next();
			if (et.getComponent().equals(c)) {
				et.setName(name);
				break;
			}
		}
	}

	public int getTabIndex(Component c) {
		return filtersPanel.indexOfComponent(c);
	}

	public void removeExtendedTab(Component c) {
		Iterator it = extendedTabs.iterator();
		while (it.hasNext()) {
			ExtendedTab tab = (ExtendedTab) it.next();
			if (tab.getComponent().equals(c)) {
				it.remove();
				break;
			}
		}
		filtersPanel.remove(c);
	}

	public void setAdvancedMode(boolean advancedMode) {
		this.advancedMode = advancedMode;
	}

	public void newFilter() {
		setFilterPair(new FilterPairImpl());
	}

	public void load() {
		FilterPair pair = loadFilterPair();
		if (pair != null)
			setFilterPair(pair);
	}

	public void save() {
		save(getFilterPair());
	}

	public static FilterPair loadFilterPair() {
		JFileChooser chooser = new JFileChooser();
		int returnVal = chooser.showOpenDialog(GUIManager.getManager()
				.getFrame());
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			File file = chooser.getSelectedFile();
			try {
				return loadFilterPair(file);
			} catch (IOException ex) {
				return null;
			}
		} else
			return null;
	}

	public static FilterPair loadFilterPair(File file) throws IOException {
		return loadFilterPair(file.toString());
	}

	public static FilterPair loadFilterPair(String path) throws IOException {
		XMLDecoder d = new XMLDecoder(new BufferedInputStream(
				IOUtil.getStream(path)));
		d.setExceptionListener(new ExceptionListener() {
			public void exceptionThrown(Exception ex) {
				ex.printStackTrace();
			}
		});
		FilterPair result = (FilterPair) d.readObject();
		System.err.println("LOADED filterpair: " + result
				+ ", compoundfilterimpl.class = " + CompoundFilterImpl.class);
		d.close();
		return result;
	}

	public void load(File file) {
		try {
			setFilterPair(loadFilterPair(file));
		} catch (IOException ex) {
			ex.printStackTrace();
		}
	}

	public void load(String filename) {
		load(new File(filename));
	}

	public static void save(FilterPair filterPair) {
		JFileChooser chooser = new JFileChooser();
		int returnVal = chooser.showSaveDialog(GUIManager.getManager()
				.getFrame());
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			File file = chooser.getSelectedFile();
			save(file.toString(), filterPair);
		}
	}

	public static void save(String filename, FilterPair filterPair) {
		try {
			XMLEncoder e = new XMLEncoder(new BufferedOutputStream(
					new FileOutputStream(filename)));
			e.writeObject(filterPair);
			e.close();
		} catch (IOException ex) {
			ex.printStackTrace();
		}
	}

	public void save(String filename) {
		save(filename, getFilterPair());
	}

	@Override
	public void setFont(Font font) {
		super.setFont(font);
		if (objectFilterEditor != null)
			objectFilterEditor.setFont(font);
		if (filtersPanel != null)
			filtersPanel.setFont(font);
		if (linkFilterEditor != null)
			linkFilterEditor.setFont(font);
		if (compoundCheckBox != null)
			compoundCheckBox.setFont(font);
		if (renderCheckBox != null)
			renderCheckBox.setFont(font);
		if (linkCheckBox != null)
			linkCheckBox.setFont(font);
		if (termCheckBox != null)
			termCheckBox.setFont(font);
		if (keywordCheckBox != null)
			keywordCheckBox.setFont(font);
		if (loadButton != null)
			loadButton.setFont(font);
		if (saveButton != null)
			saveButton.setFont(font);
	}

	public void setButtonColor(Color buttonColor) {
		objectFilterEditor.setButtonColor(buttonColor);
		linkFilterEditor.setButtonColor(buttonColor);

		compoundCheckBox.setBackground(buttonColor);
		renderCheckBox.setBackground(buttonColor);
		linkCheckBox.setBackground(buttonColor);
		termCheckBox.setBackground(buttonColor);
		keywordCheckBox.setBackground(buttonColor);

		newButton.setBackground(buttonColor);
		loadButton.setBackground(buttonColor);
		saveButton.setBackground(buttonColor);
	}

	@Override
	public void setBackground(Color background) {
		super.setBackground(background);
		if (filtersPanel != null) {
			filtersPanel.setBackground(background);
			objectPanel.setBackground(background);
			linkPanel.setBackground(background);
			advancedPanel.setBackground(background);
		}
	}

	public FilterPairEditor() {
		// filtersPanel.setLayout(new BoxLayout(filtersPanel,
		// BoxLayout.Y_AXIS));
		objectPanel.setLayout(new BorderLayout());
		objectPanel.add(objectFilterEditor, "Center");
		// objectPanel.setOpaque(false);

		linkPanel.setLayout(new BorderLayout());
		// linkPanel.setOpaque(false);
		linkPanel.add(linkFilterEditor, "Center");

		newButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				newFilter();
			}
		});

		loadButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				load();
			}
		});

		saveButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				save();
			}
		});

		compoundCheckBox.setOpaque(false);
		linkCheckBox.setOpaque(false);
		termCheckBox.setOpaque(false);
		keywordCheckBox.setOpaque(false);
		renderCheckBox.setOpaque(false);

		compoundCheckBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setShowCompoundFilter(compoundCheckBox.isSelected());
			}
		});
		renderCheckBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setSpecEditorVisible(renderCheckBox.isSelected());
			}
		});
		linkCheckBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setLinkFilterVisible(linkCheckBox.isSelected());
			}
		});
		termCheckBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setTermFilterVisible(termCheckBox.isSelected());
			}
		});
		keywordCheckBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setKeywordMode(keywordCheckBox.isSelected());
			}
		});

		// filtersPanel.add(objectPanel, "Term filter");

		/*
		 * buttonPanel.setLayout(new BoxLayout(BoxLayout.X_AXIS));
		 * buttonPanel.add(compoundCheckBox);
		 * buttonPanel.add(Box.createHorizontalStrut(5));
		 * buttonPanel.add(renderCheckBox);
		 * buttonPanel.add(Box.createHorizontalStrut(5));
		 * buttonPanel.add(linkCheckBox);
		 * buttonPanel.add(Box.createHorizontalStrut(5));
		 * buttonPanel.add(termCheckBox);
		 * buttonPanel.add(Box.createHorizontalStrut(5));
		 * buttonPanel.add(keywordCheckBox);
		 * buttonPanel.add(Box.createHorizontalGlue());
		 * buttonPanel.setOpaque(false); advancedPanel.setLayout(new
		 * BorderLayout()); advancedPanel.add(buttonPanel, "Center");
		 */
		advancedPanel.setLayout(new BoxLayout(advancedPanel, BoxLayout.Y_AXIS));
		configureAdvancedPanel();

		setLayout(new BorderLayout());
		add(filtersPanel, "Center");
		setFilterPair(new FilterPairImpl());

		updateSplitPane();
		filtersPanel.setSelectedIndex(0);
	}
	
	public Box getLoadSaveButtonPanel() {
		Box leftButtons = new Box(BoxLayout.X_AXIS);
		leftButtons.add(newButton);
		leftButtons.add(loadButton);
		leftButtons.add(saveButton);
		return leftButtons;
	}

	public void setRendererOptionAllowed(boolean rendererOptionAllowed) {
		this.rendererOptionAllowed = rendererOptionAllowed;
		setSpecEditorVisible(rendererOptionAllowed
				&& config.getSpecEditorVisible());
		configureAdvancedPanel();
	}

	public void setKeywordOptionAllowed(boolean keywordOptionAllowed) {
		this.keywordOptionAllowed = keywordOptionAllowed;
		setKeywordMode(keywordOptionAllowed && config.getKeywordMode());
		configureAdvancedPanel();
	}

	protected void configureAdvancedPanel() {
		advancedPanel.removeAll();
		advancedPanel.add(compoundCheckBox);
		if (rendererOptionAllowed)
			advancedPanel.add(renderCheckBox);
		advancedPanel.add(linkCheckBox);
		advancedPanel.add(termCheckBox);
		if (keywordOptionAllowed)
			advancedPanel.add(keywordCheckBox);
	}

	protected boolean isLinkFilterActive() {
		return config.getLinkFilterVisible();
	}

	protected boolean isObjectFilterActive() {
		return config.getTermFilterVisible();
	}

	public void acceptEdits() {
		objectFilterEditor.acceptEdits();
		linkFilterEditor.acceptEdits();
		if (isObjectFilterActive()) {
			filterPair.setObjectFilter(objectFilterEditor.getFilter());
			if (config.getSpecEditorVisible())
				filterPair
						.setObjectRenderSpec((ObjectRenderSpec) objectFilterEditor
								.getSpec());
			else
				filterPair.setObjectRenderSpec(null);
		} else {
			filterPair.setObjectFilter(null);
			filterPair.setObjectRenderSpec(null);
		}

		if (isLinkFilterActive()) {
			filterPair.setLinkFilter(linkFilterEditor.getFilter());
			if (config.getSpecEditorVisible())
				filterPair.setLinkRenderSpec((LinkRenderSpec) linkFilterEditor
						.getSpec());
			else
				filterPair.setLinkRenderSpec(null);
		} else {
			filterPair.setLinkFilter(null);
			filterPair.setLinkRenderSpec(null);
		}
	}

	public void reset() {
		newFilter();
		extendedTabs.clear();
		rebuildTabs();
	}

	protected void rebuildTabs() {
		filtersPanel.removeAll();
		if (config.getTermFilterVisible())
			filtersPanel.add(objectPanel, "Term filter");
		if (config.getLinkFilterVisible())
			filtersPanel.add(linkPanel, "Link filter");
		filtersPanel.addTab("Advanced Options", customizeIcon, advancedPanel);
		// filtersPanel.setIconAt(
		Iterator it = extendedTabs.iterator();
		while (it.hasNext()) {
			ExtendedTab tab = (ExtendedTab) it.next();
			filtersPanel.add(tab.getComponent(), tab.getName());
		}
		filtersPanel.validate();
	}

	protected void updateSplitPane() {
		rebuildTabs();
		filtersPanel.setSelectedComponent(advancedPanel);
	}

	protected void updateGUI() {
		objectFilterEditor.setFilter(filterPair.getObjectFilter());
		objectFilterEditor.setSpec(filterPair.getObjectRenderSpec());

		linkFilterEditor.setFilter(filterPair.getLinkFilter());
		linkFilterEditor.setSpec(filterPair.getLinkRenderSpec());

		boolean termVisible = false;
		boolean filterVisible = false;
		boolean specVisible = false;
		boolean compoundVisible = false;

		if (filterPair.getObjectFilter() != null) {
			termVisible = true;
		}

		if (filterPair.getLinkFilter() != null) {
			filterVisible = !config.getKeywordMode();
		}
		if (filterPair.getObjectRenderSpec() != null
				|| filterPair.getLinkRenderSpec() != null) {
			specVisible = true;
		}
		if (isCompound(filterPair.getLinkFilter())
				|| isCompound(filterPair.getObjectFilter())) {
			compoundVisible = true;
		}

		if (termVisible)
			setTermFilterVisible(termVisible);

		if (filterVisible)
			setLinkFilterVisible(filterVisible);

		if (specVisible)
			setSpecEditorVisible(specVisible);

		if (compoundVisible && !config.getKeywordMode())
			setShowCompoundFilter(compoundVisible);
		linkCheckBox.setEnabled(!config.getKeywordMode());
		compoundCheckBox.setEnabled(!config.getKeywordMode());
	}

	protected boolean isCompound(Filter f) {
		if (f == null)
			return false;
		if (!(f instanceof CompoundFilter))
			return false;
		CompoundFilter filter = (CompoundFilter) f;
		return filter.getFilters().size() > 1
				|| (filter.getFilters().size() == 1 && filter.getFilters().get(
						0) instanceof CompoundFilter);
	}

	public void setFilterPair(FilterPair filterPair) {
		this.filterPair = filterPair;
		config.setFilterPair(filterPair);
		updateGUI();
	}

	public FilterPair getFilterPair() {
		acceptEdits();
		return (FilterPair) filterPair.clone();
	}

	public void setTermFilterVisible(boolean termFilterVisible) {
		boolean doRedraw = config.getTermFilterVisible() != termFilterVisible;
		config.setTermFilterVisible(termFilterVisible);
		if (doRedraw) {
			updateSplitPane();
		}

		termCheckBox.setSelected(termFilterVisible);
	}

	public void setLinkFilterVisible(boolean linkFilterVisible) {
		boolean doRedraw = config.getLinkFilterVisible() != linkFilterVisible;
		config.setLinkFilterVisible(linkFilterVisible);
		if (doRedraw) {
			updateSplitPane();
		}

		linkCheckBox.setSelected(linkFilterVisible);
	}

	public void setShowCompoundFilter(boolean compoundVisible) {
		config.setCompoundFiltering(compoundVisible);
		linkFilterEditor.setShowCompoundFilter(compoundVisible);
		objectFilterEditor.setShowCompoundFilter(compoundVisible);

		compoundCheckBox.setSelected(compoundVisible);
	}

	public void setSpecEditorVisible(boolean specEditorVisible) {
		fireRendererStatusChanged(new RenderStatusEvent(this, specEditorVisible));
		config.setSpecEditorVisible(specEditorVisible);
		linkFilterEditor.setSpecEditorVisible(specEditorVisible);
		objectFilterEditor.setSpecEditorVisible(specEditorVisible);

		renderCheckBox.setSelected(specEditorVisible);
	}

	public void setKeywordMode(boolean keywordMode) {
		if (keywordMode != config.getKeywordMode()) {
			if (keywordMode) {
				objectFilterEditor.setFilterBuilder(new KeywordFilterBuilder());

				setShowCompoundFilter(false);
				setLinkFilterVisible(false);
				setTermFilterVisible(true);

				linkCheckBox.setEnabled(false);
				compoundCheckBox.setEnabled(false);
			} else {
				CompoundFilter f = (CompoundFilter) objectFilterEditor
						.getFilter();

				objectFilterEditor.setFilterBuilder(new FilterBuilder());

				linkCheckBox.setEnabled(true);
				compoundCheckBox.setEnabled(true);

				if (f.getFilters().size() > 1)
					setShowCompoundFilter(true);
			}
			config.setKeywordMode(keywordMode);
			keywordCheckBox.setSelected(keywordMode);
			repaint();
		}
	}

	public boolean getSpecEditorVisible() {
		return config.getSpecEditorVisible();
	}
}

package org.oboedit.gui;

import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import org.bbop.dataadapter.*;
import org.bbop.swing.*;
import org.bbop.util.*;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.dataadapter.OBOSerializationEngine;
import org.obo.datamodel.*;
import org.obo.filters.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.gui.widget.FilterBuilder;
import org.oboedit.gui.widget.FilterPairEditor;

public class AdvancedOBOUI extends JPanel implements GraphicalUI {

	/**
	 * 
	 */
	private static final long serialVersionUID = 4097591578255403651L;

	public final static int MAX_HISTORY_LENGTH = 5;

	protected JButton browseButton = new JButton("Browse...");

	protected IOOperation op;

	protected String defaultBrowsePath = System.getProperty("user.home");

	protected ListEditor pathList;

	protected ListEditor namespaceList;

	protected OBOFileAdapter.OBOAdapterConfiguration currentProfile;

	protected JLabel pathLabel;

	protected JLabel pathOrURLLabel = new JLabel("Path or URL");

	protected JLabel urlLabel = new JLabel("URL for import");

	protected JCheckBox importExternalRefsBox = new JCheckBox(
			"Import external references");

	protected JCheckBox allowDanglingBox = new JCheckBox(
			"Allow dangling references");

	protected JTextField pathField = new JTextField(20);

	protected JTextField urlField = new JTextField(20);

	protected JLabel nsLabel = new JLabel("Namespace");

	protected JTextField nsField = new JTextField(10);

	protected JPanel nsPanel = new JPanel();

	protected static Font boldFont = new Font("Arial", Font.BOLD, 12);

	protected JButton delButton = new JButton("Delete");

	protected boolean notListPick = false;

	protected int currentSelectedIndex = -1;

	protected Vector paths = new Vector();

	protected HashMap nsmap = new HashMap();

	protected OBOSession currentHistory;

	protected JPanel pathBox;

	protected JButton addSaveButton = new JButton("Add save record");

	protected JButton delSaveButton = new JButton("Delete save record");

	protected JPanel saveButtonPanel = new JPanel();

	protected boolean embed = false;

	protected JPanel serializerPanel = new JPanel();

	protected JLabel serializerLabel = new JLabel("Output type");

	protected JComboBox serializerBox = new JComboBox();
	
	protected GraphicalUI advancedUI;
	
	public void setUIConfiguration(UIConfiguration uiconfig) {
	}

	public void acceptComponentConfig(boolean storeonly)
			throws DataAdapterUIException {
		collectParams();
		if (!storeonly && op.equals(OBOAdapter.WRITE_ONTOLOGY)) {
			java.util.List overwrite = new LinkedList();
			Iterator it = currentProfile.getSaveRecords().iterator();
			while (it.hasNext()) {
				Object o = it.next();

				OBOSerializationEngine.FilteredPath path = (OBOSerializationEngine.FilteredPath) o;
				if (path.getPath().length() == 0) {
					throw new DataAdapterUIException("Cannot save to empty "
							+ "path.");
				}
				if ((new File(path.getPath())).exists())
					overwrite.add(path.getPath());
			}
			if (overwrite.size() > 0) {
				StringBuffer out = new StringBuffer();
				out.append("The following files exist:\n");
				it = overwrite.iterator();
				while (it.hasNext()) {
					String s = (String) it.next();
					out.append("   * " + s + "\n");
				}
				out.append("Overwrite these files?");
				int val = JOptionPane.showConfirmDialog(this, out.toString(),
						"Overwrite files?", JOptionPane.YES_NO_OPTION);
				if (val != JOptionPane.YES_OPTION)
					throw new DataAdapterUIException();
			}
		}
	}

	public UIConfiguration getUIConfiguration() {
		return null;
	}

	protected GraphicalUI simpleUI;

	public Set getFiles(IOOperation op) {
		return new HashSet();
	}

	public GraphicalUI getAdvancedUI() {
		return null;
	}

	public void setSimpleUI(GraphicalUI simpleUI) {
		this.simpleUI = simpleUI;
	}

	public GraphicalUI getSimpleUI() {
		return simpleUI;
	}

	protected class IOProfileEditor extends JPanel implements
			GenericEditorComponent {
		/**
		 * 
		 */
		private static final long serialVersionUID = -6166108451127532061L;

		protected ListEditor editor;

		protected JPanel panel = new JPanel();

		protected FilterBuilder objectFilterEditor = new FilterBuilder();

		protected FilterBuilder linkFilterEditor = new FilterBuilder();

		protected JTextField pathField = new JTextField();

		protected JTextArea remarkField = new JTextArea();

		protected JScrollPane remarkScroller = new JScrollPane(remarkField,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

		protected JCheckBox categoryBox = new JCheckBox(
				"Discard unused categories");

		protected JCheckBox filterBox = new JCheckBox("Filter terms");

		protected JCheckBox filterTypesBox = new JCheckBox(
				"Always save properties");

		protected JCheckBox linkFilterBox = new JCheckBox("Filter links");

		protected JCheckBox allowDanglingBox = new JCheckBox(
				"Allow dangling parents");

		protected JCheckBox prefilterBox = new JCheckBox("Do type prefiltering");

		protected JComboBox prefilterTypeChooser = new JComboBox();

		protected JLabel rootAlgorithmLabel = new JLabel("Root selection "
				+ "algorithm");

		protected JComboBox rootAlgorithmChooser = new JComboBox();

		protected JCheckBox saveImpliedBox = new JCheckBox("Save implied links");

		protected JComboBox impliedTypeBox = new JComboBox();

		protected JCheckBox realizeImpliedBox = new JCheckBox(
				"Realize implied links");

		protected JComboBox idRuleSelector = new JComboBox();

		public void setMasterComponent(Component c) {
			if (c instanceof ListEditor)
				editor = (ListEditor) c;
		}

		public IOProfileEditor() {
			setLayout(new BorderLayout());

			impliedTypeBox
					.addItem(OBOSerializationEngine.SAVE_FOR_PRESENTATION);
			impliedTypeBox.addItem(OBOSerializationEngine.SAVE_ALL);

			idRuleSelector.addItem("Don't write id rules");
			idRuleSelector.addItem("Write current id rules");
			idRuleSelector.addItem("Write originally loaded id rules");

			rootAlgorithmChooser.addItem("GREEDY");
			rootAlgorithmChooser.addItem("STRICT");

			currentHistory = SessionManager.getManager().getSession();
			Iterator it = TermUtil.getRelationshipTypes(currentHistory).iterator();
			while (it.hasNext()) {
				OBOProperty property = (OBOProperty) it.next();
				prefilterTypeChooser.addItem(property);
			}

			objectFilterEditor.setFilterFactory(new ObjectFilterFactory());
			linkFilterEditor.setFilterFactory(new LinkFilterFactory());

			objectFilterEditor.setPreferredSize(null);
			linkFilterEditor.setPreferredSize(null);

			objectFilterEditor.setShowCompoundFilter(true);
			linkFilterEditor.setShowCompoundFilter(true);

			objectFilterEditor.setOpaque(false);
			linkFilterEditor.setOpaque(false);
			filterBox.setOpaque(false);
			categoryBox.setOpaque(false);
			filterTypesBox.setOpaque(false);
			linkFilterBox.setOpaque(false);
			prefilterBox.setOpaque(false);

			allowDanglingBox.setOpaque(false);
			saveImpliedBox.setOpaque(false);
			realizeImpliedBox.setOpaque(false);

			JButton newButton = new JButton(Preferences
					.loadLibraryIcon("file.gif"));
			JButton loadButton = new JButton(Preferences
					.loadLibraryIcon("folder.gif"));
			JButton saveButton = new JButton(Preferences
					.loadLibraryIcon("floppy.gif"));
			newButton.setPreferredSize(new Dimension(20, 20));
			loadButton.setPreferredSize(new Dimension(20, 20));
			saveButton.setPreferredSize(new Dimension(20, 20));

			newButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					objectFilterEditor.setFilter(new CompoundFilterImpl());
					linkFilterEditor.setFilter(new CompoundFilterImpl());
					rebuildPanel();
				}
			});
			loadButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					FilterPair pair = FilterPairEditor.loadFilterPair();
					if (pair != null) {
						objectFilterEditor
								.setFilter((pair.getObjectFilter() == null ? new ObjectFilterImpl()
										: pair.getObjectFilter()));
						linkFilterEditor
								.setFilter((pair.getObjectFilter() == null ? new ObjectFilterImpl()
										: pair.getObjectFilter()));
						filterBox.setSelected(pair.getObjectFilter() != null);
						linkFilterEditor
								.setEnabled(pair.getLinkFilter() != null);
						rebuildPanel();
					}
				}
			});
			saveButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					FilterPair pair = new FilterPairImpl();
					objectFilterEditor.acceptEdits();
					linkFilterEditor.acceptEdits();
					if (filterBox.isSelected())
						pair.setObjectFilter(objectFilterEditor.getFilter());
					if (linkFilterBox.isSelected())
						pair.setLinkFilter(linkFilterEditor.getFilter());
					FilterPairEditor.save(pair);
				}
			});

			TitledBorder linkBorder = new TitledBorder("Link filtering");
			TitledBorder objectBorder = new TitledBorder("Object filtering");
			TitledBorder remarkBorder = new TitledBorder("OBO File Remark");



			objectFilterEditor.setBorder(objectBorder);
			linkFilterEditor.setBorder(linkBorder);
			remarkScroller.setBorder(remarkBorder);

			JButton browseButton = new JButton("Browse...");
			browseButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					File startFile = new File(pathField.getText());
					String startPath = null;
					if (startFile.exists()) {
						if (startFile.isDirectory())
							startPath = startFile.toString();
						else
							startPath = startFile.getParent();
					} else if (startFile.getParentFile() != null
							&& startFile.getParentFile().exists()) {
						startPath = startFile.getParent();
					}

					JFileChooser chooser;
					if (startPath == null)
						chooser = new JFileChooser();
					else
						chooser = new JFileChooser(startPath);
					if (chooser.showSaveDialog(AdvancedOBOUI.this) == JFileChooser.APPROVE_OPTION) {
						File file = chooser.getSelectedFile();
						pathField.setText(file.toString());
						editor.commit();
					}
				}
			});

			JLabel pathLabel = new JLabel("Save path");

			Box labelBox = new Box(BoxLayout.X_AXIS);
			labelBox.add(pathLabel);
			labelBox.add(Box.createHorizontalStrut(10));

			JPanel pathPanel = new JPanel();
			pathPanel.setLayout(new BorderLayout());
			pathPanel.add(labelBox, "West");
			pathPanel.add(pathField, "Center");
			pathPanel.add(browseButton, "East");
			pathPanel.setOpaque(false);

			add(pathPanel, "North");

			panel.setOpaque(false);
			// panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
			Box checkboxMainPanel = new Box(BoxLayout.Y_AXIS);
			Box checkboxPanelA = new Box(BoxLayout.X_AXIS);
			Box checkboxPanelA2 = new Box(BoxLayout.X_AXIS);
			Box checkboxPanelB = new Box(BoxLayout.X_AXIS);
			Box checkboxPanelC = new Box(BoxLayout.X_AXIS);
			checkboxPanelA.add(Box.createHorizontalGlue());
			checkboxPanelA.add(filterBox);
			checkboxPanelA.add(Box.createHorizontalStrut(10));
			checkboxPanelA.add(filterTypesBox);
			checkboxPanelA.add(Box.createHorizontalStrut(10));
			checkboxPanelA.add(linkFilterBox);
			checkboxPanelA.add(Box.createHorizontalStrut(10));
			checkboxPanelA.add(allowDanglingBox);
			checkboxPanelA.add(Box.createHorizontalStrut(10));
			checkboxPanelA.add(newButton);
			checkboxPanelA.add(loadButton);
			checkboxPanelA.add(saveButton);

			checkboxPanelA2.add(prefilterBox);
			checkboxPanelA2.add(prefilterTypeChooser);
			checkboxPanelA2.add(Box.createHorizontalStrut(10));
			checkboxPanelA2.add(categoryBox);
			checkboxPanelA2.add(Box.createHorizontalStrut(10));
			checkboxPanelA2.add(rootAlgorithmLabel);
			checkboxPanelA2.add(Box.createHorizontalStrut(5));
			checkboxPanelA2.add(rootAlgorithmChooser);

			checkboxPanelA.add(Box.createHorizontalGlue());
			checkboxPanelB.add(saveImpliedBox);
			checkboxPanelB.add(Box.createHorizontalStrut(10));
			checkboxPanelB.add(impliedTypeBox);
			checkboxPanelB.add(Box.createHorizontalStrut(10));
			checkboxPanelB.add(realizeImpliedBox);
			checkboxPanelC.add(Box.createHorizontalGlue());
			checkboxPanelC.add(idRuleSelector);
			checkboxPanelC.add(Box.createHorizontalGlue());

			checkboxMainPanel.add(checkboxPanelA);
			checkboxMainPanel.add(checkboxPanelA2);
			checkboxMainPanel.add(checkboxPanelB);
			checkboxMainPanel.add(checkboxPanelC);

			JPanel southPanel = new JPanel();
			southPanel.setLayout(new BorderLayout());
			southPanel.add(checkboxMainPanel, "West");
			southPanel.add(remarkScroller, "Center");

			add(panel, "Center");
			// add(checkboxMainPanel, "South");
			add(southPanel, "South");

			filterBox.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					rebuildPanel();
				}
			});

			prefilterBox.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					rebuildPanel();
				}
			});
			linkFilterBox.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					rebuildPanel();
				}
			});
			saveImpliedBox.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					impliedTypeBox.setEnabled(saveImpliedBox.isSelected());
					realizeImpliedBox.setEnabled(saveImpliedBox.isSelected());
				}
			});

			rebuildPanel();
			setPreferredSize(new Dimension(400, 300));
		}

		protected void rebuildPanel() {
			panel.remove(objectFilterEditor);
			panel.remove(linkFilterEditor);
			int count = 0;
			if (filterBox.isSelected()) {
				panel.add(objectFilterEditor);
				count++;
			}
			if (linkFilterBox.isSelected()) {
				panel.add(linkFilterEditor);
				count++;
			}
			boolean rootAlgorithmControlsEnabled = filterBox.isSelected()
					|| linkFilterBox.isSelected() || prefilterBox.isSelected();
			rootAlgorithmLabel.setEnabled(rootAlgorithmControlsEnabled);
			rootAlgorithmChooser.setEnabled(rootAlgorithmControlsEnabled);

			categoryBox.setEnabled(rootAlgorithmControlsEnabled);

			impliedTypeBox.setEnabled(saveImpliedBox.isSelected());
			realizeImpliedBox.setEnabled(saveImpliedBox.isSelected());
			prefilterTypeChooser.setEnabled(prefilterBox.isSelected());
			filterTypesBox.setEnabled(filterBox.isSelected());

			if (count < 1)
				count = 1;
			panel.setLayout(new GridLayout(count, 1));
			panel.validate();
			panel.repaint();
		}

		public Object createNewValue() {
			OBOSerializationEngine.FilteredPath path = new OBOSerializationEngine.FilteredPath(
					new FilterPairImpl(), "<new save path>");
			path.setRemark(SessionManager.getManager().getSession()
					.getCurrentHistory().getComment());
			return path;
		}

		public void load(java.lang.Object o) {
			if (!(o instanceof OBOSerializationEngine.FilteredPath))
				return;
			OBOSerializationEngine.FilteredPath profile = (OBOSerializationEngine.FilteredPath) o;
			pathField.setText(profile.getPath());
			remarkField.setText(profile.getRemark());
			objectFilterEditor.setFilter(profile.getFilterPair()
					.getObjectFilter());
			linkFilterEditor.setFilter(profile.getFilterPair().getLinkFilter());

			allowDanglingBox.setSelected(profile.getAllowDangling());
			saveImpliedBox.setSelected(profile.getSaveImplied());
			impliedTypeBox.setSelectedItem(profile.getImpliedType());
			realizeImpliedBox.setSelected(profile.getRealizeImpliedLinks());

			prefilterBox.setSelected(profile.getPrefilterProperty() != null);
			if (profile.getPrefilterProperty() != null) {
				OBOProperty property = (OBOProperty) SessionManager.getManager()
						.getSession().getObject(profile.getPrefilterProperty());
				prefilterTypeChooser.setSelectedItem(property);
			}
			rootAlgorithmChooser.setSelectedItem(profile.getRootAlgorithm());

			idRuleSelector.setSelectedIndex(profile.getIDRuleMode());
			/*
			 * Controller controller = Controller.getController(); if
			 * (!controller.getUseReasoner()){ saveImpliedBox.setEnabled(false);
			 * impliedTypeBox.setEnabled(false); }
			 */
			categoryBox.setSelected(profile.getDiscardUnusedCategories());
			filterBox.setSelected(profile.getDoFilter());
			filterTypesBox.setSelected(profile.getSaveTypes());
			linkFilterBox.setSelected(profile.getDoLinkFilter());
			rebuildPanel();
		}

		public void setEditable(boolean in) {
		}

		public void store(java.lang.Object saveme) {
			if (!(saveme instanceof OBOSerializationEngine.FilteredPath))
				return;
			OBOSerializationEngine.FilteredPath profile = (OBOSerializationEngine.FilteredPath) saveme;
			profile.setPath(pathField.getText());

			profile.setIDRuleMode(idRuleSelector.getSelectedIndex());

			if (remarkField.getText().trim().length() > 0)
				profile.setRemark(remarkField.getText().trim());

			profile.setAllowDangling(allowDanglingBox.isSelected());
			profile.setSaveImplied(saveImpliedBox.isSelected());
			profile.setImpliedType((String) impliedTypeBox.getSelectedItem());
			profile.setRealizeImpliedLinks(realizeImpliedBox.isSelected());
			profile.setDoFilter(filterBox.isSelected());
			profile.setSaveTypes(filterTypesBox.isSelected());
			profile.setDoLinkFilter(linkFilterBox.isSelected());
			if (prefilterBox.isSelected()) {
				profile
						.setPrefilterProperty(((OBOProperty) prefilterTypeChooser
								.getSelectedItem()).getID());
			}
			profile.setDiscardUnusedCategories((prefilterBox.isSelected()
					|| filterBox.isSelected() || linkFilterBox.isSelected())
					&& categoryBox.isSelected());
			profile.setRootAlgorithm((String) rootAlgorithmChooser
					.getSelectedItem());
			objectFilterEditor.acceptEdits();
			linkFilterEditor.acceptEdits();

			if (filterBox.isSelected())
				profile.getFilterPair().setObjectFilter(
						objectFilterEditor.getFilter());
			if (linkFilterBox.isSelected())
				profile.getFilterPair().setLinkFilter(
						linkFilterEditor.getFilter());
		}
	}

	protected class PathEditor extends JPanel implements GenericEditorComponent {
		/**
		 * 
		 */
		private static final long serialVersionUID = -6933319183519890664L;

		protected ListEditor editor;

		public void setMasterComponent(Component c) {
			if (c instanceof ListEditor)
				editor = (ListEditor) c;
		}

		public PathEditor() {
			FocusListener listener = new FocusListener() {
				public void focusLost(FocusEvent e) {
					editor.commit();
				}

				public void focusGained(FocusEvent e) {
				}
			};
			browseButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					File startFile = new File(pathField.getText());
					String startPath = null;
					if (startFile.exists()) {
						if (startFile.isDirectory())
							startPath = startFile.toString();
						else
							startPath = startFile.getParent();
					} else if (startFile.getParentFile() != null
							&& startFile.getParentFile().exists()) {
						startPath = startFile.getParent();
					}

					JFileChooser chooser;
					if (startPath == null)
						chooser = new JFileChooser();
					else
						chooser = new JFileChooser(startPath);
					if (chooser.showOpenDialog(AdvancedOBOUI.this) == JFileChooser.APPROVE_OPTION) {
						File file = chooser.getSelectedFile();
						pathField.setText(file.toString());
						editor.commit();
					}
				}
			});

			pathField.addFocusListener(listener);

			pathField.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					editor.commit();
				}
			});
			setAlignmentX(JComponent.LEFT_ALIGNMENT);
			add(pathOrURLLabel);
			pathOrURLLabel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
			JPanel temp = new JPanel();
			temp.setOpaque(false);
			temp.setLayout(new BoxLayout(temp, BoxLayout.X_AXIS));
			temp.setAlignmentX(JComponent.LEFT_ALIGNMENT);
			temp.add(pathField);
			temp.add(Box.createHorizontalStrut(10));
			temp.add(browseButton);
			add(temp);
		}

		public Object createNewValue() {
			return new EditableString("<new path>");
		}

		public void load(java.lang.Object o) {
			EditableString es = (EditableString) o;
			pathField.setText(es.toString());
		}

		public void setEditable(boolean in) {
		}

		public void store(java.lang.Object saveme) {
			EditableString es = (EditableString) saveme;
			es.setValue(pathField.getText());
		}
	}

	public AdvancedOBOUI() {
		serializerBox.addItem("OBO_1_2");
		serializerBox.addItem("OBO_1_0");

		serializerPanel.setOpaque(false);
		serializerPanel.setLayout(new BoxLayout(serializerPanel,
				BoxLayout.X_AXIS));
		serializerPanel.add(Box.createHorizontalGlue());
		serializerPanel.add(serializerLabel);
		serializerPanel.add(Box.createHorizontalStrut(10));
		serializerPanel.add(serializerBox);
		serializerPanel.add(Box.createHorizontalGlue());

		nsPanel.setLayout(new BoxLayout(nsPanel, BoxLayout.X_AXIS));
		nsPanel.add(nsLabel);
		nsPanel.add(Box.createHorizontalStrut(10));
		nsPanel.add(nsField);
		nsPanel.setOpaque(false);
		setOpaque(false);

		namespaceList = new ListEditor(new IOProfileEditor(), false, true,
				true, true, false);
		pathList = new ListEditor(new PathEditor(), true, true, true, true,
				false);
		addSaveButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				namespaceList.add();
			}
		});
		delSaveButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				namespaceList.del();
			}
		});

		pathLabel = new JLabel("File path");
		pathList.setPreferredSize(new Dimension(500, 200));
		namespaceList.setPreferredSize(new Dimension(500, 200));

		saveButtonPanel.setLayout(new BoxLayout(saveButtonPanel,
				BoxLayout.X_AXIS));
		saveButtonPanel.add(Box.createHorizontalGlue());
		saveButtonPanel.add(addSaveButton);
		saveButtonPanel.add(Box.createHorizontalStrut(10));
		saveButtonPanel.add(delSaveButton);
		saveButtonPanel.add(Box.createHorizontalGlue());

		pathBox = new JPanel();
		pathBox.setLayout(new BorderLayout());
		pathBox.setOpaque(false);

		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

		allowDanglingBox.setOpaque(false);
		allowDanglingBox.setAlignmentX(JComponent.CENTER_ALIGNMENT);
		pathBox.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		pathLabel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		urlLabel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		importExternalRefsBox.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		pathField.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		urlField.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		delButton.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		nsLabel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		nsField.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		nsPanel.setAlignmentX(JComponent.LEFT_ALIGNMENT);

		add(pathBox);

	}

	protected void storeProfile(OBOFileAdapter.OBOAdapterConfiguration profile) {

		profile.getReadPaths().clear();
		Vector temp = pathList.getData();
		for (int i = 0; i < temp.size(); i++)
			profile.getReadPaths().add(temp.get(i).toString());
		profile.setAllowDangling(allowDanglingBox.isSelected());
		profile.setSerializer((String) serializerBox.getSelectedItem());
		profile.setSaveRecords(namespaceList.getData());
		profile.setBasicSave(false);
	}

	public void setConfiguration(AdapterConfiguration config) {
		OBOFileAdapter.OBOAdapterConfiguration profile = (OBOFileAdapter.OBOAdapterConfiguration) config;
		currentProfile = profile;
		Vector out = new Vector();
		Iterator it = profile.getReadPaths().iterator();
		while (it.hasNext()) {
			out.add(new EditableString((String) it.next()));
		}

		pathList.setData(out);
		allowDanglingBox.setSelected(profile.getAllowDangling());
		System.err
				.println("setConfiguration called, allowDanglingBox.setSelected("
						+ profile.getAllowDangling() + ")");
		serializerBox.setSelectedItem(profile.getSerializer());

		Vector v = new Vector();
		v.addAll(profile.getSaveRecords());

		namespaceList.setData(v);
		if (v.size() > 0)
			namespaceList.select(0);
	}

	public void cleanup() {
	}

	public AdapterConfiguration createEmptyConfig() {
		return new OBOFileAdapter.OBOAdapterConfiguration();
	}

	public void init(AdapterWidgetI widget, IOOperation op,
			DataAdapter adapter, Object input) {
		this.op = op;
		currentHistory = (OBOSession) input;

		// this.op = op;
		pathBox.removeAll();
		if (op.equals(OBOAdapter.READ_ONTOLOGY)) {
			pathBox.add(pathList, "Center");
			pathBox.add(allowDanglingBox, "South");
		} else {
			pathBox.add(namespaceList, "Center");
			pathBox.add(serializerPanel, "South");
		}

		// setDefaultGUIValues(currentHistory);
		setConfiguration(createEmptyConfig());
		validate();
		repaint();
	}

	protected void collectParams() {
		paths.clear();
		namespaceList.commit();
		Vector temp = pathList.getData();
		for (int i = 0; i < temp.size(); i++)
			paths.add(temp.get(i).toString());
		storeProfile(currentProfile);
	}

	public AdapterConfiguration getConfig(IOOperation op, DataAdapter adapter,
			Object input) {
		// collectParams();
		return currentProfile;
	}

	/*
	 * protected void setDefaultGUIValues(OBOSession history) { new
	 * Exception("Someone called setDefaultGUIValues()").printStackTrace();
	 * nsmap.clear(); Vector v = new Vector(); if (history.getDefaultNamespace() !=
	 * null) { String path = history.getDefaultNamespace().getPath(); try { URL
	 * url = new URL(path); if (url.getProtocol().equals("file")) { path =
	 * url.getPath(); } } catch (MalformedURLException e) {}
	 * OBOFileAdapter.SaveProfileRecord spr = new OBOFileAdapter.
	 * SaveProfileRecord(history.getDefaultNamespace(), path, path,
	 * OBOFileSerializer.PRIMARY, true); v.add(spr);
	 * nsmap.put(spr.getNamespace(), spr); } Iterator it =
	 * history.getNamespaces().iterator(); while(it.hasNext()) { Namespace ns =
	 * (Namespace) it.next();
	 * 
	 * if (!ns.equals(history.getDefaultNamespace())) { String path =
	 * ns.getPath(); try { URL url = new URL(path); if
	 * (url.getProtocol().equals("file")) path = url.getPath(); } catch
	 * (MalformedURLException e) {} OBOFileAdapter.SaveProfileRecord spr = new
	 * OBOFileAdapter.SaveProfileRecord(ns, path, path,
	 * OBOFileSerializer.ABSORB, true); v.add(spr);
	 * nsmap.put(spr.getNamespace(), spr); } }
	 * 
	 * namespaceList.setData(v); }
	 */
	@Override
	public void setFont(Font font) {
		super.setFont(font);
		if (pathLabel != null)
			pathLabel.setFont(font);
		if (serializerBox != null)
			serializerBox.setFont(font);
		if (serializerLabel != null)
			serializerLabel.setFont(font);
		if (allowDanglingBox != null)
			allowDanglingBox.setFont(font);
		if (addSaveButton != null)
			addSaveButton.setFont(font);
		if (delSaveButton != null)
			delSaveButton.setFont(font);
		if (pathOrURLLabel != null)
			pathOrURLLabel.setFont(font);
		if (delButton != null)
			delButton.setFont(font);
		if (browseButton != null)
			browseButton.setFont(font);
		if (pathList != null)
			pathList.setFont(font);
		if (namespaceList != null)
			namespaceList.setFont(font);
		if (urlLabel != null)
			urlLabel.setFont(font);
		if (importExternalRefsBox != null)
			importExternalRefsBox.setFont(font);
		if (pathField != null)
			pathField.setFont(font);
		if (nsLabel != null)
			nsLabel.setFont(font);
		if (nsField != null)
			nsField.setFont(font);

		if (urlField != null)
			urlField.setFont(font);
	}

	public void setDefaultBrowsePath(String defaultBrowsePath) {
		this.defaultBrowsePath = defaultBrowsePath;
	}

	public void setAdvancedUI(GraphicalUI advancedUI) {
		this.advancedUI = advancedUI;
	}
}

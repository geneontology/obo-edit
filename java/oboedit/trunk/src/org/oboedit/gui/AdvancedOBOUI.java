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

import org.apache.log4j.*;

public class AdvancedOBOUI extends JPanel implements GraphicalUI {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AdvancedOBOUI.class);

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

    protected JLabel addInstruction = new JLabel("Click 'Add' to create a new save profile");

	protected OBOFileAdapter.OBOAdapterConfiguration currentProfile;

	protected JLabel pathLabel;

	protected JLabel pathOrURLLabel = new JLabel("Path or URL");

	protected JLabel urlLabel = new JLabel("URL for import");

	protected JCheckBox importExternalRefsBox = new JCheckBox(
			"Import external references");

	protected JCheckBox allowDanglingBox = new JCheckBox(
			"Allow dangling references");

	protected JTextField pathField = new JTextField(30);

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

        protected Dimension preferredSize;

    // Not yet being used--the goal is to have GraphicalAdapterChooser call this method, but it wasn't working right
    public Dimension getPreferredSize() {
//	logger.info("AdvancedOBOUI.getPreferredSize " + preferredSize); // DEL
	return preferredSize;
    }
    public void setPreferredSize(Dimension dim) {
	preferredSize = dim;
//	logger.info("AdvancedOBOUI.setPreferredSize " + preferredSize); // DEL
    }

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
					throw new DataAdapterUIException("Cannot save to empty path.");
				}
				if ((new File(path.getPath())).exists())
					overwrite.add(path.getPath());
			}
			if (overwrite.size() > 0) {
				StringBuffer out = new StringBuffer();
				out.append("The following file(s) already exist:\n");
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

		protected FilterComponent objectFilterEditor = new FilterComponent(
				new TermFilterEditorFactory());

		protected FilterComponent linkFilterEditor = new FilterComponent(
				new LinkFilterEditorFactory());

		protected JTextField pathField = new JTextField(30);

	        protected JTextArea remarkField = new JTextArea(1, 60);

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

		protected JCheckBox writeModificationBox = new JCheckBox(
				"Write modification info");

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
			Iterator it = TermUtil.getRelationshipTypes(currentHistory)
					.iterator();
			while (it.hasNext()) {
				OBOProperty property = (OBOProperty) it.next();
				prefilterTypeChooser.addItem(property);
			}

			objectFilterEditor.setPreferredSize(null);
			linkFilterEditor.setPreferredSize(null);

			objectFilterEditor.setOpaque(false);
			linkFilterEditor.setOpaque(false);
			filterBox.setOpaque(false);
			categoryBox.setOpaque(false);
			filterTypesBox.setOpaque(false);
			linkFilterBox.setOpaque(false);
			prefilterBox.setOpaque(false);

			allowDanglingBox.setOpaque(false);
			writeModificationBox.setOpaque(false);
			saveImpliedBox.setOpaque(false);
			realizeImpliedBox.setOpaque(false);

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
// 			pathPanel.setLayout(new BorderLayout());
// 			pathPanel.add(labelBox, "West");
// 			pathPanel.add(pathField, "Center");
// 			pathPanel.add(browseButton, "East");
			pathPanel.setLayout(new BoxLayout(pathPanel, BoxLayout.X_AXIS));
//			pathPanel.add(labelBox);
			pathPanel.add(pathLabel);
			pathPanel.add(pathField);
			pathPanel.add(browseButton);
			pathPanel.setOpaque(false);

			add(pathPanel, "North");

			panel.setOpaque(false);
//			panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS)); // ?
			
			Box checkboxMainPanel = new Box(BoxLayout.Y_AXIS);
			Box checkboxPanelA = new Box(BoxLayout.X_AXIS);
			Box checkboxPanelA1 = new Box(BoxLayout.X_AXIS);
			Box checkboxPanelA2 = new Box(BoxLayout.X_AXIS);
			Box checkboxPanelA3 = new Box(BoxLayout.X_AXIS);
			Box checkboxPanelB = new Box(BoxLayout.X_AXIS);
			Box checkboxPanelC = new Box(BoxLayout.X_AXIS);
//			checkboxPanelA.add(Box.createHorizontalGlue());
			checkboxPanelA.add(filterBox);
			checkboxPanelA.add(Box.createHorizontalStrut(10));
			checkboxPanelA.add(filterTypesBox);
			checkboxPanelA.add(Box.createHorizontalStrut(10));
			checkboxPanelA.add(linkFilterBox);
			checkboxPanelA.add(Box.createHorizontalStrut(10));
			checkboxPanelA.add(allowDanglingBox);
//			checkboxPanelA.add(Box.createHorizontalStrut(10));

			checkboxPanelA1.add(writeModificationBox);
			checkboxPanelA1.add(Box.createHorizontalGlue());
			// Todo: add "save creation info" checkbox to A1

			checkboxPanelA2.add(prefilterBox);
			checkboxPanelA2.add(prefilterTypeChooser);
			checkboxPanelA2.add(Box.createHorizontalStrut(10));
			checkboxPanelA2.add(categoryBox);
//			checkboxPanelA2.add(Box.createHorizontalStrut(10));

			checkboxPanelA3.add(rootAlgorithmLabel);
			checkboxPanelA3.add(Box.createHorizontalStrut(5));
			checkboxPanelA3.add(rootAlgorithmChooser);

			checkboxPanelB.add(saveImpliedBox);
			checkboxPanelB.add(Box.createHorizontalStrut(10));
			checkboxPanelB.add(impliedTypeBox);
			checkboxPanelB.add(Box.createHorizontalStrut(10));
			checkboxPanelB.add(realizeImpliedBox);

//			checkboxPanelC.add(Box.createHorizontalGlue());
			checkboxPanelC.add(new JLabel("ID rules"));
			checkboxPanelC.add(Box.createHorizontalStrut(10));
			checkboxPanelC.add(idRuleSelector);
//			checkboxPanelC.add(Box.createHorizontalGlue());

			checkboxMainPanel.add(checkboxPanelA);
			checkboxMainPanel.add(checkboxPanelA1);
			checkboxMainPanel.add(checkboxPanelA2);
			checkboxMainPanel.add(checkboxPanelA3);
			checkboxMainPanel.add(checkboxPanelB);
			checkboxMainPanel.add(checkboxPanelC);

//			JPanel southPanel = new JPanel();
//			southPanel.setLayout(new BorderLayout());
//			southPanel.add(checkboxMainPanel, "West");
//			southPanel.add(remarkScroller, "Center");
//			add(southPanel, "South");

			JPanel southPanel = new JPanel();
			southPanel.setLayout(new BorderLayout());
//			southPanel.add(checkboxMainPanel, "Center");
//			southPanel.add(remarkScroller, "South");
			southPanel.add(checkboxMainPanel, "West");
			southPanel.add(remarkScroller, "Center");
			southPanel.add(remarkScroller, "South");
//			southPanel.add(panel, "North");
			add(southPanel, "South");

//			add(checkboxMainPanel, "West");
			// "panel" is where the object/link filters go when you check those checkboxes
			add(panel, "Center");

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
//			setPreferredSize(new Dimension(400, 300));
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
			// ! Todo:  fire a resize event that GraphicalAdapterChooser could hear and respond to?
		}

		public Object createNewValue() {
			OBOSerializationEngine.FilteredPath path = new OBOSerializationEngine.FilteredPath(
					null, null, "<new save path>");
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
			objectFilterEditor.setFilter(profile.getObjectFilter());
			linkFilterEditor.setFilter(profile.getLinkFilter());

			allowDanglingBox.setSelected(profile.getAllowDangling());
			writeModificationBox.setSelected(profile.getWriteModificationData());
			saveImpliedBox.setSelected(profile.getSaveImplied());
			impliedTypeBox.setSelectedItem(profile.getImpliedType());
			realizeImpliedBox.setSelected(profile.getRealizeImpliedLinks());

			prefilterBox.setSelected(profile.getPrefilterProperty() != null);
			if (profile.getPrefilterProperty() != null) {
				OBOProperty property = (OBOProperty) SessionManager
						.getManager().getSession().getObject(
								profile.getPrefilterProperty());
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
			profile.setWriteModificationData(writeModificationBox.isSelected());
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

			if (filterBox.isSelected())
				profile.setObjectFilter(objectFilterEditor.getFilter());
			if (linkFilterBox.isSelected())
				profile.setLinkFilter(linkFilterEditor.getFilter());
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
//			setAlignmentX(JComponent.LEFT_ALIGNMENT);
			add(pathOrURLLabel);
//			pathOrURLLabel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
			JPanel temp = new JPanel();
			temp.setOpaque(false);
			temp.setLayout(new BoxLayout(temp, BoxLayout.X_AXIS));
			temp.setAlignmentX(JComponent.LEFT_ALIGNMENT);
			temp.add(pathField);
			temp.add(Box.createHorizontalStrut(10));
			temp.add(browseButton);
			add(temp, "Center");
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
		serializerBox.setMaximumSize(new Dimension(Integer.MAX_VALUE, serializerBox.getPreferredSize().height));

		serializerPanel.setOpaque(false);
		serializerPanel.setLayout(new BoxLayout(serializerPanel,
				BoxLayout.X_AXIS));
//		serializerPanel.add(Box.createHorizontalGlue());
		serializerPanel.add(serializerLabel);
		serializerPanel.add(Box.createHorizontalStrut(10));
		serializerPanel.add(serializerBox);
//		serializerPanel.add(Box.createHorizontalGlue());

		// nsPanel (with Namespace box) is not currently added to the layout!
		nsPanel.setLayout(new BoxLayout(nsPanel, BoxLayout.X_AXIS));
		nsPanel.add(nsLabel);
		nsPanel.add(Box.createHorizontalStrut(10));
		nsPanel.add(nsField);
		nsPanel.setOpaque(false);
		setOpaque(false);

		namespaceList = new ListEditor(new IOProfileEditor(), false, true,
				true, true, false);
		pathList = new ListEditor(new PathEditor(), true, true, true, true, false);
		// Not working--label doesn't change
//		pathList = new ListEditor(new PathEditor(), new JLabel("Select a path or click the Add button to create a new one"), 
//					  new Vector(), true, true, true, true, false);
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
//		pathList.setPreferredSize(new Dimension(700, 200));
//		pathList.setMinimumSize(new Dimension(200, 400));
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
//		pathBox.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		pathBox.setAlignmentX(JComponent.CENTER_ALIGNMENT);
		pathLabel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		urlLabel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		importExternalRefsBox.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		pathField.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		urlField.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		delButton.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		nsLabel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		nsField.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		nsPanel.setAlignmentX(JComponent.LEFT_ALIGNMENT);

		// Things are added to pathBox in init()
		add(Box.createHorizontalStrut(10));
		add(Box.createHorizontalGlue());
		add(pathBox);
	}

	protected void storeProfile(OBOFileAdapter.OBOAdapterConfiguration profile) {

		profile.getReadPaths().clear();
		Vector temp = pathList.getData();
		for (int i = 0; i < temp.size(); i++)
			profile.getReadPaths().add(temp.get(i).toString());
		profile.setAllowDangling(allowDanglingBox.isSelected());
		// Options in serializerBox are OBO_1_0 and OBO_1_2
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
//		System.err
//				.println("setConfiguration called, allowDanglingBox.setSelected("
//						+ profile.getAllowDangling() + ")");
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

		pathBox.removeAll();
		if (op.equals(OBOAdapter.READ_ONTOLOGY)) {
//		    setPreferredSize(new Dimension(800,550));
		    pathList.setPreferredSize(new Dimension(700, 200));
//			pathBox.add(pathList, "Center");
		        // Center puts it on the left and it ends up getting shrunken.  West seems to work better
			pathBox.add(pathList, "West");
			pathBox.add(addInstruction, "North"); // this puts it outside the bordered panel--oh well
			pathBox.add(allowDanglingBox, "South");
		} else {
//		    setPreferredSize(new Dimension(600,160));
			pathBox.add(namespaceList, "Center");
			pathBox.add(addInstruction, "South");
			pathBox.add(serializerPanel, "North");
			// Not adding namespace panel because putting something in the namespace box
			// doesn't seem to change the output.  (That is presumably a bug.)
//			pathBox.add(nsPanel, "North");
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

package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.ConfigurationPanel;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.MutableLinkDatabase;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.impl.DefaultMutableLinkDatabase;
import org.obo.datamodel.impl.FilteredLinkDatabase;
import org.obo.datamodel.impl.MaskedLinkDatabase;
import org.obo.filters.LinkFilterImpl;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.graph.BoundsGuarantor;
import org.oboedit.graph.FocusPicker;
import org.oboedit.graph.HierarchicalGraphLayout;
import org.oboedit.graph.SelectionBehavior;
import org.oboedit.graph.TooltipBehavior;
import org.oboedit.graph.ZoomToAllGuarantor;
import org.oboedit.graph.ZoomWidgetBehavior;
import org.oboedit.gui.event.ReloadEvent;
import org.oboedit.gui.event.ReloadListener;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;
import org.oboedit.util.GUIUtil;

public class DAGViewCanvas extends AbstractGUIComponent {

	public static class GraphDAGViewConfiguration implements ComponentConfiguration {
		protected boolean showAnimations = false;
		protected boolean succinctDisplay = false;
		protected boolean showPerType = true;
		protected boolean allTypes = true;
		protected boolean nonTransitive = false;
		protected int orientation = HORIZONTAL;
		
		public boolean isShowAnimations() {
			return showAnimations;
		}
		
		public void setShowAnimations(boolean showAnimations) {
			this.showAnimations = showAnimations;
		}
		
		public boolean isSuccinctDisplay() {
			return succinctDisplay;
		}
		
		public void setSuccinctDisplay(boolean succinctDisplay) {
			this.succinctDisplay = succinctDisplay;
		}
		
		public boolean isShowPerType() {
			return showPerType;
		}
		
		public void setShowPerType(boolean showPerType) {
			this.showPerType = showPerType;
		}
		
		public boolean isAllTypes() {
			return allTypes;
		}
		
		public void setAllTypes(boolean allTypes) {
			this.allTypes = allTypes;
		}
		
		public boolean isNonTransitive() {
			return nonTransitive;
		}
		
		public void setNonTransitive(boolean nonTransitive) {
			this.nonTransitive = nonTransitive;
		}

		public int getOrientation() {
			return orientation;
		}

		public void setOrientation(int orientation) {
			this.orientation = orientation;
		}
	}
	/**
	 * 
	 */
	private static final long serialVersionUID = 1221804652480622045L;
	
	public static int HORIZONTAL = 0;
	public static int VERTICAL = 1;

	protected GraphDAGViewConfiguration config = new GraphDAGViewConfiguration();
	
	protected OBOSession session;

	protected LinkDatabase linkProviderDatabase;

	protected LinkDatabase reasoner;

	protected List<LinkedObject> terms = new LinkedList<LinkedObject>();

	protected JPanel dagPanel = new JPanel();

	protected JLabel topLabel = new JLabel();

	protected JCheckBox showAnimations = new JCheckBox("Animate", false);
	protected JCheckBox succinctCheckbox = new JCheckBox("Succinct", true);
	protected JCheckBox showBreakdownBox = new JCheckBox(
			"Show per-type panels", true);
	protected JCheckBox allTypesBox = new JCheckBox("Show all types panel",
			true);
	protected JCheckBox nonTransitiveBox = new JCheckBox(
			"Show non-transitive types", false);

	protected static final String[] orientations = { "horizontal", "vertical" };

	protected JComboBox orientationChooser = new JComboBox(orientations);

	protected ConfigurationPanel configPanel = new ConfigurationPanel() {

		@Override
		public void commit() {
			commitConfig();
		}

		@Override
		public void init() {
			initConfig();
		}

	};
	
	public DAGViewCanvas(String id) {
		super(id);
		configPanel.setLayout(new BoxLayout(configPanel, BoxLayout.Y_AXIS));
		configPanel.add(createPanel(allTypesBox));
		configPanel.add(createPanel(showAnimations));
		configPanel.add(createPanel(nonTransitiveBox));
		configPanel.add(createPanel(showBreakdownBox));
		configPanel.add(createPanel(succinctCheckbox));
		Box orientationPanel = Box.createHorizontalBox();
		orientationPanel.add(new JLabel("Panel orientation"));
		orientationPanel.add(Box.createHorizontalStrut(10));
		orientationPanel.add(orientationChooser);
		orientationPanel.add(Box.createHorizontalGlue());
		configPanel.add(orientationPanel);
		configPanel.add(Box.createVerticalGlue());
	}
	
	protected static JComponent createPanel(JComponent c) {
		Box out = Box.createHorizontalBox();
		out.add(c);
		out.add(Box.createHorizontalGlue());
		return out;
	}
	
	protected void commitConfig() {
		config.setAllTypes(allTypesBox.isSelected());
		config.setShowAnimations(showAnimations.isSelected());
		config.setNonTransitive(nonTransitiveBox.isSelected());
		config.setShowPerType(showBreakdownBox.isSelected());
		config.setSuccinctDisplay(succinctCheckbox.isSelected());
		config.setOrientation(orientationChooser.getSelectedIndex());
		reload();
	}
	
	protected void initConfig() {
		allTypesBox.setAlignmentX(0);
		showAnimations.setAlignmentX(0);
		allTypesBox.setSelected(config.isAllTypes());
		showAnimations.setSelected(config.isShowAnimations());
		nonTransitiveBox.setSelected(config.isNonTransitive());
		showBreakdownBox.setSelected(config.isShowPerType());
		succinctCheckbox.setSelected(config.isSuccinctDisplay());
		orientationChooser.setSelectedIndex(config.getOrientation());
	}

	public void setDataProviders(OBOSession session,
			LinkDatabase linkProviderDatabase, LinkDatabase reasoner) {
		this.linkProviderDatabase = linkProviderDatabase;
		this.reasoner = reasoner;
		this.session = session;
	}

	public void setTerms(Collection<LinkedObject> t) {
		this.terms.clear();
		if (t != null)
			this.terms.addAll(t);
		reload();
	}

	protected LinkDatabaseCanvas getCanvas(OBOProperty type,
			Collection<Link> parents, boolean succinct) {
		LinkDatabaseCanvas canvas = new LinkDatabaseCanvas(
				new HierarchicalGraphLayout()) {

			protected void addDefaultBehaviors() {
				addViewBehavior(new FocusPicker());
				addViewBehavior(new SelectionBehavior());
				// addViewBehavior(new LinkoutMeterBehavior());
				addViewBehavior(new TooltipBehavior());
				addViewBehavior(new ZoomWidgetBehavior(8,20));
				addViewBehavior(new BoundsGuarantor() {
					@Override
					protected void installDefaultCyclers() {
						addBoundsGuarantor(new ZoomToAllGuarantor(canvas));
					}
				});
			}

		};
		canvas.setDisableAnimations(!config.isShowAnimations());

		LinkDatabase linkDatabase;
		Collection<LinkedObject> objects = new LinkedList<LinkedObject>();
		if (!succinct) {
			MutableLinkDatabase mutable = new DefaultMutableLinkDatabase(true);

			for (LinkedObject object : objects) {
				mutable.addObject(object);
			}

			for (Link link : parents) {
				if (TermUtil.isImplied(link)) {
					Collection<Link> implied = ReasonerUtil
							.getGivenSupportingLinks(
									(ReasonedLinkDatabase) reasoner, link);
					for (Link backingLink : implied) {
						mutable.addParent(backingLink);
					}
				} else
					mutable.addParent(link);
			}
			for (IdentifiedObject io : mutable.getObjects())
				if (io instanceof LinkedObject)
					objects.add((LinkedObject) io);
			linkDatabase = mutable;
		} else {
			FilteredLinkDatabase filtered = new FilteredLinkDatabase(reasoner);
			filtered.setLinkFilter(new LinkFilterImpl(type));
			MaskedLinkDatabase collapsible = new MaskedLinkDatabase(filtered);
			objects.addAll(terms);
			for (Link link : parents) {
				objects.add(link.getParent());
			}
			collapsible.setVisible(objects, true);
			linkDatabase = collapsible;
		}
		// canvas.setLinkDatabase(reasoner);
		canvas.setLinkDatabase(linkDatabase);
		canvas.setLinkProviderDatabase(linkProviderDatabase);
		canvas.setVisibleObjects(objects);
		canvas.setLive(false);
		canvas.relayout();
		return canvas;
	}
	
	@Override
	public ConfigurationPanel getConfigurationPanel() {
		return configPanel;
	}

	protected static JPanel wrapCanvas(String title, LinkDatabaseCanvas canvas) {
		JPanel panel = new JPanel();
		panel.setOpaque(true);
		panel.setBackground(Color.white);
		panel.setLayout(new GridLayout(1, 1));
		panel.add(canvas);
		panel.setBorder(new TitledBorder(title));
		return panel;
	}

	public void reload() {

		dagPanel.removeAll();
		if (terms.size() == 0) {
			topLabel.setText("Select a term to see ancestry views");
		} else {
			StringBuffer buffer = new StringBuffer("Ancestor views of ");
			for (int i = 0; i < terms.size(); i++) {
				if (i > 0 && terms.size() > 2)
					buffer.append(", ");
				if (i == terms.size() - 1 && terms.size() > 1)
					buffer.append("and ");
				buffer.append(terms.get(i).getName());
			}
			topLabel.setText(buffer.toString());
			Collection<LinkDatabaseCanvas> canvasList = new LinkedList<LinkDatabaseCanvas>();
			Collection<JComponent> componentList = new LinkedList<JComponent>();

			if (config.isAllTypes()) {
				Collection<Link> parents = new HashSet<Link>();
				for (LinkedObject lo : terms) {
					for (Link link : reasoner.getParents(lo)) {
						if (config.isNonTransitive()
								|| link.getType().isTransitive())
							parents.add(link);
					}
				}
				LinkDatabaseCanvas canvas = getCanvas(null, parents, false);
				JPanel panel = wrapCanvas("All parents", canvas);
				canvasList.add(canvas);
				componentList.add(panel);
			}

			if (config.isShowPerType()) {
				Map<OBOProperty, Collection<Link>> typeMap = getTypeMap();

				for (OBOProperty type : typeMap.keySet()) {
					if (!config.isNonTransitive() && !type.isTransitive())
						continue;
					Collection<Link> parents = typeMap.get(type);
					LinkDatabaseCanvas canvas = getCanvas(type, parents,
							config.isSuccinctDisplay());
					JPanel panel = wrapCanvas(type.getID(), canvas);
					canvasList.add(canvas);
					componentList.add(panel);
				}
			}

			if (config.getOrientation() == HORIZONTAL)
				dagPanel.setLayout(new GridLayout(1, canvasList.size()));
			else
				dagPanel.setLayout(new GridLayout(canvasList.size(), 1));
			
			for (JComponent c : componentList) {
				dagPanel.add(c);
			}
			dagPanel.validate();
			validate();
			repaint();
			for (LinkDatabaseCanvas c : canvasList) {
				c.zoomToObjects();
			}
		}
	}

	public Map<OBOProperty, Collection<Link>> getTypeMap() {
		Map<OBOProperty, Collection<Link>> map = new HashMap<OBOProperty, Collection<Link>>();
		if (reasoner == null)
			return map;
		for (LinkedObject term : terms) {
			for (Link link : reasoner.getParents(term)) {
				Collection<Link> objects = map.get(link.getType());
				if (objects == null) {
					objects = new LinkedList<Link>();
					map.put(link.getType(), objects);
				}
				objects.add(link);
			}
		}
		return map;
	}

	public void cleanup() {
		GUIUtil.removeReloadListener(reloadListener);
		SelectionManager.getManager().removeSelectionListener(selectListener);
	}

	public JComponent getComponent() {
		return this;
	}

	public ComponentConfiguration getConfiguration() {
		return config;
	}
	
	protected ReloadListener reloadListener = new ReloadListener() {
		public void reload(ReloadEvent e) {
			updateProviders();
		}
	};

	protected SelectionListener selectListener = new SelectionListener() {
		public void selectionChanged(SelectionEvent e) {
			setTerms(e.getSelection().getTerms());
		}
	};

	protected void updateProviders() {
		OBOSession session = SessionManager.getManager().getSession();
		setDataProviders(session, session.getLinkDatabase(), SessionManager
				.getManager().getCurrentFullLinkDatabase());
		reload();
	}

	public void init() {
		removeAll();
		
		setLayout(new BorderLayout());
		setBackground(Color.white);
		dagPanel.setOpaque(false);
		add(dagPanel, "Center");
		add(topLabel, "North");
		GUIUtil.addReloadListener(reloadListener);
		SelectionManager.getManager().addSelectionListener(selectListener);
		updateProviders();
		setTerms(SelectionManager.getGlobalSelection().getTerms());
	}

	public boolean isSingleton() {
		return false;
	}

	public void setConfiguration(ComponentConfiguration config) {
		if (config == null)
			config = new GraphDAGViewConfiguration();
		this.config = (GraphDAGViewConfiguration) config;
	}
	
	public void setXML(String xml) {
	}

	public boolean isXMLSettable() {
		return false;
	}
}

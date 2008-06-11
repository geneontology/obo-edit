package org.bbop.framework.dock.idw;

import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ContainerEvent;
import java.awt.event.ContainerListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.XMLDecoder;
import java.beans.XMLEncoder;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.SwingUtilities;

import net.infonode.docking.DefaultButtonFactories;
import net.infonode.docking.DockingWindow;
import net.infonode.docking.DockingWindowAdapter;
import net.infonode.docking.DockingWindowListener;
import net.infonode.docking.FloatingWindow;
import net.infonode.docking.OperationAbortedException;
import net.infonode.docking.RootWindow;
import net.infonode.docking.SplitWindow;
import net.infonode.docking.TabWindow;
import net.infonode.docking.View;
import net.infonode.docking.internal.ViewTitleBar;
import net.infonode.docking.properties.RootWindowProperties;
import net.infonode.docking.theme.DockingWindowsTheme;
import net.infonode.docking.theme.ShapedGradientDockingTheme;
import net.infonode.docking.util.StringViewMap;
import net.infonode.gui.colorprovider.FixedColorProvider;
import net.infonode.tabbedpanel.Tab;
import net.infonode.tabbedpanel.TabbedPanel;
import net.infonode.tabbedpanel.titledtab.TitledTab;
import net.infonode.util.Direction;

import org.bbop.framework.ComponentManager;
import org.bbop.framework.ConfigurationPanel;
import org.bbop.framework.FrameworkUtil;
import org.bbop.framework.GUIComponent;
import org.bbop.framework.GUIComponentFactory;
import org.bbop.framework.GUIManager;
import org.bbop.framework.HelpManager;
import org.bbop.framework.dock.LayoutDriver;
import org.bbop.framework.dock.LayoutListener;
import org.bbop.framework.dock.Perspective;
import org.bbop.io.FileUtil;
import org.bbop.io.IOUtil;
import org.bbop.swing.SwingUtil;
import org.bbop.util.ObjectUtil;

import org.apache.log4j.*;

public class IDWDriver implements LayoutDriver {

	//initialize logger
	protected final static Logger logger = Logger.getLogger("IDWDriver.class");

	protected StringViewMap viewMap;

	protected RootWindow rootWindow;

	protected DockingWindowsTheme currentTheme = new ShapedGradientDockingTheme();
	
	protected DockingWindowsTheme customTheme = null;
	
	protected RootWindowProperties properties = new RootWindowProperties();

	protected List<Perspective> perspectives = new LinkedList<Perspective>();

	protected Perspective currentPerspective;

	protected Color background = Color.lightGray;

	protected Font font = new Font("Arial", 0, 10);

	protected Color darkColor = Color.blue;

	protected Color lightColor = Color.white;

	protected Collection<ViewListener> viewListeners = new LinkedList<ViewListener>();

	protected String perspectiveResourceDir;

	protected String defaultPerspectiveResourcePath;

	protected String perspectiveListResourcePath;

	protected Collection<LayoutListener> layoutListeners = new ArrayList<LayoutListener>();

  /** whether to save layout when exiting - default is true */
  private boolean saveLayoutOnExit = true;

	protected DockingWindowListener floatingWindowCloseListener = new DockingWindowAdapter() {
		public void windowClosed(DockingWindow window) {
			View v = SwingUtil.getDescendantOfType(window, View.class);
			if (v != null)
				destroyView(v);
		};
		
		@Override
		public void windowClosing(DockingWindow window)
				throws OperationAbortedException {
			View v = SwingUtil.getDescendantOfType(window, View.class);
			GUIComponent c = getComponent(v);
			if (c != null) {
				for (LayoutListener listener : layoutListeners) {
					if (!listener.closing(c))
						throw new OperationAbortedException();
				}
			}
		}
	};

	protected DockingWindowListener masterDockingListener = new DockingWindowListener() {

		public void viewFocusChanged(View old, View newView) {
			GUIComponent oldC = getComponent(old);
			GUIComponent newC = getComponent(newView);
			for (LayoutListener listener : layoutListeners) {
				listener.focusChanged(oldC, newC);
			}
			if (newView != null)
				lastComponent = newView;
		}

		public void windowAdded(DockingWindow parent, DockingWindow child) {
			GUIComponent parentC = getComponent(parent);
			GUIComponent childC = getComponent(child);
			if (childC != null) {
				for (LayoutListener listener : layoutListeners) {
					listener.add(parentC, childC);
				}
			}
			if (child != null && child instanceof View)
			lastComponent = (View) child;
		}

		public void windowClosed(DockingWindow d) {
			View v = SwingUtil.getDescendantOfType(d, View.class);
			if (v != null)
				destroyView(v);
		}

		public void windowUndocked(DockingWindow d) {
			FloatingWindow floater = SwingUtil.getAncestorOfClass(
					FloatingWindow.class, d);
			floater.addListener(floatingWindowCloseListener);
		}

		public void windowDocked(DockingWindow d) {
		}

		public void windowMaximized(DockingWindow d) {
		}

		public void windowMinimized(DockingWindow d) {
		}

		public void windowRestored(DockingWindow d) {
		}

		public void windowHidden(DockingWindow arg0) {
		}

		public void windowClosing(DockingWindow d)
				throws OperationAbortedException {
			GUIComponent c = getComponent(d);
			if (c != null) {
				for (LayoutListener listener : layoutListeners) {
					if (!listener.closing(c))
						throw new OperationAbortedException();
				}
			}
		}

		public void windowDocking(DockingWindow d)
				throws OperationAbortedException {
			GUIComponent c = getComponent(d);
			if (c != null) {
				for (LayoutListener listener : layoutListeners) {
					if (!listener.docking(c))
						throw new OperationAbortedException();
				}
			}
		}

		public void windowMaximizing(DockingWindow d)
				throws OperationAbortedException {
			GUIComponent c = getComponent(d);
			if (c != null) {
				for (LayoutListener listener : layoutListeners) {
					if (!listener.maximizing(c))
						throw new OperationAbortedException();
				}
			}

		}

		public void windowMinimizing(DockingWindow d)
				throws OperationAbortedException {
			GUIComponent c = getComponent(d);
			if (c != null) {
				for (LayoutListener listener : layoutListeners) {
					if (!listener.minimizing(c))
						throw new OperationAbortedException();
				}
			}
		}

		public void windowRestoring(DockingWindow d)
				throws OperationAbortedException {
			GUIComponent c = getComponent(d);
			if (c != null) {
				for (LayoutListener listener : layoutListeners) {
					if (!listener.restoring(c))
						throw new OperationAbortedException();
				}
			}
		}

		public void windowShown(DockingWindow d) {
		}

		public void windowUndocking(DockingWindow d)
				throws OperationAbortedException {
			GUIComponent c = getComponent(d);
			if (c != null) {
				for (LayoutListener listener : layoutListeners) {
					if (!listener.undocking(c))
						throw new OperationAbortedException();
				}
			}
		}

		public void windowRemoved(DockingWindow arg0, DockingWindow arg1) {
		}
	};

	public IDWDriver() {
	}

	public void addLayoutListener(LayoutListener listener) {
		layoutListeners.add(listener);
	}

	public void removeLayoutListener(LayoutListener listener) {
		layoutListeners.remove(listener);
	}

	public void addViewListener(ViewListener listener) {
		viewListeners.add(listener);
	}

	public void removeViewListener(ViewListener listener) {
		viewListeners.remove(listener);
	}

	protected void fireCreatedView(View v, GUIComponent c) {
		for (ViewListener listener : viewListeners) {
			listener.viewCreated(v, c);
		}
	}

	protected void fireDestroyedView(View v, GUIComponent c) {
		for (ViewListener listener : viewListeners) {
			listener.viewDestroyed(v, c);
		}
	}

  /** cleanup is called on exitin and changing layout managers (though there currently
      is only 1 layout mgt - this - and saves all perspectives,
      unless saveLayoutOnExit is set to false (default true) */
	public void cleanup() {
		if (saveLayoutOnExit && currentPerspective != null) {
			savePerspectiveAs(currentPerspective, currentPerspective.getName());
			//savePerspectives(); // need to keep as it tracks which perspective is current
		}
		if (currentPerspective != null)
			savePerspectives(); // need to keep as it tracks which perspective is current
	}

  /** Whether to save layout on exit - default true */
  public void setSaveLayoutOnExit(boolean saveLayout) {
    this.saveLayoutOnExit = saveLayout;
  }
  	
  	/** Saves known list of perspectives and current perspective to 
  	 * 	perpectives.xml
  	 */
	public void savePerspectives() {
		File file = getPerspectivesFile();
		XMLEncoder encoder;
		try {
			encoder = new XMLEncoder(new BufferedOutputStream(
					new FileOutputStream(file)));
			encoder.writeObject(perspectives);
			encoder.writeObject(currentPerspective);
			encoder.close();
		} catch (IOException e) {
			logger.warn("savePerspectives: exception trying to write perspectives file " + file);
			e.printStackTrace();
		}
	}

	public GUIComponent createMainPanel(String id) {
		return new DockPanel(id, this);
	}

	public boolean deletePerspective(Perspective p) {
		if (p.getBuiltIn())
			return false;
		File perspectiveFile = getFile(p);
		int index = perspectives.indexOf(p);
		if (index >= 0) {
			perspectives.remove(index);
			perspectiveFile.delete();
			if (p.equals(currentPerspective)) {
				if (index >= perspectives.size())
					index = 0;
				setPerspective(perspectives.get(index));
			}
			return true;
		}
		return false;
	}

	public Perspective getCurrentPerspective() {
		return currentPerspective;
	}

	public Perspective getPerspective(String name) {
		for (Perspective p : perspectives) {
			if (p.getName().equalsIgnoreCase(name))
				return p;
		}
		return null;
	}

	public List<Perspective> getPerspectives() {
		return perspectives;
	}

	protected static File getFile(Perspective perspective) {
		return getFile(perspective.getID());
	}

	protected static File getFile(String name) {
		return new File(getPerspectivesDir(), name.toLowerCase() + ".idw");
	}

	protected static File getPerspectivesDir() {
		return new File(GUIManager.getPrefsDir(), "perspectives");
	}

  /** this is the name of the file that will get written in user prefs,
      NOT the resource that gets read from jar/classfiles */
	protected static File getPerspectivesFile() {
		return new File(getPerspectivesDir(), "perspectives.xml");
	}

    /** load perspectives from jar/classes to user prefs perspective dir */
    @SuppressWarnings("unchecked")
	protected void loadPerspectives() {
	// this is the file to copy to
	File toFile = getPerspectivesFile();
	if (getPerspectiveListResourcePath() != null) {
	    // if toFile doesnt exist then copy resource to it, throws file not found
			try {
				FileUtil.ensureExists(toFile, getPerspectiveListResourcePath());
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				logger.warn("loadPerspectives: exception trying to create perspectives file " + toFile + " from resource" + getPerspectiveListResourcePath());
				e1.printStackTrace();
			}
		}

	// get perspectives from toFile perspective.xml (listed in there)
		if (toFile.exists()) {
			XMLDecoder d;
			perspectives = null;
			Perspective currentPerspective = null;
			try {
				d = new XMLDecoder(new BufferedInputStream(new FileInputStream(
                                                     toFile)));
				perspectives = (List<Perspective>) d.readObject();
				currentPerspective = (Perspective) d.readObject();
				d.close();
			}
			catch (Exception e) {
				logger.warn("loadPerspectives: exception trying to read perspectives file " + toFile);
				e.printStackTrace();
			}

			if (perspectives == null)
				perspectives = new LinkedList<Perspective>();
			else {
				// delete any nulls or bad data that might have been added
				// during a
				// failed
				// deserialization
				Iterator<Perspective> it = perspectives.iterator();
				while (it.hasNext()) {
					Perspective p = it.next();
					if (p == null)
						it.remove();
					else {
						try {
							InputStream s = getInputStream(getFile(p)
									.getAbsolutePath());
							s.close();
						} catch (Throwable t) {
							it.remove();
						}
					}
				}
			}
			// reads in perspecteves from perpspectives file
			createDefaultPerspectives();
			// set perspective to 1st perspective
			if (currentPerspective == null && perspectives.size() > 0)
				currentPerspective = perspectives.get(0);
			if (currentPerspective != null)
				setPerspective(currentPerspective);
		}
	}

	protected void createDefaultPerspectives() {
		try {
			FileUtil.ensureExists(getPerspectivesFile(),
					getPerspectiveListResourcePath());
			XMLDecoder d = new XMLDecoder(new BufferedInputStream(
					new FileInputStream(getPerspectivesFile())));
			List<Perspective> defaultPerspectiveList = (List<Perspective>) d
					.readObject();
			for (Perspective p : defaultPerspectiveList) {
				if (!perspectives.contains(p))
					perspectives.add(p);
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			logger.warn("createDefaultPerspectives: exception trying to create default perspectives file " + getPerspectivesFile() + " from resource" + getPerspectiveListResourcePath());
			e.printStackTrace();
		}
	}

  private static final Logger LOG =  Logger.getLogger("org.bbop.framework.dock.idw");

  /** check if resource/persepctive is findable from class loader in perspective
      resource dir. try 3 different classloaders, system class loader,
      class.getResource, and class.getClassLoader.getResource which can give different
      results (odd). the last is what works for webstart (for phenote).
      if not found at all return defaultPerspective which hopefully works
      but obviously is probably a different perspective than whats passed in */
	protected String getResource(Perspective perspective) {
		String out = perspectiveResourceDir + "/" + perspective.getID()
			+ ".idw";
		boolean classLoaderCanFind =
			ClassLoader.getSystemClassLoader().getResource(out) != null;
		if (!classLoaderCanFind) {
			// IDWDriver.class is a different class loader than ClassLoader.getSysCL
			// and even different than IDWD.class.getClassLoader()????
			classLoaderCanFind = IDWDriver.class.getResource(out) != null;
			if (!classLoaderCanFind) // this is what works in webstart (???)
				classLoaderCanFind = IDWDriver.class.getClassLoader().getResource(out) != null;
		}
		if (!classLoaderCanFind) {
//			LOG.info("IDWDriver.getResource: can't find resource "+out+" using default perspective resource path "
//				 +defaultPerspectiveResourcePath + " and resource dir " + perspectiveResourceDir);
			out = defaultPerspectiveResourcePath;
		}
		return out;
	}

	public void setPerspective(Perspective perspective) {
		if (currentPerspective != null) {
			savePerspectiveAs(currentPerspective, currentPerspective.getName());
		}
		try {
			FileUtil.ensureExists(getFile(perspective),
					getResource(perspective));
			File file = getFile(perspective);
			if (file.exists()) {
				String path = file.getAbsolutePath();
				restorePerspectiveFromDisk(path);
			}
			currentPerspective = perspective;
		} catch (Exception e) {
			logger.warn("setPerspective: exception trying to restore perspective from " + getFile(perspective));
			e.printStackTrace();
			if (currentPerspective == null && perspectives.size() > 0) {
				currentPerspective = perspectives.get(0);
			} else
				currentPerspective = null;
		}
	}

	protected void restorePerspectiveFromDisk(String path)
			throws FileNotFoundException, IOException {

		Collection<GUIComponent> comps = new LinkedList<GUIComponent>(
				ComponentManager.getManager().getActiveComponents());
		for (GUIComponent c : comps) {
			View v = viewMap.getView(c.getID());
			if (v != null)
				v.removeListener(masterDockingListener);
			if (v != null)
				destroyView(v);
//			else
//				logger.info("COULD NOT DESTROY VIEW: " + c);
		}

		ObjectInputStream stream = getInputStream(path);
		rootWindow.read(stream, false);
		stream.close();
		Collection<String> deadViews = new ArrayList<String>();
		for (int i = 0; i < viewMap.getViewCount(); i++) {
			View v = viewMap.getViewAtIndex(i);
			if (v instanceof DefaultViewSerializer.GarbageView) {
			        logger.info("View #" + i + ", "  + v + ", is a garbage view--destroying"); 
				destroyView(v);
				i--;  // there's now one fewer thing in viewMap--don't miss the next one.
				deadViews.add(getComponent(v).getID());
			}
		}
		for (String id : deadViews)
			viewMap.removeView(id);
	}

	protected ObjectInputStream getInputStream(String path) throws IOException {
		return new ObjectInputStream(new BufferedInputStream(
				new FileInputStream(path)));
	}

	protected ObjectOutputStream getOutputStream(String path)
			throws IOException {
		return new ObjectOutputStream(new BufferedOutputStream(
				new FileOutputStream(path)));
	}

	public void init() {
		viewMap = new StringViewMap();
		rootWindow = new RootWindow(new DefaultViewSerializer(viewMap, this));
		// rootWindow = DockingUtil.createRootWindow(new DefaultViewSerializer(
		// this));
		rootWindow.getWindowBar(Direction.DOWN).setEnabled(true);
		properties.addSuperObject(currentTheme.getRootWindowProperties());
		rootWindow.getRootWindowProperties().addSuperObject(properties);

		if (customTheme == null) {
		    customTheme = new BBOPDockingTheme(new FixedColorProvider(
				getDarkColor()), new FixedColorProvider(getLightColor()),
				new FixedColorProvider(getBackground()), 4, getFont());
		}
		setTheme(customTheme);
		loadPerspectives();
	}

	protected void configureTheme() {
		if (currentTheme instanceof BBOPDockingTheme) {
			((BBOPDockingTheme) currentTheme).configure(getDarkColor(),
					getLightColor(), getBackground(), 4, getFont());
		}
	}

	public void setTheme(DockingWindowsTheme theme) {
		properties.getMap().clear(true);
		properties.replaceSuperObject(currentTheme.getRootWindowProperties(),
				theme.getRootWindowProperties());
		properties.getTabWindowProperties().getTabbedPanelProperties()
				.setTabAreaOrientation(Direction.UP);
		currentTheme = theme;
		configureTheme();
	}
	
	public void setCustomTheme(DockingWindowsTheme theme) {
	    customTheme = theme;
	}

	public RootWindow getRootWindow() {
		return rootWindow;
	}

	public Perspective savePerspectiveAs(Perspective p, String name) {
		Perspective result;
		try {
			if (currentPerspective == null || currentPerspective.equals(p)) {
				getFile(name).getParentFile().mkdirs();
				ObjectOutputStream out = getOutputStream(getFile(name)
						.getAbsolutePath());
				rootWindow.write(out, false);
				out.close();
				if (currentPerspective == null
						|| !currentPerspective.getName().equals(name)) {
					result = new Perspective(name.toLowerCase(), name);
					perspectives.add(result);
					currentPerspective = result;
				} else {
					result = currentPerspective;
				}
			} else {
				IOUtil.copyFile(getFile(p), getFile(name));
				result = new Perspective(name.toLowerCase(), name);
				perspectives.add(result);
			}
		} catch (IOException ex) {
			result = null;
		}
		return result;
	}

	protected DockingWindow lastComponent;

	protected String getDefaultIDSuffix() {
		return "main";
	}

	public View createView(final GUIComponentFactory factory, String id,
			String label) {
		if (id == null)
			id = factory.getID() + ":" + getDefaultIDSuffix();
		View tempView = viewMap.getView(id);
		if (tempView != null && tempView.getWindowParent() == null) {
			return tempView;
		}
		final GUIComponent c = ComponentManager.getManager().createComponent(
				factory, null);
		ComponentConfigCard card = new ComponentConfigCard(c);
		if (label == null)
			label = c.getTitle();
		final String flabel = label;
		final View v = new View(label, null, (JComponent) card);
		v.addListener(masterDockingListener);
		v.addListener(new DockingWindowAdapter() {

			@Override
			public void windowAdded(DockingWindow addedToWindow,
					DockingWindow addedWindow) {
				v.getViewProperties().setTitle(flabel);
				lastComponent = addedWindow;
			}
		});
		DefaultButtonFactories.getCloseButtonFactory();
		final JToggleButton configButton = IDWUtil
				.createFlatHighlightToggleButton(wrenchIcon,
						"Configure this component", 0, null);
		updateConfigButton(configButton);
		configButton.addActionListener(new AbstractAction() {

			public void actionPerformed(ActionEvent e) {
				updateConfigButton(configButton);
				configureComponent(configButton, v);
			}
		});
		if (c.getConfigurationPanel() == null)
			configButton.setEnabled(false);
		if (c.getConfigurationPanel() != null) {
			v.getCustomTitleBarComponents().add(configButton);
			// Also add to tab in case theme is not using titlebar style
			v.getCustomTabComponents().add(configButton);
		}
		if (factory.getHelpTopicID() != null) {
			try {
				if (HelpManager.getManager().isEnabled()) {
					final JToggleButton helpButton = IDWUtil
							.createFlatHighlightToggleButton(questionIcon,
									"Display help for this component", 0, null);
					helpButton.addActionListener(new ActionListener() {

						public void actionPerformed(ActionEvent e) {
							HelpManager.getManager().displayHelp(helpButton,
									factory.getHelpTopicID());
						}

					});
					v.getCustomTitleBarComponents().add(helpButton);
				}
			} catch (Throwable t) {

			}

		}
		final MouseListener listener = new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (SwingUtilities.isRightMouseButton(e)) {
					ViewTitleBar bar = (ViewTitleBar) e.getComponent();
					StringBuffer buffer = new StringBuffer();
					buffer.append("View");
					java.awt.Component p = v.getParent();
					while (p != null) {
						if (p instanceof DockingWindow) {
							Collection comps = SwingUtil.getAllDescendants(
									(Container) p, Tab.class);
							buffer.append(" -> " + p.getClass());

						}
						if (p != null
								&& p instanceof net.infonode.tabbedpanel.Tab) {
						}
						p = p.getParent();
					}
					EditorField field = new EditorField(v, bar);
					field.install();
				}
			}
		};
		v.addContainerListener(new ContainerListener() {

			public void componentAdded(ContainerEvent e) {
				if (e.getChild() instanceof ViewTitleBar) {
					ViewTitleBar bar = (ViewTitleBar) e.getChild();
					bar.addMouseListener(listener);
				}
			}

			public void componentRemoved(ContainerEvent e) {
				if (e.getChild() instanceof ViewTitleBar) {
					ViewTitleBar bar = (ViewTitleBar) e.getChild();
					bar.removeMouseListener(listener);
				}
			}

		});

		return v;
	}

	protected class EditorField extends JTextField {
		protected View view;
		protected ViewTitleBar bar;

		public EditorField(View view, ViewTitleBar bar) {
			setMaximumSize(new Dimension(Integer.MAX_VALUE,
					(int) getMaximumSize().getHeight()));
			this.bar = bar;
			this.view = view;
			setFont(bar.getLabel().getFont());
			setOpaque(false);
			setText(bar.getText());
			addActionListener(new ActionListener() {

				public void actionPerformed(ActionEvent e) {
					commit();
				}
			});
			addFocusListener(new FocusListener() {

				public void focusGained(FocusEvent e) {
				}

				public void focusLost(FocusEvent e) {
					commit();
				}

			});
		}

		public void install() {
			JComponent[] comps = { this };
			bar.setLeftTitleComponents(comps);
			view.getViewProperties().getViewTitleBarProperties()
					.getNormalProperties().setTitleVisible(false);
			// int width = Integer.MAX_VALUE;
			// for (Component c : bar.getComponents()) {
			// if (c != this && c != bar.getLabel() && c.isVisible())
			// if (c.getX() < width)
			// width = c.getX();
			// }
			// setPreferredSize(new Dimension(width, (int) getPreferredSize()
			// .getHeight()));
			requestFocus();
		}

		public void commit() {
			if (IDWDriver.getComponent(view) != null)
				setComponentLabel(IDWDriver.getComponent(view), getText());
			bar.setLeftTitleComponents(new JComponent[0]);
		}
	}

	protected void updateConfigButton(JToggleButton configButton) {
		if (configButton.isSelected()) {
			configButton.setIcon(checkIcon);
			configButton
					.setToolTipText("Complete configuration and display component");
		} else {
			configButton.setIcon(wrenchIcon);
			configButton.setToolTipText("Configure this component");
		}
	}

	public static GUIComponent getComponent(DockingWindow d) {
		if (d instanceof View) {
			return getComponent((View) d);
		}
		return null;
	}

	public static GUIComponent getComponent(View v) {
		if (v == null || v.getComponent() == null)
			return null;
		return ((ComponentConfigCard) v.getComponent()).getComponent();
	}

	protected void addView(View v) {
		GUIComponent c = ((ComponentConfigCard) v.getComponent())
				.getComponent();
		if (c.teardownWhenHidden() || !viewMap.contains(v)) {
			viewMap.addView(c.getID(), v);
			ComponentManager.getManager().addActiveComponent(c);
		}
		fireCreatedView(v, c);
	}

	protected void destroyView(View v) {
		GUIComponent c = ((ComponentConfigCard) v.getComponent())
				.getComponent();
		if (ObjectUtil.equals(v, lastComponent)) {
			lastComponent = lastComponent.getWindowParent();
		}
		if (v.getWindowParent() != null)
			v.close();
		if (c.teardownWhenHidden()) {
			ComponentManager.getManager().removeActiveComponent(c);
			rootWindow.removeView(v);
			viewMap.removeView(c.getID());
		}
		fireDestroyedView(v, c);
	}

	public View getView(GUIComponent c) {
		return viewMap.getView(c.getID());
	}

	public Collection<View> getAllViews() {
		List<View> out = new LinkedList<View>();
		for (GUIComponent c : ComponentManager.getManager()
				.getActiveComponents()) {
			out.add(getView(c));
		}
		return out;
	}

	public String showComponent(GUIComponentFactory factory,
			GUIComponent target, String name, String label,
			boolean preferFloat, Rectangle floatRect) {
		View v = createView(factory, name, label);
		GUIComponent c = ((ComponentConfigCard) v.getComponent())
				.getComponent();

		boolean restoringFromCache = viewMap.contains(v);
		addView(v);
		addWindow(v, target, restoringFromCache, preferFloat, floatRect);
		return c.getID();
	}

	protected static Rectangle getFloatRectangle(View window) {
		Dimension d = window.getComponent().getPreferredSize();
		Frame f = GUIManager.getManager().getFrame();
		int x = (int) (f.getX() + (f.getWidth() - d.getWidth()) / 2);
		int y = (int) (f.getY() + (f.getHeight() - d.getHeight()) / 2);
		return new Rectangle(x, y, d.width, d.height);
	}

	protected void addWindow(View window, GUIComponent targetComponent,
			boolean tryRestore, boolean preferFloat, Rectangle floatRect) {
		if (rootWindow == null)
			return;
		if (tryRestore)
			window.restore();
		if (window.getWindowParent() == null) {
			if (preferFloat) {
				if (floatRect == null) {
					floatRect = getFloatRectangle(window);
				}
				FloatingWindow w = rootWindow.createFloatingWindow(floatRect
						.getLocation(), floatRect.getSize(), window);
				w.addListener(floatingWindowCloseListener);
				w.getTopLevelAncestor().setVisible(true);
			} else {
				DockingWindow w;
				DockingWindow target = null;
				if (targetComponent != null)
					target = viewMap.getView(targetComponent.getID());
				if (target == null) {
					target = lastComponent;
				}

				if (target == null || target.getWindowParent() == null)
					w = rootWindow.getWindow();
				else
					w = target.getWindowParent();

				if (w == null)
					rootWindow.setWindow(window);
				else if (w instanceof TabWindow)
					((TabWindow) w).addTab(window);
				else if (w instanceof SplitWindow) {
					DockingWindow left = ((SplitWindow) w).getLeftWindow();
					DockingWindow right = ((SplitWindow) w).getRightWindow();
					((SplitWindow) w).setWindows(new TabWindow(),
							new TabWindow());
					if (ObjectUtil.equals(target, left)) {
						left = new TabWindow(
								new DockingWindow[] { left, window });
					} else {
						right = new TabWindow(new DockingWindow[] { right,
								window });
					}
					((SplitWindow) w).setWindows(left, right);
				} else
					rootWindow.setWindow(new TabWindow(new DockingWindow[] { w,
							window }));
			}
		}
	}

	protected Icon wrenchIcon = new BitmapIcon(FrameworkUtil
			.getResourceImage("tiny_wrench_icon.gif"));

	protected Icon checkIcon = new BitmapIcon(FrameworkUtil
			.getResourceImage("tiny_checkmark_icon.gif"));

	protected Icon questionIcon = new BitmapIcon(FrameworkUtil
			.getResourceImage("tiny_question_icon.gif"));

	protected void configureComponent(JToggleButton button, View v) {
		ComponentConfigCard card = (ComponentConfigCard) v.getComponent();
		if (button.isSelected()) {
			v.getViewProperties().setTitle(
					"Configuring: " + card.getComponent().getTitle());
			ConfigurationPanel panel = card.getConfigScreen();
			panel.setComponent(card.getComponent());
			panel.init();
		} else {
			v.getViewProperties().setTitle(card.getComponent().getTitle());
			card.getConfigScreen().commit();
		}
		card.toggle();
		card.repaint();
	}

	public Color getBackground() {
		return background;
	}

	public void setBackground(Color background) {
		this.background = background;
		configureTheme();
	}

	public Font getFont() {
		return font;
	}

	public void setFont(Font font) {
		this.font = font;
		configureTheme();
	}

	public Color getDarkColor() {
		return darkColor;
	}

	public void setDarkColor(Color darkColor) {
		this.darkColor = darkColor;
	}

	public Color getLightColor() {
		return lightColor;
	}

	public void setLightColor(Color lightColor) {
		this.lightColor = lightColor;
	}

	public String getPerspectiveResourceDir() {
		return perspectiveResourceDir;
	}

	public void setPerspectiveResourceDir(String perspectiveResourceDir) {
		this.perspectiveResourceDir = perspectiveResourceDir;
	}

	public String getDefaultPerspectiveResourcePath() {
		return defaultPerspectiveResourcePath;
	}

	public void setDefaultPerspectiveResourcePath(
			String defaultPerspectiveResourcePath) {
		this.defaultPerspectiveResourcePath = defaultPerspectiveResourcePath;
	}

	public String getPerspectiveListResourcePath() {
		return perspectiveListResourcePath;
	}

	public void setPerspectiveListResourcePath(
			String perspectiveListResourcePath) {
		this.perspectiveListResourcePath = perspectiveListResourcePath;
	}

	public void setComponentTitlebarTooltip(GUIComponent target, String label) {
		View v = getView(target);
		if (v == null)
			return;
		ViewTitleBar titleBar = SwingUtil.getDescendantOfType(v,
				ViewTitleBar.class);
		if (titleBar == null) {
//		    logger.info("setComponentTitlebarTooltip: can't get title bar for " + label);
		    return;
		}
		TabWindow tabWindow = (TabWindow) SwingUtilities.getAncestorOfClass(
				TabWindow.class, v);
		titleBar.setToolTipText(label);
		tabWindow.setToolTipText(label);
	}

	public void setComponentTitlebarColor(GUIComponent target, Color color) {
		View v = getView(target);
		if (v == null)
			return;
		ViewTitleBar titleBar = SwingUtil.getDescendantOfType(v,
				ViewTitleBar.class);
		TabWindow tabWindow = (TabWindow) SwingUtilities.getAncestorOfClass(
				TabWindow.class, v);
		if (tabWindow != null) {
			TabbedPanel panel = SwingUtil.getDescendantOfType(tabWindow,
					TabbedPanel.class);
			if (panel == null)
				return;
			for (int i = 0; i < panel.getTabCount(); i++) {
				Tab t = panel.getTabAt(i);
				if (t instanceof TitledTab) {
					TitledTab tt = (TitledTab) t;
					if (SwingUtilities.isDescendingFrom(v, t
							.getContentComponent())) {
						tt.getProperties().getNormalProperties()
								.getComponentProperties().setForegroundColor(
										color);
					}
				}
			}
		}
		// v.getViewProperties().getViewTitleBarProperties().getNormalProperties()
		// .getComponentProperties().setForegroundColor(color);
		if (titleBar != null)
		    titleBar.getLabel().setForeground(color);
	}

	public void setComponentLabel(GUIComponent target, String label) {
		View v = getView(target);
		if (v == null) {
//			logger.info("setComponentLabel: view is null for target " + target + ", label " + label);
			return;
		}
		ViewTitleBar titleBar = SwingUtil.getDescendantOfType(v,
				ViewTitleBar.class);
		v.getViewProperties().getViewTitleBarProperties().getNormalProperties()
				.setTitle(label);
		v.getViewProperties().getViewTitleBarProperties().getNormalProperties()
				.setTitleVisible(true);
		v.getViewProperties().setTitle(label);
		TabWindow tabWindow = (TabWindow) SwingUtilities.getAncestorOfClass(
				TabWindow.class, v);
		if (tabWindow != null) {
			TabbedPanel panel = SwingUtil.getDescendantOfType(tabWindow,
					TabbedPanel.class);
			if (panel == null)
				return;
			for (int i = 0; i < panel.getTabCount(); i++) {
				Tab t = panel.getTabAt(i);
				if (t instanceof TitledTab) {
					TitledTab tt = (TitledTab) t;
					if (SwingUtilities.isDescendingFrom(v, t
							.getContentComponent())) {
						tt.getProperties().getNormalProperties().setText(label);
						tt.setText(label);
					}
				}
			}
		}
		for (LayoutListener listener : layoutListeners) {
			listener.titleChanged(target, label);
		}
	}

	public String getComponentLabel(GUIComponent c) {
		View v = getView(c);
		if (v == null)
			return null;
		return v.getViewProperties().getTitle();
	}

	public Color getComponentTitlebarColor(GUIComponent c) {
		View v = getView(c);
		ViewTitleBar titleBar = SwingUtil.getDescendantOfType(v,
				ViewTitleBar.class);
		return titleBar.getLabel().getForeground();
	}

	public String getComponentTitlebarTooltip(GUIComponent c) {
		View v = getView(c);
		ViewTitleBar titleBar = SwingUtil.getDescendantOfType(v,
				ViewTitleBar.class);
		return titleBar.getToolTipText();
	}

	public boolean isFloating(GUIComponent c) {
		View v = getView(c);
		return v.isUndocked();
	}

	public void setFloating(GUIComponent c, boolean floating) {
		View v = getView(c);
		if (floating) {
			Frame f = GUIManager.getManager().getFrame();
			Rectangle r = getFloatRectangle(v);
			v.undock(r.getLocation());
		} else
			v.dock();
	}

    public void focusComponent(GUIComponent c) {
        // both calls are required to focus component and last child, especially if it's floating
        getView(c).requestFocus();
        getView(c).restoreFocus();
    }

    public void restoreComponent(GUIComponent c) {
        getView(c).restore();
    }
    
    public void lockDockingPanels() {
    	rootWindow.getRootWindowProperties().getDockingWindowProperties().setDragEnabled(false);
    	rootWindow.getRootWindowProperties().getDockingWindowProperties().setUndockEnabled(false);
    	rootWindow.getRootWindowProperties().getDockingWindowProperties().setCloseEnabled(false);
    	rootWindow.getRootWindowProperties().getDockingWindowProperties().setMinimizeEnabled(false);
    	rootWindow.getRootWindowProperties().getDockingWindowProperties().setMaximizeEnabled(false);
    }
    
    public void unlockDockingPanels() {
    	rootWindow.getRootWindowProperties().getDockingWindowProperties().setDragEnabled(true);
    	rootWindow.getRootWindowProperties().getDockingWindowProperties().setUndockEnabled(true);
    	rootWindow.getRootWindowProperties().getDockingWindowProperties().setCloseEnabled(true);
    	rootWindow.getRootWindowProperties().getDockingWindowProperties().setMinimizeEnabled(true);
    	rootWindow.getRootWindowProperties().getDockingWindowProperties().setMaximizeEnabled(true);
    }
	
}

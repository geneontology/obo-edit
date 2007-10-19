package org.bbop.framework.dock.idw;

import java.awt.Color;
import java.awt.Font;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
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
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JToggleButton;

import net.infonode.docking.DefaultButtonFactories;
import net.infonode.docking.DockingWindow;
import net.infonode.docking.DockingWindowAdapter;
import net.infonode.docking.RootWindow;
import net.infonode.docking.SplitWindow;
import net.infonode.docking.TabWindow;
import net.infonode.docking.View;
import net.infonode.docking.properties.RootWindowProperties;
import net.infonode.docking.theme.DockingWindowsTheme;
import net.infonode.docking.theme.ShapedGradientDockingTheme;
import net.infonode.docking.util.PropertiesUtil;
import net.infonode.docking.util.StringViewMap;
import net.infonode.gui.colorprovider.FixedColorProvider;
import net.infonode.util.Direction;

import org.bbop.framework.ComponentManager;
import org.bbop.framework.ConfigurationPanel;
import org.bbop.framework.FrameworkUtil;
import org.bbop.framework.GUIManager;
import org.bbop.framework.GUIComponent;
import org.bbop.framework.GUIComponentFactory;
import org.bbop.framework.dock.LayoutDriver;
import org.bbop.framework.dock.Perspective;
import org.bbop.io.FileUtil;
import org.bbop.io.IOUtil;
import org.bbop.util.ObjectUtil;

public class IDWDriver implements LayoutDriver {

	protected StringViewMap viewMap;

	protected RootWindow rootWindow;

	protected DockingWindowsTheme currentTheme = new ShapedGradientDockingTheme();

	protected RootWindowProperties properties = new RootWindowProperties();

	protected List<Perspective> perspectives = new LinkedList<Perspective>();

	protected Perspective currentPerspective;

	protected Color background;

	protected Font font;

	protected Color darkColor;

	protected Color lightColor;

	protected Collection<ViewListener> viewListeners = new LinkedList<ViewListener>();

	protected String perspectiveResourceDir;

	protected String defaultPerspectiveResourcePath;

	protected String perspectiveListResourcePath;

	public IDWDriver() {
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

	public void cleanup() {
		if (currentPerspective != null) {
			savePerspectiveAs(currentPerspective, currentPerspective.getName());
			savePerspectives();
		}
	}

	protected void savePerspectives() {
		File file = getPerspectivesFile();
		XMLEncoder encoder;
		try {
			encoder = new XMLEncoder(new BufferedOutputStream(
					new FileOutputStream(file)));
			encoder.writeObject(perspectives);
			encoder.writeObject(currentPerspective);
			encoder.close();
		} catch (IOException e) {
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

	protected static File getPerspectivesFile() {
		return new File(getPerspectivesDir(), "perspectives.xml");
	}

	@SuppressWarnings("unchecked")
	protected void loadPerspectives() {
		File file = getPerspectivesFile();
		if (getPerspectiveListResourcePath() != null) {
			try {
				FileUtil.ensureExists(file, getPerspectiveListResourcePath());
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
		}
		if (file.exists()) {

			XMLDecoder d;
			perspectives = null;
			Perspective currentPerspective = null;
			try {
				d = new XMLDecoder(new BufferedInputStream(new FileInputStream(
						file)));
				perspectives = (List<Perspective>) d.readObject();
				currentPerspective = (Perspective) d.readObject();
				d.close();
			} catch (Exception e) {
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
			createDefaultPerspectives();
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
			e.printStackTrace();
		}
	}

	protected String getResource(Perspective perspective) {

		String out = perspectiveResourceDir + "/" + perspective.getID()
				+ ".idw";
		if (ClassLoader.getSystemClassLoader().getResource(out) == null)
			out = defaultPerspectiveResourcePath;
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
				destroyView(v);
			else
				System.err.println("COULD NOT DESTROY VIEW: " + c);
		}

		ObjectInputStream stream = getInputStream(path);
		rootWindow.read(stream, false);
		stream.close();
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
		RootWindowProperties titleBarStyleProperties = PropertiesUtil
				.createTitleBarStyleRootWindowProperties();
		properties.addSuperObject(titleBarStyleProperties);

		BBOPDockingTheme theme = new BBOPDockingTheme(new FixedColorProvider(
				getDarkColor()), new FixedColorProvider(getLightColor()),
				new FixedColorProvider(getBackground()), 4, getFont());
		setTheme(theme);
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

	public View createView(GUIComponentFactory factory, String id, String label) {
		if (id == null)
			id = factory.getIDs().get(0) + ":" + factory.getDefaultID();
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
		final View v = new View(label, null, (JComponent) card) {
			@Override
			protected void finalize() throws Throwable {
				super.finalize();
			}
		};
		v.addListener(new DockingWindowAdapter() {

			@Override
			public void windowAdded(DockingWindow addedToWindow,
					DockingWindow addedWindow) {
				v.getViewProperties().setTitle(flabel);
				lastComponent = addedWindow;
			}

			@Override
			public void viewFocusChanged(View previouslyFocusedView,
					View focusedView) {
				if (focusedView != null)
					lastComponent = focusedView;
			}

			@Override
			public void windowClosed(DockingWindow window) {
				destroyView(v);
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
		/*
		 * if (c instanceof Filterable || c instanceof FilteredRenderable) {
		 * final JButton filterButton = ButtonFactory
		 * .createFlatHighlightButton(filterIcon, "Click to modify filters", 0,
		 * null); filterButton.setFocusable(false);
		 * filterButton.addActionListener(new ActionListener() {
		 * 
		 * public void actionPerformed(ActionEvent e) { JPopupMenu menu =
		 * GUIUtil.getFilterMenu((JComponent) c); menu.show(filterButton,
		 * filterButton.getWidth() / 2, filterButton.getHeight() / 2); }
		 * 
		 * }); v.getCustomTitleBarComponents().add(filterButton); }
		 */
		if (c.getConfigurationPanel() != null)
			v.getCustomTitleBarComponents().add(configButton);
		return v;
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

	public String showComponent(GUIComponentFactory factory,
			GUIComponent target, String name, String label,
			boolean preferFloat, Rectangle floatRect) {
		View v = createView(factory, name, label);
		GUIComponent c = ((ComponentConfigCard) v.getComponent())
				.getComponent();

		boolean restoringFromCache = viewMap.contains(v);
		addView(v);
		addWindow(v, target, restoringFromCache);
		return c.getID();
	}

	protected void addWindow(DockingWindow window,
			GUIComponent targetComponent, boolean tryRestore) {
		if (rootWindow == null)
			return;
		if (tryRestore)
			window.restore();
		if (window.getWindowParent() == null) {
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
				((SplitWindow) w).setWindows(new TabWindow(), new TabWindow());
				if (ObjectUtil.equals(target, left)) {
					left = new TabWindow(new DockingWindow[] { left, window });
				} else {
					right = new TabWindow(new DockingWindow[] { right, window });
				}
				((SplitWindow) w).setWindows(left, right);
			} else
				rootWindow.setWindow(new TabWindow(new DockingWindow[] { w,
						window }));
		}
	}

	protected Icon wrenchIcon = new BitmapIcon(FrameworkUtil
			.getResourceImage("tiny_wrench_icon.gif"));

	protected Icon checkIcon = new BitmapIcon(FrameworkUtil
			.getResourceImage("tiny_checkmark_icon.gif"));

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
		/*
		 * v.getViewProperties().setTitle( "Configuring: " +
		 * v.getViewProperties().getTitle()); v.setComponent(new
		 * JLabel("Bye!")); v.repaint();
		 */
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
}

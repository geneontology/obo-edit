package org.oboedit.util;

import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;

import org.bbop.swing.BackgroundEventQueue;
import org.bbop.swing.KeyRecorder;
import org.bbop.util.TaskDelegate;
import org.obo.datamodel.LinkedObject;
import org.obo.filters.FilterPair;
import org.obo.filters.RenderedFilter;
import org.obo.history.HistoryItem;
import org.obo.reasoner.ReasonerListener;
import org.obo.util.IDUtil;
import org.oboedit.controller.FilterManager;
import org.oboedit.controller.IDManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Filterable;
import org.oboedit.gui.FilteredRenderable;
import org.oboedit.gui.Selection;
import org.oboedit.gui.event.GlobalFilterListener;
import org.oboedit.gui.event.HistoryAppliedEvent;
import org.oboedit.gui.event.HistoryListener;
import org.oboedit.gui.event.ReasonerStatusEvent;
import org.oboedit.gui.event.ReasonerStatusListener;
import org.oboedit.gui.event.ReloadEvent;
import org.oboedit.gui.event.ReloadListener;
import org.oboedit.gui.event.RootChangeEvent;
import org.oboedit.gui.event.RootChangeListener;

public class GUIUtil {
	protected static class ListenerBundle {
		public GlobalFilterListener globalFilterListener;

		public HistoryListener historyListener;

		public ReasonerListener reasonerListener;

		public ReasonerStatusListener reasonerStatusListener;

		public RootChangeListener rootListener;
	}

	protected static Map<ReloadListener, ListenerBundle> bundleMap = new HashMap<ReloadListener, ListenerBundle>();

	public static void addReloadListener(final ReloadListener listener) {
		addReloadListener(SessionManager.getManager(), FilterManager
				.getManager(), listener);
	}

	public static void addReloadListener(SessionManager sessionManager,
			FilterManager filterManager, final ReloadListener listener) {
		ListenerBundle bundle = new ListenerBundle();
		bundle.globalFilterListener = new GlobalFilterListener() {
			public void globalFilterChanged() {
				listener.reload(new ReloadEvent(this, null, false, true, false,
						false));
			}
		};
		bundle.historyListener = new HistoryListener() {
			public void applied(HistoryAppliedEvent event) {
				listener.reload(new ReloadEvent(this, event, true, false,
						false, false));
			}

			public void reversed(HistoryAppliedEvent event) {
				listener.reload(new ReloadEvent(this, event, true, false,
						false, false));
			}
		};
		bundle.reasonerListener = new ReasonerListener() {

			public void reasoningFinished() {
				listener.reload(new ReloadEvent(this, null, false, false, true,
						false));
			}

			public void reasoningStarted() {
			}
		};
		bundle.reasonerStatusListener = new ReasonerStatusListener() {
			public void statusChanged(ReasonerStatusEvent e) {
				if (!e.isActive()) {
					listener.reload(new ReloadEvent(this, null, false, false,
							true, false));
				}
			}
		};
		bundle.rootListener = new RootChangeListener() {
			public void changeRoot(RootChangeEvent e) {
				if (!SessionManager.getManager().getUseReasoner())
					listener.reload(new ReloadEvent(this, null, false, false,
							false, true));
			}
		};
		filterManager.addGlobalFilterListener(bundle.globalFilterListener);
		sessionManager.addHistoryListener(bundle.historyListener);
		sessionManager.addReasonerListener(bundle.reasonerListener, true);
		sessionManager.addReasonerStatusListener(bundle.reasonerStatusListener);
		sessionManager.addRootChangeListener(bundle.rootListener);
		bundleMap.put(listener, bundle);
	}

	public static void removeReloadListener(ReloadListener listener) {
		removeReloadListener(SessionManager.getManager(), FilterManager
				.getManager(), listener);
	}

	public static void removeReloadListener(SessionManager sessionManager,
			FilterManager filterManager, ReloadListener listener) {
		ListenerBundle bundle = bundleMap.get(listener);
		if (bundle != null) {
			filterManager
					.removeGlobalFilterListener(bundle.globalFilterListener);
			sessionManager.removeHistoryListener(bundle.historyListener);
			sessionManager.removeReasonerListener(bundle.reasonerListener);
			sessionManager
					.removeReasonerStatusListener(bundle.reasonerStatusListener);
			sessionManager.removeRootChangeListener(bundle.rootListener);
			bundleMap.remove(listener);
		}
	}

	protected static final boolean isMacOS = System.getProperty("os.name").equals(
			"Mac OS X");

	protected static final boolean isWindows = System.getProperty("os.name")
			.startsWith("Windows");

	public static boolean isMacOS() {
		return GUIUtil.isMacOS;
	}
	
	public static boolean isWindows() {
		return isWindows;
	}

	public static boolean isPopupTrigger(MouseEvent e) {
		return isPopupTrigger(e, null);
	}

	public static boolean isPopupTrigger(MouseEvent e,
			KeyRecorder.KeyChecker keyChecker) {
		if (isMacOS()) {
			return (e.isMetaDown() || (SwingUtilities.isRightMouseButton(e) && !e
					.isControlDown()))
					&& e.getID() == MouseEvent.MOUSE_RELEASED;
		} else {
			boolean isRight = SwingUtilities.isRightMouseButton(e);
			return isRight && e.getID() == MouseEvent.MOUSE_RELEASED;
		}
	}

	public static Font decodeFont(String fontName, String fontSize,
			String fontType) {
		if (fontName == null || fontSize == null || fontType == null)
			return null;
		try {
			int style = Font.PLAIN;
			if (fontType.equalsIgnoreCase("bold"))
				style = Font.BOLD;
			else if (fontType.equalsIgnoreCase("italic"))
				style = Font.ITALIC;
			else if (fontType.equalsIgnoreCase("bolditalic"))
				style = Font.BOLD | Font.ITALIC;

			Font out = new Font(fontName, style, Integer.parseInt(fontSize));
			return out;
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	public static JPopupMenu getFilterMenu(final JComponent c) {
		if (!(c instanceof Filterable || c instanceof FilteredRenderable))
			return null;
		JPopupMenu v = new JPopupMenu("Filter");
		JMenuItem removeAllDecorationAndFilters = new JMenuItem(
				"Remove all renderers and filters");
		Filterable tf = null;
		FilteredRenderable tfr = null;
		if (c instanceof FilteredRenderable)
			tfr = (FilteredRenderable) c;
		if (c instanceof Filterable)
			tf = (Filterable) c;
		final Filterable f = tf;
		final FilteredRenderable fr = tfr;

		if (fr != null && f != null) {
			removeAllDecorationAndFilters.setEnabled((fr.getObjectRenderers()
					.size() > 0 || f.getFilter() != null));

			removeAllDecorationAndFilters
					.addActionListener(new ActionListener() {
						public void actionPerformed(ActionEvent e) {
							fr.getObjectRenderers().clear();
							if (f.getFilter() != null) {
								f.setFilter(null);
							}
							c.repaint();
						}
					});
			v.add(removeAllDecorationAndFilters);
		}

		if (fr != null) {
			JMenuItem removeAllDecoration = new JMenuItem(
					"Remove all renderers");
			removeAllDecoration.setEnabled(fr.getObjectRenderers().size() > 0);
			removeAllDecoration.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					fr.getObjectRenderers().clear();
					c.repaint();
				}
			});
			v.add(removeAllDecoration);

			JMenu renderMenu = new JMenu("Remove specific renderer");
			renderMenu.setEnabled(fr.getObjectRenderers().size() > 0);
			for (final RenderedFilter renderer : fr.getObjectRenderers()) {
				JMenuItem ritem = new JMenuItem("Remove " + renderer.toString());
				ritem.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						fr.removeObjectRenderer(renderer);
					}
				});
				renderMenu.add(ritem);
			}
			for (final RenderedFilter renderer : fr.getLinkRenderers()) {
				JMenuItem ritem = new JMenuItem("Remove " + renderer.toString());
				ritem.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						fr.removeLinkRenderer(renderer);
					}
				});
				renderMenu.add(ritem);
			}
			v.add(renderMenu);
		}

		if (f != null) {
			JMenuItem filterMenuItem = new JMenuItem("Remove filter");
			filterMenuItem.setEnabled(f.getFilter() != null);
			filterMenuItem.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					f.setFilter(null);
				}
			});
			v.add(filterMenuItem);
		}
		return v;
	}
	
	public static final Object PRE_SELECTION_KEY = new Object();
	public static final Object POST_SELECTION_KEY = new Object();
	
	public static String fetchID(LinkedObject parent) {
		return IDUtil.fetchID(IDManager.getManager().getIDAdapter(),
				SessionManager.getManager().getSession(), parent);
	}
	
	public static void setSelections(HistoryItem item, Selection pre, Selection post) {
		setPreSelection(item, pre);
		setPostSelection(item, post);
	}
	
	public static void setPreSelection(HistoryItem item, Selection selection) {
		item.setProperty(PRE_SELECTION_KEY, selection);
	}
	
	public static Selection getPreSelection(HistoryItem item) {
		return (Selection) item.getProperty(PRE_SELECTION_KEY);
	}
	
	public static void setPostSelection(HistoryItem item, Selection selection) {
		item.setProperty(POST_SELECTION_KEY, selection);
	}
	
	public static Selection getPostSelection(HistoryItem item) {
		return (Selection) item.getProperty(POST_SELECTION_KEY);
	}

	public static void addRendererPair(FilteredRenderable r, FilterPair pair) {
		RenderedFilter f = pair.getLinkRenderer();
		if (f != null)
			r.addLinkRenderer(f);
		f = pair.getObjectRenderer();
		if (f != null)
			r.addObjectRenderer(f);
	}
}

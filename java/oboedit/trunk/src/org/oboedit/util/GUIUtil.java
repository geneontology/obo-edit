package org.oboedit.util;

import java.awt.Font;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import org.bbop.io.IOUtil;
import org.bbop.swing.KeyRecorder;
import org.obo.datamodel.LinkedObject;
import org.obo.history.HistoryItem;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerListener;
import org.obo.util.IDUtil;
import org.oboedit.controller.FilterManager;
import org.oboedit.controller.IDManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.FilteredRenderable;
import org.oboedit.gui.ObjectSelector;
import org.oboedit.gui.Selection;
import org.oboedit.gui.event.GlobalFilterListener;
import org.oboedit.gui.event.HistoryAppliedEvent;
import org.oboedit.gui.event.HistoryListener;
import org.oboedit.gui.event.OntologyReloadListener;
import org.oboedit.gui.event.ReasonerStatusEvent;
import org.oboedit.gui.event.ReasonerStatusListener;
import org.oboedit.gui.event.ReloadEvent;
import org.oboedit.gui.event.ReloadListener;
import org.oboedit.gui.event.RootChangeEvent;
import org.oboedit.gui.event.RootChangeListener;
import org.oboedit.gui.filter.GeneralRendererSpec;
import org.oboedit.gui.filter.GeneralRendererSpecField;
import org.oboedit.gui.filter.RenderSpec;
import org.oboedit.gui.filter.RenderedFilter;

import org.apache.log4j.*;

public class GUIUtil {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GUIUtil.class);
	protected static class ListenerBundle {
		public GlobalFilterListener globalFilterListener;

		public HistoryListener historyListener;

		public ReasonerListener reasonerListener;

		public ReasonerStatusListener reasonerStatusListener;

		public OntologyReloadListener ontologyReloadListener;

		public RootChangeListener rootListener;
	}

	protected static Map<ReloadListener, ListenerBundle> bundleMap = new HashMap<ReloadListener, ListenerBundle>();

	public static void addReloadListener(final ReloadListener listener) {
		addReloadListener(SessionManager.getManager(), FilterManager
				.getManager(), listener);
	}

	// Note: args to ReloadEvent constructor are
// 	public ReloadEvent(Object source, EventObject parentEvent,
// 			boolean isHistory,
// 			boolean isFilter,
// 			boolean isReasoner,
// 			boolean isRoot,
// 			boolean isOntologyReload) {

	public static void addReloadListener(SessionManager sessionManager,
			FilterManager filterManager, final ReloadListener listener) {
		ListenerBundle bundle = new ListenerBundle();
		bundle.globalFilterListener = new GlobalFilterListener() {
			public void globalFilterChanged() {
				listener.reload(new ReloadEvent(this, null, false, true, false,
						false, false));
			}
		};
		bundle.ontologyReloadListener = new OntologyReloadListener() {

			public void reload() {
				listener.reload(new ReloadEvent(this, null, false, false,
						false, false, true));
			}

		};
		bundle.historyListener = new HistoryListener() {
			public void applied(HistoryAppliedEvent event) {
				listener.reload(new ReloadEvent(this, event, true, false,
						false, false, false));
			}

			public void reversed(HistoryAppliedEvent event) {
				listener.reload(new ReloadEvent(this, event, true, false,
						false, false, false));
			}
		};
		bundle.reasonerListener = new ReasonerListener() {

			public void reasoningFinished() {
				listener.reload(new ReloadEvent(this, null, false, false, true,
						false, false));
			}

			public void reasoningStarted() {
			}
		};
		bundle.reasonerStatusListener = new ReasonerStatusListener() {
			public void statusChanged(ReasonerStatusEvent e) {
				if (!e.isActive()) {
					listener.reload(new ReloadEvent(this, null, false, false,
							true, false, false));
				}
			}
		};
		bundle.rootListener = new RootChangeListener() {
			public void changeRoot(RootChangeEvent e) {
				if (!SessionManager.getManager().getUseReasoner())
					listener.reload(new ReloadEvent(this, null, false, false,
							false, true, false));
			}
		};
		filterManager.addGlobalFilterListener(bundle.globalFilterListener);
		sessionManager.addHistoryListener(bundle.historyListener);
		sessionManager.addReasonerListener(bundle.reasonerListener, true);
		sessionManager.addReasonerStatusListener(bundle.reasonerStatusListener);
		sessionManager.addRootChangeListener(bundle.rootListener);
		sessionManager.addOntologyReloadListener(bundle.ontologyReloadListener);
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
			sessionManager
					.removeOntologyReloadListener(bundle.ontologyReloadListener);
			bundleMap.remove(listener);
		}
	}

	protected static final boolean isMacOS = System.getProperty("os.name")
			.equals("Mac OS X");

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

	public static final Object PRE_SELECTION_KEY = new Object();
	public static final Object POST_SELECTION_KEY = new Object();

	public static String fetchID(LinkedObject parent) {
		return IDUtil.fetchID(IDManager.getManager().getIDAdapter(),
				SessionManager.getManager().getSession(), parent);
	}

	public static void setSelections(HistoryItem item, Selection pre, Selection post) {
//		logger.debug("GUIUtil.setSelections");
//		logger.debug("HistoryItem item: " + item);
//		logger.debug("Selection pre: " + pre);
//		logger.debug("Selection post: " + post);
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

	public static RenderSpec getSpec(FilteredRenderable fr, Object o, Collection<RenderedFilter>... fs) {
		RenderSpec out = null;
		ReasonedLinkDatabase r = SessionManager.getManager().getReasoner();
		for (Collection<RenderedFilter> f : fs) {
			if (f.size() == 0)
				continue;
			for (RenderedFilter rf : f) {
			    if (rf != null) {
				rf.getFilter().setReasoner(r);
				if (rf.getFilter().satisfies(o)) {
					if (out == null)
						out = rf.getSpec();
					else {
						out = out.merge(fr, rf.getSpec(), o);
					}
				}
			    }
			}
		}
		return out;
	}

	public static String renderHTML(FilteredRenderable selector, String text,
			Collection<GeneralRendererSpecField<?>> ignore,
			GeneralRendererSpec spec, Object obj) {
//		logger.debug("GUIUtil.renderHTML()");
		StringBuffer out = new StringBuffer(text);
		if (spec != null) {
			for (GeneralRendererSpecField field : spec.getFields()) {
				if (ignore.contains(field))
					continue;
				if (field.getHTMLType() > 0) {
					Object val = spec.getValue(field);
					field.renderHTML(selector, val, out, obj);
				}
			}
		}
		out.insert(0, "<html>");
		out.append("</html>");
		return out.toString();
	}

	public static String renderHTML(FilteredRenderable selector, String text,
			Object o, Collection<GeneralRendererSpecField<?>> ignore,
			Collection<RenderedFilter>... f) {
		String out = text;
		RenderSpec spec = getSpec(selector, o, f);
		if (spec instanceof GeneralRendererSpec || spec == null) {
			out = renderHTML(selector, text, ignore, (GeneralRendererSpec) spec, o);
		}
		return out;
	}

	// public static void addRendererPair(FilteredRenderable r, FilterPair pair)
	// {
	// RenderedFilter f = pair.getLinkRenderer();
	// if (f != null)
	// r.addLinkRenderer(f);
	// f = pair.getObjectRenderer();
	// if (f != null)
	// r.addObjectRenderer(f);
	// }

    public static void copyExistingConfigFiles(File oldPrefsDir, File newPrefsDir) {
		logger.info("prefs directory " + newPrefsDir + " does not yet exist--creating.");
		if (!newPrefsDir.mkdir()) {
			String err = "Error: could not create " + newPrefsDir;
			logger.info(err);
			JOptionPane.showMessageDialog(null,err,"Error creating configuration directory",JOptionPane.ERROR_MESSAGE);
		}
		else {
			if (oldPrefsDir.exists()) {
				String m = "I see you have user settings in " + oldPrefsDir + 
					".\nUser settings are now stored in " + newPrefsDir +
					".\nWould you like to copy your old settings from " + oldPrefsDir +
					",\n keeping in mind that if they are from old versions of OBO-Edit2, they might not work right?";
				int val = JOptionPane.showConfirmDialog(null, m, "Copy user settings to new directory?", JOptionPane.YES_NO_OPTION);
				if (val == JOptionPane.YES_OPTION) {
					try {
						IOUtil.copyFiles(oldPrefsDir, newPrefsDir);

					} catch (Exception e) {
						logger.info("Caught exception while trying to copy " + oldPrefsDir + " to " + newPrefsDir);
					}
				}
			}
		}
	}
}

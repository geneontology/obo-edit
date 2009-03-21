package org.oboedit.graph;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.PathCapable;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.impl.OnTheFlyReasoner;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.graph.VisiblesProvider.EnabledCheckDelegate;
import org.oboedit.graph.VisiblesProvider.SelectionProvider;
import org.oboedit.gui.Selection;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import edu.umd.cs.piccolo.event.PInputEvent;
import org.oboedit.controller.SessionManager;
import org.apache.log4j.*;

public class RootDisplayRightClickMenuFactory implements RightClickMenuFactory {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(RootDisplayRightClickMenuFactory.class);

	protected final static short TOO_MANY_TO_SHOW_ALL = 500;

	protected static interface EnabledStatusUpdatable {
		public void updateEnabledStatus();
	}

	protected class ProviderMenu extends JMenu implements
	EnabledStatusUpdatable {
		public ProviderMenu(String label) {
			super(label);
		}

		public void updateEnabledStatus() {
			for (Component c : getMenuComponents()) {
				if (c.isEnabled()) {
					setEnabled(true);
					return;
				}
			}
			setEnabled(false);
		}

		public String toString() {
			return getText();
		}
	}

	protected class ProviderMenuItem extends JMenuItem implements
	EnabledStatusUpdatable {
		protected VisiblesProvider provider;

		public String toString() {
			return getText();
		}

		public ProviderMenuItem(VisiblesProvider provider_in) {
			super(provider_in.getLabel());
			this.provider = provider_in;

			addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					final boolean screenIsBlank = canvas.getVisibleNodes().isEmpty();
					final Collection<PathCapable> visibles = new HashSet<PathCapable>(
							canvas.getVisibleObjects());
					Iterator<PathCapable> it = visibles.iterator();
					while (it.hasNext()) {
						if (!(it.next() instanceof LinkedObject))
							it.remove();
					}
					Selection selection = null;
					if (provider.getSelectionProvider() != null)
						selection = provider.getSelectionProvider().getSelection();
					if (selection == null)
						selection = SelectionManager.createEmptySelection(null);
					logger.debug("selection: " + selection);
					logger.debug("Showing " + provider.getLabel() + " in Graph Editor"); 
					final Collection<? extends LinkedObject> shown = provider.getShown(selection);
					if (shown != null)
						visibles.addAll(shown);
					Collection<? extends LinkedObject> hidden = provider
					.getHidden(selection);
					if (hidden != null)
						visibles.removeAll(hidden);
					canvas.setVisibleObjects(visibles);
					if (screenIsBlank) {
						canvas.addPostLayoutAction(new Runnable() {
							public void run() {
								canvas.zoomToObjects();
							}
						});
					}
				}
			});

		}

		public void updateEnabledStatus() {
			if (canvas != null) {
				Selection selection = null;
				if (provider.getSelectionProvider() != null)
					selection = provider.getSelectionProvider().getSelection();
				if (selection == null)
					selection = SelectionManager.createEmptySelection(null);
				boolean enabled = provider.isEnabled(selection, canvas
						.getVisibleObjects());
				setEnabled(enabled);
			}
		}
	}

	protected List<JMenuItem> menuItems = new LinkedList<JMenuItem>();

	protected JMenu showMenu = new ProviderMenu("Show");

	protected JMenu hideMenu = new ProviderMenu("Hide");

	protected LinkDatabaseCanvas canvas;

	protected Selection pickerSelection;

	protected VisiblesProvider.EnabledCheckDelegate selectionCheckDelegate = new VisiblesProvider.EnabledCheckDelegate() {
		public boolean isEnabled(Selection s) {
			return !s.isEmpty();
		}
	};

	protected VisiblesProvider.EnabledCheckDelegate fullSelectionCheckDelegate = new VisiblesProvider.EnabledCheckDelegate() {
		public boolean isEnabled(Selection s) {
			return s.getTerms().size() > 0;
		}
	};

	protected VisiblesProvider.EnabledCheckDelegate reasonerCheckDelegate = new VisiblesProvider.EnabledCheckDelegate() {
		public boolean isEnabled(Selection s) {
			return canvas.getLinkDatabase() instanceof ReasonedLinkDatabase;
		}
	};

	protected VisiblesProvider.DefaultSelectionProvider singlePickProvider = new VisiblesProvider.DefaultSelectionProvider();

	protected VisiblesProvider.DefaultSelectionProvider fullSelectionProvider = new VisiblesProvider.DefaultSelectionProvider();

	protected class ParentVisiblesProvider extends AbstractVisiblesProvider {
		public ParentVisiblesProvider(String label,
				SelectionProvider selectionProvider,
				EnabledCheckDelegate... delegates) {
			super(label, true, selectionProvider, delegates);
		}

		// parentLink --> canvas.getLinkProviderDatabase().getParents(lo)
		public Collection<? extends LinkedObject> getShown(Selection selection) {
			Collection<LinkedObject> out = new HashSet<LinkedObject>();
			for (LinkedObject lo : selection.getTerms()) {
//				logger.debug("lo: " +lo);
				for (Link parentLink : canvas.getLinkProviderDatabase().getParents(lo)){
					out.add(parentLink.getParent());
				}

			}
			return out;
		}
	}

	protected static VisiblesProvider.EnabledCheckDelegate[] appendDelegate(
			VisiblesProvider.EnabledCheckDelegate[] delegates,
			VisiblesProvider.EnabledCheckDelegate delegate) {
		LinkedList<VisiblesProvider.EnabledCheckDelegate> out = new LinkedList<VisiblesProvider.EnabledCheckDelegate>();
		out.add(delegate);
		for (VisiblesProvider.EnabledCheckDelegate d : delegates)
			out.add(d);
		return out.toArray(delegates);
	}

	protected class DescendantVisiblesProvider extends AbstractVisiblesProvider {
		protected OBOProperty type;
		protected boolean showTransitives;
		protected boolean showIntransitives;

		public DescendantVisiblesProvider(String label, OBOProperty type,
				boolean showTransitives, boolean showIntransitives,
				SelectionProvider selectionProvider,
				EnabledCheckDelegate... delegates) {
			super(label, false, selectionProvider, appendDelegate(delegates,
					reasonerCheckDelegate));
			this.type = type;
			this.showTransitives = showTransitives;
			this.showIntransitives = showIntransitives;
		}

		protected boolean isEnabled(Selection selection) {
			return super.isEnabled(selection);
		}

		// parentLink --> getChildren(lo)
		public Collection<? extends LinkedObject> getShown(Selection selection) {
			Collection<LinkedObject> out = new HashSet<LinkedObject>();
			ReasonedLinkDatabase reasoner = null;
			reasoner = canvas.getReasoner();

			for (LinkedObject lo : selection.getTerms()) {
				Collection<Link> allChildren;
				if(reasoner != null)
					allChildren = reasoner.getChildren(lo);
				else 
					allChildren = canvas.getLinkProviderDatabase().getChildren(lo);
				for (Link parentLink : allChildren) {
					if (type == null
							|| (parentLink.getType().isTransitive() && showTransitives)
							|| (!parentLink.getType().isTransitive() && showIntransitives)
							|| reasoner.isSubPropertyOf(parentLink.getType(),
									type))
						out.add(parentLink.getChild());
				}
			}
			return out;
		}
	}

	protected class AncestorVisiblesProvider extends AbstractVisiblesProvider {
		protected OBOProperty type;
		protected boolean showTransitives;
		protected boolean showIntransitives;

		public AncestorVisiblesProvider(String label, OBOProperty type,
				boolean showTransitives, boolean showIntransitives,
				SelectionProvider selectionProvider,
				EnabledCheckDelegate... delegates) {
			super(label, false, selectionProvider, appendDelegate(delegates,
					reasonerCheckDelegate));
			this.type = type;
			this.showTransitives = showTransitives;
			this.showIntransitives = showIntransitives;
		}
		@Override
		protected boolean isEnabled(Selection selection) {
			boolean enabled = super.isEnabled(selection);
			return enabled;
		}
		// parentLink --> getParents(lo)
		public Collection<? extends LinkedObject> getShown(Selection selection) {
			Collection<LinkedObject> out = new HashSet<LinkedObject>();
			ReasonedLinkDatabase reasoner = null;
			reasoner = canvas.getReasoner();

			for (LinkedObject lo : selection.getTerms()) {
				Collection<Link> allParents;
				if(reasoner != null)
					allParents = reasoner.getParents(lo);
				else 
					allParents = canvas.getLinkProviderDatabase().getParents(lo);

				for (Link parentLink : allParents) {
					if (type == null
							|| (parentLink.getType().isTransitive() && showTransitives)
							|| (!parentLink.getType().isTransitive() && showIntransitives)
							|| reasoner.isSubPropertyOf(parentLink.getType(),
									type))
						out.add(parentLink.getParent());
				}
			}
			return out;
		}
	}

	protected class ChildVisiblesProvider extends AbstractVisiblesProvider {
		public ChildVisiblesProvider(String label,
				SelectionProvider selectionProvider,
				EnabledCheckDelegate... delegates) {
			super(label, true, selectionProvider, delegates);
		}

		//childLink --> canvas.getLinkProviderDatabase().getChildren(lo)
		public Collection<? extends LinkedObject> getShown(Selection selection) {
			Collection<LinkedObject> out = new HashSet<LinkedObject>();
			for (LinkedObject lo : selection.getTerms()) {
				for (Link childLink : canvas.getLinkProviderDatabase().getChildren(lo))
					out.add(childLink.getChild());
			}
			return out;
		}
	}

	protected class HideSelfProvider extends AbstractVisiblesProvider {
		public HideSelfProvider(String label,
				SelectionProvider selectionProvider,
				EnabledCheckDelegate... delegates) {
			super(label, true, selectionProvider, delegates);
		}
		public Collection<? extends LinkedObject> getHidden(Selection selection) {
			return selection.getTerms();
		}
	}

	protected class HideNonSelfProvider extends AbstractVisiblesProvider {
		public HideNonSelfProvider(String label,
				SelectionProvider selectionProvider,
				EnabledCheckDelegate... delegates) {
			super(label, true, selectionProvider, delegates);
		}
		// pc --> canvas.getVisibleObjects()
		public Collection<? extends LinkedObject> getHidden(Selection selection) {
			Collection<LinkedObject> out = new LinkedList<LinkedObject>();
			for (PathCapable pc : canvas.getVisibleObjects()) {
				if (pc instanceof LinkedObject) {
					if (!selection.getTerms().contains(pc)) {
						out.add((LinkedObject) pc);
					}
				}
			}
			return out;
		}
	}


	public RootDisplayRightClickMenuFactory() {
		menuItems.add(showMenu);
		menuItems.add(hideMenu);
		final VisiblesProvider.SelectionProvider selectionProvider = null;
		final VisiblesProvider.EnabledCheckDelegate checkDelegate = null;

		JMenuItem hideEverythingItem = createItemFromDisplayables(new AbstractVisiblesProvider("Everything") {
			public Collection<? extends LinkedObject> getHidden(Selection selection) {
				return TermUtil.getTerms(canvas.getLinkDatabase());
			}
		});

		JMenuItem showEverythingItem = createItemFromDisplayables(new AbstractVisiblesProvider("Everything") {
			public Collection<? extends LinkedObject> getShown(Selection selection) {
				Collection<IdentifiedObject> objects = canvas.getLinkDatabase().getObjects();
				if (objects.size() > TOO_MANY_TO_SHOW_ALL) {
					// This grays out the menu item
					logger.info("GraphEditor Show Everything: too many objects to show (" + objects.size() + ")");
					return new LinkedList<OBOClass>();
				}
				else
					return TermUtil.getTerms(canvas.getLinkDatabase());
			}
		});

		// Show -> All 
		JMenuItem showAllParentsItem = createItemFromDisplayables(new ParentVisiblesProvider(
				"- Parents", singlePickProvider, selectionCheckDelegate));
		JMenuItem showAllParentsOfSelectionItem = createItemFromDisplayables(new ParentVisiblesProvider(
				"- Parents of selected terms", fullSelectionProvider,
				selectionCheckDelegate));
		JMenuItem showAllChildrenItem = createItemFromDisplayables(new ChildVisiblesProvider(
				"- Children", singlePickProvider, selectionCheckDelegate));
		JMenuItem showAllChildrenOfSelectionItem = createItemFromDisplayables(new ChildVisiblesProvider(
				"- Children of selected terms", fullSelectionProvider, selectionCheckDelegate));
		JMenuItem showAllRootsItem = createItemFromDisplayables(new AbstractVisiblesProvider("- Roots") {
			public Collection<? extends LinkedObject> getShown(Selection selection) {
				Collection<? extends LinkedObject> roots = TermUtil.getRoots(canvas.getLinkDatabase());
				return roots;
			}
		});
		showMenu.add("All");
		showMenu.add(showAllParentsItem);
		showMenu.add(showAllParentsOfSelectionItem);
		showMenu.add(showAllChildrenItem);
		showMenu.add(showAllChildrenOfSelectionItem);
		showMenu.add(showAllRootsItem);
		showMenu.addSeparator();

		//Show -> Ancestors
		showMenu.add("Ancestors");
		showMenu.add(getAncestorMenu("Of current  term", 
				singlePickProvider, selectionCheckDelegate));
		showMenu.add(getAncestorMenu("Of current selection",
				fullSelectionProvider, fullSelectionCheckDelegate));
		showMenu.addSeparator();

		//Show -> Descendants
		showMenu.add("Descendants");
		showMenu.add(getDescendantMenu("Of current term", 
				singlePickProvider, selectionCheckDelegate));
		showMenu.add(getDescendantMenu("Of current selection",
				fullSelectionProvider, fullSelectionCheckDelegate));
		showMenu.addSeparator();


		// hide
		JMenuItem hideCurrentItem = createItemFromDisplayables(new HideSelfProvider(
				"Current term", singlePickProvider, selectionCheckDelegate));
		JMenuItem hideSelectionItem = createItemFromDisplayables(new HideSelfProvider(
				"All selected terms", fullSelectionProvider,
				selectionCheckDelegate));
		JMenuItem hideNonCurrentItem = createItemFromDisplayables(new HideNonSelfProvider(
				"Everything but current term", singlePickProvider,
				selectionCheckDelegate));
		JMenuItem hideNonSelectionItem = createItemFromDisplayables(new HideNonSelfProvider(
				"Everything but selection", fullSelectionProvider,
				selectionCheckDelegate));

		showMenu.add(showEverythingItem);

		hideMenu.add(hideSelectionItem);
		hideMenu.add(hideCurrentItem);
		hideMenu.add(hideNonCurrentItem);
		hideMenu.add(hideNonSelectionItem);
		hideMenu.add(hideEverythingItem);
	}

	protected JMenu getAncestorMenu(String label,
			final VisiblesProvider.SelectionProvider selectionProvider,
			final VisiblesProvider.EnabledCheckDelegate checkDelegate) {
		JMenu ancestorMenu = new ProviderMenu(label){
			public void updateEnabledStatus() {
				if (!checkDelegate.isEnabled(selectionProvider.getSelection())) {
					setEnabled(false);
					return;
				}
				removeAll();
//				AncestorVisiblesProvider(String label, OBOProperty type, boolean showTransitives, boolean showIntransitives,
//				SelectionProvider selectionProvider, EnabledCheckDelegate... delegates) {
				JMenuItem showAllAncestorsItem = createItemFromDisplayables(new AncestorVisiblesProvider(
						"All ancestors", (OBOProperty) null, true, true,
						selectionProvider, checkDelegate));
				JMenuItem showAllNonTransitiveAncestorsItem = createItemFromDisplayables(new AncestorVisiblesProvider(
						"All non-transitive ancestors", (OBOProperty) null, false,true, 
						selectionProvider, checkDelegate));
				JMenuItem showAllTransitiveAncestorsItem = createItemFromDisplayables(new AncestorVisiblesProvider(
						"All transitive ancestors", (OBOProperty) null, true, false,
						selectionProvider, checkDelegate));
				JMenu typesAncestorMenu= new ProviderMenu("Ancestors of Type");

				Collection<OBOProperty> types = TermUtil.getRelationshipTypes(canvas.getLinkProviderDatabase());
				for (OBOProperty type : types) {
//					logger.debug("type: " + type);
					JMenuItem item = createItemFromDisplayables(new AncestorVisiblesProvider(
							type.getName(), type, false, false,
							selectionProvider, checkDelegate));
					typesAncestorMenu.add(item);
				}
				add(showAllAncestorsItem);
				add(showAllNonTransitiveAncestorsItem);
				add(showAllTransitiveAncestorsItem);
				add(typesAncestorMenu);
				super.updateEnabledStatus();
			}
		};
		return ancestorMenu;
	}

	protected JMenu getDescendantMenu(String label,
			final VisiblesProvider.SelectionProvider selectionProvider,
			final VisiblesProvider.EnabledCheckDelegate checkDelegate) {
		JMenu descendantMenu = new ProviderMenu(label){
			public void updateEnabledStatus() {
				if (!checkDelegate.isEnabled(selectionProvider.getSelection())) {
					setEnabled(false);
					return;
				}
				removeAll();
				JMenuItem showAllDescendantsItem = createItemFromDisplayables(new DescendantVisiblesProvider(
						"All descendants", (OBOProperty) null, true, true,
						selectionProvider, checkDelegate));
				JMenuItem showAllNonTransitiveDescendantsItem = createItemFromDisplayables(new DescendantVisiblesProvider(
						"All non-transitive descendants", (OBOProperty) null, false, true, 
						selectionProvider, checkDelegate));
				JMenuItem showAllTransitiveDescendantsItem = createItemFromDisplayables(new DescendantVisiblesProvider(
						"All transitive descendants", (OBOProperty) null, true, false,
						selectionProvider, checkDelegate));
				JMenu typesDescendantMenu= new ProviderMenu("Descendants of Type");
				Collection<OBOProperty> types = TermUtil.getRelationshipTypes(canvas.getLinkProviderDatabase());
				for (OBOProperty type : types) {
					JMenuItem item = createItemFromDisplayables(new DescendantVisiblesProvider(
							type.getName(), type, false, false,
							selectionProvider, checkDelegate));
					typesDescendantMenu.add(item);
				}
				add(showAllDescendantsItem);
				add(showAllNonTransitiveDescendantsItem);
				add(showAllTransitiveDescendantsItem);
				add(typesDescendantMenu);
				super.updateEnabledStatus();
			}
		};
		return descendantMenu;
	}

	protected JMenuItem createItemFromDisplayables(
			final VisiblesProvider provider) {
		final JMenuItem item = new ProviderMenuItem(provider);
		return item;
	}

	protected void updateMenuEnabledStatus(JMenuItem item) {
		if (item instanceof EnabledStatusUpdatable) {
			EnabledStatusUpdatable esu = (EnabledStatusUpdatable) item;
			if (item instanceof JMenu) {
				Component[] comps = ((JMenu) item).getMenuComponents();
				for (Component c : ((JMenu) item).getMenuComponents()) {
//					logger.debug("c.getClass().getSimpleName(): " + c.getClass().getSimpleName());
					if(!c.getClass().getSimpleName().equalsIgnoreCase("Separator")){
						updateMenuEnabledStatus((JMenuItem) c);	
					}		
				}
			}
			esu.updateEnabledStatus();
		}
	}

	public List<JMenuItem> getMenuItems(LinkDatabaseCanvas canvas, PInputEvent e) {
		this.canvas = canvas;
		Selection selection = canvas.getSelection();
		Selection pickedAsSelection = canvas.getPickedAsSelection(e);
		if (selection.isEmpty()
				|| !selection.getAllSelectedObjects().containsAll(
						pickedAsSelection.getAllSelectedObjects()))
			selection = SelectionManager.createEmptySelection(null);
		singlePickProvider.setSelection(pickedAsSelection);
		fullSelectionProvider.setSelection(selection);

		for (JMenuItem item : menuItems) {
			updateMenuEnabledStatus(item);
		}
		return menuItems;
	}

}

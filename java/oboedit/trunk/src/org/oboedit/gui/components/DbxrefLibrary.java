package org.oboedit.gui.components;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.GUIManager;
import org.bbop.swing.*;
import org.obo.datamodel.*;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.gui.event.*;
import org.oboedit.gui.widget.DbxrefListEditor;

import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.io.*;

import org.apache.log4j.*;

public class DbxrefLibrary extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DbxrefLibrary.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	ListEditor editor;

	JList refList;

	JButton useButton;

	JButton useAsDefButton;

	JButton saveButton;

	JButton configureButton;

	JButton importButton;

	JButton exportButton;

	Vector dbxrefs = new Vector();

	JDialog dialog;

	/*
	 * protected DropTarget dropTarget;
	 * 
	 * protected class DropDbxrefsListener implements DropListener { public
	 * boolean allowDrop(DragEvent e) { if (e.getData() instanceof Vector) {
	 * Vector v = (Vector) e.getData(); return v.size() > 0 && v.get(0)
	 * instanceof Dbxref; } if (e.getData() instanceof Object[]) { Object[] v =
	 * (Object[]) e.getData(); return v.length > 0 && v[0] instanceof Dbxref; }
	 * return false; }
	 * 
	 * public void dragEnter(DragEvent e) { refList.setBorder(new
	 * LineBorder(Color.black)); }
	 * 
	 * public void dragExit(DragEvent e) { refList.setBorder(null); }
	 * 
	 * public void drop(DragEvent e) { refList.setBorder(null);
	 * 
	 * if (e.getData() instanceof Vector) { Vector newRefs = (Vector)
	 * e.getData(); for (int i = 0; i < newRefs.size(); i++) { Dbxref ref =
	 * (Dbxref) newRefs.get(i); if (!dbxrefs.contains(ref)) dbxrefs.add(ref); } }
	 * else if (e.getData() instanceof Object[]) { Object[] os = (Object[])
	 * e.getData(); for (int i = 0; i < os.length; i++) { Dbxref ref = (Dbxref)
	 * os[i]; if (!dbxrefs.contains(ref)) dbxrefs.add(ref); } } updateList(); //
	 * refList.setListData(dbxrefs); }
	 * 
	 * public void draggedOver(DragEvent e) { } }
	 */

	public static String getVersion() {
		return "1.000";
	}

	@Override
	public String getName() {
		return "Dbxref Library v" + getVersion();
	}

	public DbxrefLibrary(String id) {
		super(id);
		setLayout(new BorderLayout());
		useButton = new JButton("Use dbxref");
		useAsDefButton = new JButton("Use as def dbxref");
		importButton = new JButton("Import dbxrefs");
		exportButton = new JButton("Export dbxrefs");
		saveButton = new JButton("Save configuration");
		configureButton = new JButton("Configure dbxrefs");
		refList = new JList();
	}

	public static class DbxrefLibraryConfiguration implements
			ComponentConfiguration {
		protected Vector library;

		public DbxrefLibraryConfiguration() {
			library = new Vector();
		}

		public DbxrefLibraryConfiguration(Vector library) {
			setLibrary(library);
		}

		public void setLibrary(Vector library) {
			this.library = library;
		}

		public Vector getLibrary() {
			return library;
		}
	}

	@Override
	public void setConfiguration(ComponentConfiguration config) {
		if (config instanceof DbxrefLibraryConfiguration) {
			dbxrefs = ((DbxrefLibraryConfiguration) config).getLibrary();
			updateList();
		}
	}

	@Override
	public ComponentConfiguration getConfiguration() {
		return new DbxrefLibraryConfiguration(dbxrefs);
	}

	protected Vector getDbxrefList() {
		return dbxrefs;
	}

	/*
	 * protected void buildDbxrefList(Properties props) { dbxrefs = new
	 * Vector(); int length = 0; try { String lengthStr =
	 * props.getProperty("refCount"); length = Integer.parseInt(lengthStr); }
	 * catch (NumberFormatException e) { } for(int i=0; i < length; i++) {
	 * String database = props.getProperty("database"+i); String id =
	 * props.getProperty("id"+i); String desc = props.getProperty("desc"+i);
	 * String typeStr = props.getProperty("type"+i);
	 * dbxrefs.add(controller.getHistory().getObjectFactory().
	 * createDbxref(database, id, desc, Dbxref.ANALOG, null)); }
	 * Collections.sort(dbxrefs); }
	 */
	protected void configure() {
		JLabel noSelection = new JLabel("Nothing selected");

		JPanel panel = new JPanel();
		panel.add(noSelection);
		DbxrefListEditor component = new DbxrefListEditor();
		panel.setPreferredSize(new Dimension(250, 200));
		component.setPreferredSize(new Dimension(250, 200));

		editor = new ListEditor(component, panel, getDbxrefList(), true, true,
				true, true, false);
		editor.setPreferredSize(new Dimension(350, 200));

		dialog = new JDialog();

		JPanel dialogPanel = new JPanel();
		dialogPanel.setLayout(new BorderLayout());
		dialogPanel.add(editor, "Center");
		dialogPanel.add(saveButton, "South");
		dialog.setContentPane(dialogPanel);

		dialog.setSize(350, 230);

		dialog.show();
		Collections.sort(dbxrefs);
		refList.setListData(dbxrefs);
	}

	protected void sendUpdates(boolean isDef) {
		java.util.List<Dbxref> refs = new LinkedList<Dbxref>();
		Object[] selection = refList.getSelectedValues();
		for (int i = 0; i < selection.length; i++) {
			Dbxref ref = (Dbxref) ((Dbxref) selection[i]).clone();
			if (isDef) {
				ref.setType(Dbxref.DEFINITION);
			} else
				ref.setType(Dbxref.ANALOG);
			refs.add(ref);
		}
		GUIManager.getManager().fireUserEvent(
				new AbstractDbxrefEditorComponent.DbxrefUpdateEvent(this,
						(isDef ? "gui.dbxref.def.add"
								: "gui.dbxref.general.add"), refs
								.toArray(new Dbxref[0])));
	}

	protected void saveConfiguration() {
		dbxrefs = editor.getData();
		if (dbxrefs == null)
			dbxrefs = new Vector();
		Collections.sort(dbxrefs);
		refList.setListData(dbxrefs);
		refList.repaint();
	}

	protected void attachListeners() {
		useButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				sendUpdates(false);
			}
		});
		useAsDefButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				sendUpdates(true);
			}
		});
		saveButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				saveConfiguration();
				dialog.dispose();
			}
		});
		configureButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				configure();
			}
		});
		exportButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				exportFile();
			}
		});
		importButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				importFile();
			}
		});
	}

	protected void updateList() {
		refList.setListData(getDbxrefList());
		refList.repaint();
	}

	protected void exportFile() {
		JFileChooser chooser = new JFileChooser();
		if (chooser.showSaveDialog(GUIManager.getManager().getFrame()) == JFileChooser.APPROVE_OPTION) {
			File file = chooser.getSelectedFile();
			try {
				PrintWriter writer = new PrintWriter(new FileWriter(file));
				for (int i = 0; i < dbxrefs.size(); i++) {
					Dbxref ref = (Dbxref) dbxrefs.get(i);
					String line = ref.getDatabase() + ":" + ref.getDatabaseID();
					if (ref.getDesc() != null) {
						line += " " + ref.getDesc();
					}
					writer.println(line);
				}
				writer.close();
			} catch (IOException ex) {
				JOptionPane.showMessageDialog(GUIManager.getManager()
						.getFrame(), "Could not write file " + file);
			}
		}
	}

	protected void importFile() {
		JFileChooser chooser = new JFileChooser();
		if (chooser.showOpenDialog(GUIManager.getManager().getFrame()) == JFileChooser.APPROVE_OPTION) {
			dbxrefs.clear();
			File file = chooser.getSelectedFile();
			try {
				BufferedReader reader = new BufferedReader(new FileReader(file));
				String line;
				while ((line = reader.readLine()) != null) {
					if (line.length() == 0)
						continue;
					int colonIndex = line.indexOf(':');
					int spaceIndex = line.indexOf(' ');
					int dbxrefEndIndex = spaceIndex;
					if (dbxrefEndIndex == -1)
						dbxrefEndIndex = line.length();
					String db = line.substring(0, colonIndex);
					String id = line.substring(colonIndex + 1, dbxrefEndIndex);
					String desc = null;
					if (spaceIndex != -1) {
						desc = line.substring(spaceIndex + 1, line.length());
					}
					Dbxref dbxref = SessionManager.getManager().getSession()
							.getObjectFactory().createDbxref(db, id, desc,
									Dbxref.ANALOG, null);
					dbxrefs.add(dbxref);
				}
				Collections.sort(dbxrefs);
				refList.setListData(dbxrefs);
				refList.repaint();
			} catch (IOException ex) {
				JOptionPane.showMessageDialog(null, "Could not read file "
						+ file);
			}
		}
	}

	@Override
	public void init() {
		removeAll();
		// buildDbxrefList(props);

		JScrollPane scroller = new JScrollPane(refList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		// scroller.setPreferredSize(new Dimension(200, 200));

		attachListeners();

		updateList();

		JPanel ioPanel = new JPanel();
		ioPanel.setLayout(new GridLayout(1, 2));
		ioPanel.add(importButton);
		ioPanel.add(exportButton);

		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new GridLayout(3, 1));

		JPanel usePanel = new JPanel();
		usePanel.setLayout(new GridLayout(1, 2));
		usePanel.add(useButton);
		usePanel.add(useAsDefButton);

		buttonPanel.add(usePanel);
		buttonPanel.add(ioPanel);
		buttonPanel.add(configureButton);

		add(scroller, "Center");
		add(buttonPanel, "South");
	}

	@Override
	public void cleanup() {
		// controller.getDragController().unregisterDropTarget(dropTarget);
	}
}

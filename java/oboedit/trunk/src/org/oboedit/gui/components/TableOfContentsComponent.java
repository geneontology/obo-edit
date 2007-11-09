package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.StringBufferInputStream;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.io.IOUtil;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.util.HTMLUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.event.ReloadEvent;
import org.oboedit.gui.event.ReloadListener;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;
import org.oboedit.gui.event.TermHyperlinkListener;
import org.oboedit.util.GUIUtil;

public class TableOfContentsComponent extends AbstractGUIComponent {

	JEditorPane pane = new JEditorPane("text/html", null);
	protected File file;
	protected ReloadListener listener = new ReloadListener() {
		public void reload(ReloadEvent e) {
			TableOfContentsComponent.this.reload();
		}
	};
	protected TermHyperlinkListener hlistener = new TermHyperlinkListener() {

		@Override
		public void actionSelected(String linkParam, String val) {
		}

		@Override
		public void linkSelected(String linkParam, Link link) {
		}

		@Override
		public void termSelected(String linkParam, IdentifiedObject io) {
			if (io instanceof LinkedObject)
				SelectionManager.getManager().select(pane,
						(LinkedObject) io);
		}
		
	};

	public TableOfContentsComponent(String id) {
		super(id);
		pane.setEditable(false);
		pane.addHyperlinkListener(hlistener);
		setLayout(new BorderLayout());
		add(new JScrollPane(pane, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER), "Center");
		JPanel southPanel = new JPanel();
		southPanel.setLayout(new GridLayout(3, 9));
		for(char c = 'A'; c < 'Z'; c++) {
			JButton button = new JButton(""+c);
			button.setSize(new Dimension(12, 12));
			button.setBorderPainted(false);
			final char finalChar = c;
			button.addActionListener(new ActionListener() {

				public void actionPerformed(ActionEvent e) {
					try {
						pane.setPage(file.toURL().toString()+"#"+finalChar);
					} catch (MalformedURLException e1) {
						e1.printStackTrace();
					} catch (IOException e1) {
						e1.printStackTrace();
					}
				}				
			});
			southPanel.add(button);
		}
		add(southPanel, "South");
	}

	public void reload() {
		hlistener.setLinkDatabase(SessionManager.getManager().getCurrentLinkDatabase());
		pane.setEditable(false);
		if (file != null)
			file.delete();
		List<IdentifiedObject> objects = new ArrayList<IdentifiedObject>(
				SessionManager.getManager().getSession().getObjects());
		Collections.sort(objects, new Comparator<IdentifiedObject>() {

			public int compare(IdentifiedObject o1, IdentifiedObject o2) {
				return o1.getName().compareToIgnoreCase(o2.getName());
			}
		});
		StringBuffer html = new StringBuffer();
		html.append("<html>\n");
		html.append("<body>\n");
		char lastFirstLetter = '\0';
		boolean first = true;
		String listTag = "ul";
		for (IdentifiedObject io : objects) {
			char firstLetter = Character.toUpperCase(io.getName().charAt(0));

			if (firstLetter != lastFirstLetter) {
				if (!first)
					html.append("</" + listTag + ">");
				html.append("<a name='" + firstLetter + "'>");
				html.append("<h1>" + firstLetter + "</h1>");
				html.append("<" + listTag
						+ " style='list-style-type: none;'>\n");
			}
			if (first)
				first = false;
			html.append("<li>" + HTMLUtil.getHTMLLink(io, true));
			lastFirstLetter = firstLetter;
		}
		if (!first)
			html.append("</" + listTag + ">");
		html.append("</body>\n");
		html.append("</html>\n");
		try {
			file = File.createTempFile("toc", "html");
			file.deleteOnExit();
			IOUtil.dumpAndClose(new StringBufferInputStream(html.toString()),
					new FileOutputStream(file));
			pane.setPage(file.toURL());
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Override
	public void init() {
		super.init();
		GUIUtil.addReloadListener(listener);
		reload();
	}

	@Override
	public void cleanup() {
		super.cleanup();
		GUIUtil.removeReloadListener(listener);
	}

}

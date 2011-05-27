package org.oboedit.gui.components;

import java.awt.*;
import java.util.*;
import java.util.List;

import javax.swing.*;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;

import org.bbop.util.*;
import org.obo.datamodel.*;
import org.obo.filters.NameSearchCriterion;
import org.obo.history.*;
import org.obo.util.TermUtil;
import org.oboedit.gui.AbstractTextEditComponent;

import org.apache.log4j.*;

public class NameEditorComponent extends AbstractTextEditComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NameEditorComponent.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected JTextPane textField = new JTextPane();

	@Override
	public Component resolveName(String id, Properties props, String xml) {
		if (id.equals("field"))
			return textField;
		else
			return new JButton(id);
	}

	public NameEditorComponent() {
		if (textField.getDocument() instanceof AbstractDocument) {
			((AbstractDocument) textField.getDocument())
			.setDocumentFilter(new DocumentFilter() {
				@Override
				public void insertString(FilterBypass fb, int offset,
						String string, AttributeSet attr)
				throws BadLocationException {
					// TODO Auto-generated method stub
					super.insertString(fb, offset,
							replaceNewlines(string), attr);
				}

				@Override
				public void replace(FilterBypass fb, int offset,
						int length, String text, AttributeSet attrs)
				throws BadLocationException {
					super.replace(fb, offset, length,
							replaceNewlines(text), attrs);
				}
			});
		}
		/*
		 * textField.setDocument(new DefaultStyledDocument() { @Override public
		 * void insertString(int offs, String str, AttributeSet a) throws
		 * BadLocationException { StringBuffer b = new StringBuffer(); for(int
		 * i=0; i < str.length(); i++) { char c = str.charAt(i); if (c != '\n')
		 * b.append(c); }
		 * 
		 * super.insertString(offs, b.toString(), a); } });
		 */
	}

	@Override
	protected boolean useSubLayout() {
		return true;
	}

	protected static String replaceNewlines(String str) {
		StringBuffer b = new StringBuffer();
		for (int i = 0; i < str.length(); i++) {
			char c = str.charAt(i);
			if (c != '\n')
				b.append(c);
		}
		return b.toString();
	}

	@Override
	protected String getDefaultLayout() {
		return "<box orientation='HORZ'><label text='Name'/><spacer orientation='horz' size='10'/><component id='field'/></box>";
	}

	@Override
	protected void loadGUI() {
		if (currentObject != null) {
			textField.setText(currentObject.getName());

			boolean enable = !TermUtil.isObsolete(currentObject);
			textField.setEnabled(enable);
		} else {
			textField.setEnabled(false);
			textField.setText("<no selection>");
		}
	}

	@Override
	protected void initializeGUI() {
		textField.setMaximumSize(new Dimension(Integer.MAX_VALUE,
				(int) textField.getPreferredSize().getHeight()));
		setMinimumSize(getPreferredSize());
		setMaximumSize(new Dimension(Integer.MAX_VALUE,
				(int) getPreferredSize().getHeight()));
	}

	@Override
	protected void installListeners() {
		super.installListeners();
		getRoot().addMapping(new FieldPathSpec(NameSearchCriterion.CRITERION),
				this, textField);
	}

	@Override
	protected void uninstallListeners() {
		super.uninstallListeners();
		getRoot().removeMapping(
				new FieldPathSpec(NameSearchCriterion.CRITERION), textField);
	}

	protected String getText() {
		return textField.getText().trim();
	}

	public String getID() {
		return "NAME_EDITOR";
	}

	public void populateFields(IdentifiedObject io) {
		io.setName(textField.getText());
	}

	//	public List getChanges() {
	//		if (currentObject != null && currentObject instanceof IdentifiedObject) {
	//			if (!ObjectUtil.equals(textField.getText(), ((IdentifiedObject) currentObject).getName())) {
	//				HistoryItem item = new NameChangeHistoryItem((IdentifiedObject) currentObject, textField.getText());
	//				return Collections.singletonList(item);
	//			} else
	//				return Collections.EMPTY_LIST;
	//			} else
	//			return Collections.EMPTY_LIST;
	//	}


	public List<HistoryItem> getChanges(){
		if (currentObject != null) {
			LinkedList<HistoryItem> out = new LinkedList<HistoryItem>();
			IdentifiedObject obj = currentObject;

			if (!ObjectUtil.equals(textField.getText(), obj.getName())) {
				HistoryItem item = new NameChangeHistoryItem(obj, textField.getText());
				out.add(item);
			}
			return out;
		} else
			return Collections.emptyList();	
	}
}

package org.oboedit.gui.components;

import java.awt.*;
import java.util.*;
import javax.swing.*;

import org.bbop.expression.ExpressionException;
import org.bbop.util.*;
import org.obo.datamodel.*;
import org.obo.filters.CommentSearchCriterion;
import org.obo.filters.DefinitionDbxrefSearchCriterion;
import org.obo.history.*;
import org.oboedit.controller.VerificationManager;
import org.oboedit.gui.AbstractTextEditComponent;
import org.oboedit.gui.Preferences;

import org.apache.log4j.*;

public class CommentEditorComponent extends AbstractTextEditComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CommentEditorComponent.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 9103011155475223156L;

	protected JTextPane textField = new JTextPane();

	protected JScrollPane scroller = new JScrollPane(textField,
			JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

	@Override
	public Component resolveName(String id, Properties props, String xml) {
		if (id.equals("field"))
			return scroller;
		else
			return new JButton("id");
	}

	public CommentEditorComponent() {
	}

	@Override
	public void guiupdate() throws ExpressionException {
		super.guiupdate();
	}

	@Override
	protected boolean useSubLayout() {
		return true;
	}

	@Override
	protected String getDefaultLayout() {
		return "<component id='field' bordertitle='Comment'/>";
	}

	@Override
	protected void loadGUI() {
		if (currentObject != null) {
			textField.setEnabled(true);
			if (currentObject instanceof CommentedObject)
				textField.setText(((CommentedObject) currentObject)
						.getComment());
		} else {
			textField.setEnabled(false);
			textField.setText("<no selection>");
		}
	}

	@Override
	protected void initializeGUI() {
	}

	protected String getText() {
		return textField.getText().trim();
	}

	public String getID() {
		return "COMMENT_EDITOR";
	}

	public void populateFields(IdentifiedObject io) {
		if (io instanceof CommentedObject)
			((CommentedObject) io).setComment(getText());
	}

	protected String getWarningLabel() {
		return "Comment";
	}

	@Override
	protected void installListeners() {
		super.installListeners();
		getRoot().addMapping(
				new FieldPathSpec(CommentSearchCriterion.CRITERION), this,
				textField);
	}

	@Override
	protected void uninstallListeners() {
		super.uninstallListeners();
		getRoot().removeMapping(
				new FieldPathSpec(CommentSearchCriterion.CRITERION), textField);
	}

	public java.util.List getChanges() {
		if (currentObject != null && currentObject instanceof CommentedObject) {
			CommentedObject co = (CommentedObject) currentObject;
			if (!ObjectUtil.equals(getText(), co.getComment())) {
				HistoryItem item = new CommentChangeHistoryItem(co, getText());
				return Collections.singletonList(item);
			} else {
				return Collections.EMPTY_LIST;
			}
		} else
			return Collections.EMPTY_LIST;
	}
}

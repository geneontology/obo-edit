package org.oboedit.gui;

import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextPane;
import javax.swing.Timer;
import javax.swing.ToolTipManager;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.JTextComponent;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

import org.bbop.expression.ExpressionException;
import org.bbop.expression.JexlContext;
import org.bbop.swing.XMLLayoutUtil;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.IdentifiedObject;
import org.oboedit.controller.ExpressionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.controller.VerificationManager;
import org.oboedit.verify.CheckWarning;
import org.oboedit.verify.ObjectCheck;
import org.oboedit.verify.TextCheckWarning;

public abstract class AbstractCheckedTextEditComponent extends
		AbstractTextEditComponent implements CheckedTextEditComponent {

	public class VerifierDelegate {

		public boolean hasProblems() {
			return hasProblems;
		}
	}

	protected JTextComponent component;

	protected Color highlightColor;

	protected ObjectCheck check;

	protected JComponent warningComponent;

	protected boolean hasProblems = false;

	protected Icon warningIcon = Preferences
			.loadLibraryIcon("warning_icon.gif");

	protected Icon errorIcon = Preferences.loadLibraryIcon("error_icon.gif");

	protected Action updateAction = new AbstractAction() {
		public void actionPerformed(ActionEvent e) {
			if (dirty) {
				doHighlighting();
			} else
				checkTimer.stop();
		}
	};

	public static final int TIMER_DELAY = 500;

	protected Timer checkTimer = new Timer(TIMER_DELAY, updateAction);

	protected DocumentListener documentListener = new DocumentListener() {

		public void changedUpdate(DocumentEvent e) {
			setDirty(true);
		}

		public void insertUpdate(DocumentEvent e) {
			setDirty(true);
		}

		public void removeUpdate(DocumentEvent e) {
			setDirty(true);
		}

	};

	protected JLabel warningLabel;

	protected boolean dirty = false;

	protected VerifierDelegate verifierDelegate = new VerifierDelegate();

	protected List<CheckWarning> warnings = new LinkedList<CheckWarning>();

	{
		warningComponent = new JPanel();
		warningLabel = new JLabel();
		warningComponent.setVisible(false);
		warningComponent.setBackground(Color.red);
		warningLabel.setForeground(Color.white);
		warningComponent.add(warningLabel);
	}

	public List<CheckWarning> getWarnings() {
		return warnings;
	}

	protected void setDirty(boolean b) {
		dirty = b;
		if (dirty && !checkTimer.isRunning()) {
			checkTimer.start();
		}

	}

	@Override
	public void setContext(JexlContext context) {
		try {
			super.setContext(ExpressionManager.createSubContext(context, true,
					true, "Verifier", verifierDelegate));
		} catch (ExpressionException e) {
			e.printStackTrace();
		}
	}

	public AbstractCheckedTextEditComponent() {
		setHighlightColor(Color.red);
	}

	public JTextPane createTextComponent() {
		JTextPane out = new JTextPane() {
			@Override
			public String getToolTipText(MouseEvent event) {
				int pos = viewToModel(event.getPoint());
				Collection<TextCheckWarning> tcws = new LinkedList<TextCheckWarning>();
				for (CheckWarning w : warnings) {
					if (w instanceof TextCheckWarning) {
						TextCheckWarning tcw = (TextCheckWarning) w;
						if (tcw.getStartIndex() <= pos
								&& tcw.getEndIndex() >= pos) {
							tcws.add(tcw);
						}
					}
				}
				StringBuffer out = new StringBuffer("<html>\n");
				if (tcws.size() > 0) {
					out.append("<ul>\n");
				}
				for (TextCheckWarning w : tcws) {
					if (tcws.size() > 0) {
						out.append("<li>");
						out.append(w.getMessage());
						out.append("</li>\n");
					}
				}
				if (tcws.size() > 0) {
					out.append("</ul>");
				}
				out.append("</html>");
				System.err.println(out);
				return out.toString();
			}
		};
		ToolTipManager.sharedInstance().registerComponent(out);
		return out;
	}

	@Override
	public Component resolveName(String id, Properties props, String xml) {
		if (id.equals("warning")) {
			return warningComponent;
		} else
			return new JButton("id");
	}

	public void showWarningLabel(Icon icon, String message, Color bgColor,
			Color foregroundColor) {
		warningComponent.setBackground(bgColor);
		warningLabel.setIcon(icon);
		warningLabel.setText(message);
		warningLabel.setForeground(foregroundColor);

		if (!warningComponent.isVisible()) {
			warningComponent.setVisible(true);
			if (warningComponent.getParent() != null)
				warningComponent.getParent().validate();
			addWarningLabel();
		}
	}

	public void addWarningLabel() {
		XMLLayoutUtil.guiupdateTree(this);
		component.requestFocus();
	}

	public void hideWarningLabel() {
		if (warningComponent.isVisible()) {
			warningComponent.setVisible(false);
			XMLLayoutUtil.guiupdateTree(this);
			component.requestFocus();
		}
	}

	public void doHighlighting() {
		setDirty(false);
		hasProblems = false;
		warnings.clear();
		if (currentObject == null)
			return;
		component.getDocument().removeDocumentListener(documentListener);
		if (component.getDocument() instanceof StyledDocument) {
			((StyledDocument) component.getDocument()).setCharacterAttributes(
					0, component.getText().length(), SimpleAttributeSet.EMPTY,
					true);
		}
		ObjectCheck check = getCheck();
		IdentifiedObject clone = (IdentifiedObject) currentObject.clone();
		populateFields(clone);
		warnings.addAll(check.check(SessionManager.getManager().getSession(),
				clone, VerificationManager.TEXT_EDIT_COMMIT, true));
		int fatalCount = 0;
		int warningCount = 0;
		for (Object o : warnings) {
			CheckWarning warning = (CheckWarning) o;
			if (warning.isFatal())
				fatalCount++;
			else
				warningCount++;
			if (o instanceof TextCheckWarning) {
				TextCheckWarning tw = (TextCheckWarning) o;
				Style style = createStyle(tw);
				if (component.getDocument() instanceof StyledDocument) {
					StyledDocument d = (StyledDocument) component.getDocument();
					d.setCharacterAttributes(tw.getStartIndex(), tw
							.getEndIndex(), style, true);
				}
			}
		}
		if (fatalCount + warningCount > 0) {
			hasProblems = true;
			StringBuffer message = new StringBuffer();
			if (fatalCount > 0) {
				message.append(fatalCount + " fatal errors");
				if (warningCount > 0)
					message.append(" and ");
			}
			if (warningCount > 0) {
				message.append(warningCount + " warnings.");
			}
			showWarningLabel(fatalCount > 0 ? errorIcon : warningIcon, message
					.toString(), fatalCount > 0 ? Color.red : Color.yellow,
					fatalCount > 0 ? Color.white : Color.black);
		} else
			hideWarningLabel();
		component.getDocument().addDocumentListener(documentListener);
	}

	protected Style createStyle(TextCheckWarning tw) {
		if (component.getDocument() instanceof StyledDocument) {
			StyledDocument d = (StyledDocument) component.getDocument();
			Style s = d.getStyle(tw.getType());
			if (d.getStyle(tw.getType()) == null) {
				s = d.addStyle(tw.getType(), null);
				StyleConstants.setForeground(s, Color.red);
			}
			return s;
		}
		return null;
	}

	public ObjectCheck getCheck() {
		return check;
	}

	public void setCheck(ObjectCheck check) {
		this.check = check;
	}

	public JTextComponent getComponent() {
		return component;
	}

	public void setComponent(JTextComponent component) {
		if (this.component != null) {
			component.getDocument().removeDocumentListener(documentListener);
		}
		this.component = component;
		if (this.component != null) {
			component.getDocument().addDocumentListener(documentListener);
		}
	}

	public Color getHighlightColor() {
		return highlightColor;
	}

	public void setHighlightColor(Color highlightColor) {
		this.highlightColor = highlightColor;
	}
}

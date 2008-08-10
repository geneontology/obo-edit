package org.oboedit.gui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import javax.accessibility.Accessible;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JToolTip;
import javax.swing.JViewport;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.ToolTipManager;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.LineBorder;
import javax.swing.border.MatteBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.MouseInputAdapter;
import javax.swing.plaf.TextUI;
import javax.swing.text.BadLocationException;
import javax.swing.text.EditorKit;
import javax.swing.text.JTextComponent;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import javax.swing.text.View;
import javax.swing.text.Position.Bias;

import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;
import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.impl.SingleTermSession;
import org.obo.history.SingleTermOperationModel;
import org.oboedit.controller.SessionManager;
import org.oboedit.controller.VerificationManager;
import org.oboedit.util.TextEditUtil;
import org.oboedit.verify.CheckWarning;
import org.oboedit.verify.HistoryQuickFix;
import org.oboedit.verify.ImmediateQuickFix;
import org.oboedit.verify.QuickFix;
import org.oboedit.verify.TextCheckWarning;
import org.oboedit.verify.TextReplaceQuickFix;

import org.apache.log4j.*;

public class TextErrorDecorator implements ErrorDecorator {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TextErrorDecorator.class);

	protected class DelegateUI extends TextUI {
		protected TextUI delegate;

		public DelegateUI(TextUI delegate) {
			this.delegate = delegate;
		}

		public TextUI getDelegate() {
			return delegate;
		}

		public boolean contains(JComponent c, int x, int y) {
			return delegate.contains(c, x, y);
		}

		public void damageRange(JTextComponent t, int p0, int p1,
				Bias firstBias, Bias secondBias) {
			delegate.damageRange(t, p0, p1, firstBias, secondBias);
		}

		public void damageRange(JTextComponent t, int p0, int p1) {
			delegate.damageRange(t, p0, p1);
		}

		public boolean equals(Object obj) {
			return delegate.equals(obj);
		}

		public Accessible getAccessibleChild(JComponent c, int i) {
			return delegate.getAccessibleChild(c, i);
		}

		public int getAccessibleChildrenCount(JComponent c) {
			return delegate.getAccessibleChildrenCount(c);
		}

		public EditorKit getEditorKit(JTextComponent t) {
			return delegate.getEditorKit(t);
		}

		public Dimension getMaximumSize(JComponent c) {
			return delegate.getMaximumSize(c);
		}

		public Dimension getMinimumSize(JComponent c) {
			return delegate.getMinimumSize(c);
		}

		public int getNextVisualPositionFrom(JTextComponent t, int pos, Bias b,
				int direction, Bias[] biasRet) throws BadLocationException {
			return delegate.getNextVisualPositionFrom(t, pos, b, direction,
					biasRet);
		}

		public Dimension getPreferredSize(JComponent c) {
			return delegate.getPreferredSize(c);
		}

		public View getRootView(JTextComponent t) {
			return delegate.getRootView(t);
		}

		public String getToolTipText(JTextComponent t, Point pt) {
			if (warnings == null)
				return null;
			int pos = t.viewToModel(pt);
			StringBuffer b = new StringBuffer();
			for (CheckWarning w : warnings.singleValues()) {
				if (w instanceof TextCheckWarning) {
					TextCheckWarning tcw = (TextCheckWarning) w;
					if (tcw.getStartIndex() <= pos && pos <= tcw.getEndIndex()) {
						b.append("<li>");
						b.append(tcw.getMessage());
					}
				}
			}
			if (b.length() > 0) {
				return "<html><ul>" + b + "</ul></html>";
			}
			return null;
		}

		public int hashCode() {
			return delegate.hashCode();
		}

		public void installUI(JComponent c) {
			delegate.installUI(c);
		}

		public Rectangle modelToView(JTextComponent t, int pos, Bias bias)
				throws BadLocationException {
			return delegate.modelToView(t, pos, bias);
		}

		public Rectangle modelToView(JTextComponent t, int pos)
				throws BadLocationException {
			return delegate.modelToView(t, pos);
		}

		public void paint(Graphics g, JComponent c) {
			delegate.paint(g, c);
		}

		public String toString() {
			return delegate.toString();
		}

		public void uninstallUI(JComponent c) {
			delegate.uninstallUI(c);
		}

		public void update(Graphics g, JComponent c) {
			delegate.update(g, c);
		}

		public int viewToModel(JTextComponent t, Point pt, Bias[] biasReturn) {
			return delegate.viewToModel(t, pt, biasReturn);
		}

		public int viewToModel(JTextComponent t, Point pt) {
			return delegate.viewToModel(t, pt);
		}

	}

	protected MouseListener rightClickListener = new MouseAdapter() {
		@Override
		public void mousePressed(MouseEvent e) {
			if (SwingUtilities.isRightMouseButton(e)) {
				if (warnings == null)
					return;

				List<QuickFix> fixes = new LinkedList<QuickFix>();
				int pos = component.viewToModel(e.getPoint());
				StringBuffer b = new StringBuffer();
				for (CheckWarning w : warnings.singleValues()) {
					if (w instanceof TextCheckWarning) {
						TextCheckWarning tcw = (TextCheckWarning) w;
						if (tcw.getStartIndex() <= pos
								&& pos <= tcw.getEndIndex()) {
							fixes.addAll(tcw.getFixes());
						}
					}
				}
				if (fixes.size() > 0) {
					JPopupMenu menu = new JPopupMenu();
					for (final QuickFix f : fixes) {
						JMenuItem item = new JMenuItem(f.getDesc());
						item.addActionListener(new ActionListener() {
							public void actionPerformed(ActionEvent e) {
								applyFix(f);
							};
						});
						menu.add(item);
					}
					menu.show(component, e.getX(), e.getY());
				}
			}
		}
	};

	protected void applyFix(QuickFix action) {
		if (action instanceof ImmediateQuickFix) {
			((ImmediateQuickFix) action).run();
		} else if (action instanceof TextReplaceQuickFix) {
			int pos = component.getCaretPosition();
			component.setText(((TextReplaceQuickFix) action).getNewText());
			if (pos > component.getText().length())
				pos = component.getText().length();
			component.setCaretPosition(pos);
		} else if (action instanceof HistoryQuickFix) {
			int pos = component.getCaretPosition();
			IdentifiedObject io = (IdentifiedObject) parent.getObject().clone();
			parent.populateFields(io);
			SingleTermOperationModel model = new SingleTermOperationModel(
					SessionManager.getManager().getSession(), io);
			model.apply(((HistoryQuickFix) action).getItem());
			parent.setObject(io);
			component.requestFocus();
			if (pos > component.getText().length())
				pos = component.getText().length();
			component.setCaretPosition(pos);
		}
		TextEditUtil.addDirtyPaths(parent, getPaths());
	}

	protected JTextComponent component;

	protected Border originalBorder;

	protected FieldPathSpec spec;

	protected OBOTextEditComponent parent;

	protected DocumentListener documentListener = new DocumentListener() {

		public void changedUpdate(DocumentEvent e) {
			TextEditUtil.addDirtyPaths(parent, getPaths());
		}

		public void insertUpdate(DocumentEvent e) {
			TextEditUtil.addDirtyPaths(parent, getPaths());
		}

		public void removeUpdate(DocumentEvent e) {
			TextEditUtil.addDirtyPaths(parent, getPaths());
		}

	};

	protected MultiMap<FieldPath, CheckWarning> warnings = new MultiHashMap<FieldPath, CheckWarning>();

	protected DelegateUI delegateUI;

	public Collection<FieldPath> getPaths() {
		if (parent.getObject() == null)
			return Collections.emptySet();
		// ! Is it really necessary to clone this every time?  I think this is creating a lot of duplicate objects.
		IdentifiedObject object = (IdentifiedObject) parent.getObject().clone();
		parent.populateFields(object);
		FieldPath queryPath = FieldPathSpec.createQueryPath(spec, object);
		return queryPath.resolve();
	}

	public TextErrorDecorator(FieldPathSpec spec, OBOTextEditComponent parent,
			JTextComponent component) {
		this.spec = spec;
		this.parent = parent;
		this.component = component;
		delegateUI = new DelegateUI(component.getUI());
		component.setUI(delegateUI);
		originalBorder = component.getBorder();
		component.getDocument().addDocumentListener(documentListener);
		component.addMouseListener(rightClickListener);
		ToolTipManager.sharedInstance().registerComponent(component);
	}

	public void cleanup() {
		component.getDocument().removeDocumentListener(documentListener);
		component.setUI(delegateUI.getDelegate());
		component.setBorder(originalBorder);
		component.removeMouseListener(rightClickListener);
	}

	protected Style createStyle(TextCheckWarning tw) {
		if (component.getDocument() instanceof StyledDocument) {
			StyledDocument d = (StyledDocument) component.getDocument();
			Style s = d.getStyle(tw.getType());
			if (d.getStyle(tw.getType()) == null) {
				s = d.addStyle(tw.getType(), null);
				StyleConstants.setForeground(s, Color.red);
				if (tw.getFixes().size() > 0)
					StyleConstants.setUnderline(s, true);
			}
			return s;
		}
		return null;
	}

	public void clearWarnings() {
		this.warnings.clear();
		component.getDocument().removeDocumentListener(documentListener);
		if (component.getDocument() instanceof StyledDocument) {
			((StyledDocument) component.getDocument()).setCharacterAttributes(
					0, component.getText().length(), SimpleAttributeSet.EMPTY,
					true);
		}
		component.getDocument().addDocumentListener(documentListener);
	}

	public void setWarnings(FieldPath path, Collection<CheckWarning> warnings) {
		this.warnings.put(path, warnings);
		component.getDocument().removeDocumentListener(documentListener);
		if (component.getDocument() instanceof StyledDocument) {
			((StyledDocument) component.getDocument()).setCharacterAttributes(
					0, component.getText().length(), SimpleAttributeSet.EMPTY,
					true);
		}

		int fatalCount = 0;
		int warningCount = 0;
		for (CheckWarning warning : warnings) {
			if (warning.isFatal())
				fatalCount++;
			else
				warningCount++;
			if (warning instanceof TextCheckWarning) {
				TextCheckWarning tw = (TextCheckWarning) warning;
				Style style = createStyle(tw);
				if (component.getDocument() instanceof StyledDocument) {
					StyledDocument d = (StyledDocument) component.getDocument();
					d.setCharacterAttributes(tw.getStartIndex(), tw
							.getEndIndex()
							- tw.getStartIndex(), style, true);
				}
			}
		}
		decorate(component, fatalCount, warningCount);
		// component.setBorder(getBorder(fatalCount > 0, warningCount > 0));
		component.validate();
		component.getDocument().addDocumentListener(documentListener);
	}

	protected Icon errorIcon = Preferences.getPreferences().loadLibraryIcon(
			"warning_icon.gif");

	protected void decorate(JTextComponent c, int fatalCount, int warningCount) {
		// setBorder(component, getBorder(fatalCount > 0, warningCount > 0));
		createWrapperPanel(component);
	}

	protected void createWrapperPanel(JTextComponent component) {

		// TODO Auto-generated method stub

	}

	protected JComponent getBestContainer(JTextComponent component) {
		JComponent c = component;
		if (component.getParent() != null
				&& component.getParent() instanceof JViewport) {
			if (component.getParent().getParent() != null
					&& component.getParent().getParent() instanceof JScrollPane) {
				c = (JComponent) component.getParent().getParent();
			} else
				c = (JComponent) component.getParent();
		}

		return c;
	}

	protected void setBorder(JTextComponent component, Border border) {
		JComponent c = getBestContainer(component);
		c.setBorder(border);
	}

	protected Border getBorder(boolean fatal, boolean warning) {
		if (!fatal && !warning)
			return originalBorder;
		Color borderColor;
		if (fatal)
			borderColor = Color.red;
		else
			borderColor = Color.orange;
		LineBorder lineBorder = new LineBorder(borderColor, 5);
		MatteBorder matteBorder = new MatteBorder(errorIcon);
		TitledBorder titledBorder = new TitledBorder(lineBorder, "Error");
		if (originalBorder == null)
			return titledBorder;
		CompoundBorder cBorder = new CompoundBorder(titledBorder,
				originalBorder);
		return cBorder;
	}

}

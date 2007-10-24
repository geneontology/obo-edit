package org.oboedit.gui.widget;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;

import org.bbop.swing.*;
import org.obo.datamodel.*;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;

public class SynonymListEditor extends JPanel implements GenericEditorComponent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	JTextField textField;

	JComboBox typeList;

	JComboBox categoryList;

	Object obj;

	ListEditor editor;

	JList referenceList;

	JLabel referenceLabel;

	JButton referenceButton;

	Vector references = new Vector();

	Border lineBorder = new LineBorder(Color.black);

	protected static final String[] TYPES = { "Related Synonym",
			"Exact Synonym", "Narrow Synonym", "Broad Synonym" };

	/*
	 * DropListener dropListener = new DropAdapter() { @Override public boolean
	 * allowDrop(DragEvent e) { if (e.getData() instanceof Vector) { Vector v =
	 * (Vector) e.getData(); for (int i = 0; i < v.size(); i++) { if
	 * (!(v.elementAt(i) instanceof Dbxref)) return false; } return true; } else
	 * if (e.getData() instanceof Object[]) { Object[] v = (Object[])
	 * e.getData(); for (int i = 0; i < v.length; i++) { if (!(v[i] instanceof
	 * Dbxref)) return false; } return true; } return false; }
	 * 
	 * @Override public void dragEnter(DragEvent e) {
	 * referenceList.setBorder(lineBorder); }
	 * 
	 * @Override public void dragExit(DragEvent e) {
	 * referenceList.setBorder(null); }
	 * 
	 * @Override public void drop(DragEvent e) { referenceList.setBorder(null);
	 * if (e.getData() instanceof Vector) { Vector v = (Vector) e.getData(); for
	 * (int i = 0; i < v.size(); i++) { Dbxref s = (Dbxref) ((Dbxref)
	 * v.elementAt(i)).clone(); s.setType(Dbxref.RELATED_SYNONYM); if
	 * (!references.contains(s)) { references.addElement(s);
	 * referenceList.setListData(references); } } } else if (e.getData()
	 * instanceof Object[]) { Object[] v = (Object[]) e.getData(); for (int i =
	 * 0; i < v.length; i++) { Dbxref s = (Dbxref) ((Dbxref) v[i]).clone();
	 * s.setType(Dbxref.RELATED_SYNONYM); if (!references.contains(s)) {
	 * references.add(s); referenceList.setListData(references); } } } } };
	 */
	public SynonymListEditor() {
		textField = new JTextField();
		referenceList = new JList();
		referenceLabel = new JLabel("DbXrefs");
		referenceButton = new JButton("Edit");
		typeList = new JComboBox(TYPES);
		categoryList = new JComboBox();

		// referenceList.setPreferredSize(new Dimension(40,100));
		// referenceList.setMinimumSize(new Dimension(40,100));
		referenceLabel.setFont(getFont());
		referenceButton.setFont(getFont());
		typeList.setFont(getFont());
		textField.setFont(getFont());

		textField.setMaximumSize(new Dimension(Integer.MAX_VALUE, 16));

		initGUI();
		attachListeners();
	}

	@Override
	public void setEnabled(boolean enable) {
		super.setEnabled(enable);
		textField.setEnabled(enable);
		typeList.setEnabled(enable);
		categoryList.setEnabled(enable);
		// editor.setEnabled(enable);
		referenceList.setEnabled(enable);
		referenceLabel.setEnabled(enable);
		referenceButton.setEnabled(enable);
	}

	public void showDbxrefEditor() {
		JTextArea noDbxLabel = new JTextArea("Select a dbxref from the list "
				+ "to edit it, or press add to " + "create a new dbxref");
		noDbxLabel.setOpaque(false);
		noDbxLabel.setLineWrap(true);
		noDbxLabel.setWrapStyleWord(true);
		noDbxLabel.setEditable(false);
		noDbxLabel.setEnabled(false);

		final JDialog dialog = new JDialog((Frame) null,
				"Edit synonym dbxrefs", true);
		final ListEditor editor = new ListEditor(new DbxrefListEditor(
				Dbxref.RELATED_SYNONYM), noDbxLabel, references,
				true, true, true, true, false);
		editor.setOpaque(false);

		JButton okButton = new JButton("Ok");

		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				editor.commit();
				dialog.setVisible(false);
			}
		});

		Box buttonPanel = Box.createHorizontalBox();
		buttonPanel.add(Box.createHorizontalGlue());
		buttonPanel.add(okButton);
		buttonPanel.add(Box.createHorizontalGlue());

		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());
		panel.add(editor, "Center");
		panel.add(buttonPanel, "South");

		dialog.setContentPane(panel);
		dialog.pack();
		dialog.show();

		referenceList.setListData(references);
		commit();
	}

	public void setMasterComponent(Component c) {
		if (c instanceof ListEditor)
			editor = (ListEditor) c;
	}

	public void initGUI() {
		setLayout(new BorderLayout());

		JPanel textPanel = new JPanel();
		textPanel.setLayout(new BoxLayout(textPanel, BoxLayout.Y_AXIS));
		textPanel.setOpaque(false);

		referenceButton.setPreferredSize(new Dimension((int) referenceButton
				.getPreferredSize().getWidth(), referenceButton.getFont()
				.getSize()));
		
		JPanel referenceBox = new JPanel();
		referenceBox.setOpaque(false);
		referenceBox.setLayout(new BoxLayout(referenceBox, BoxLayout.X_AXIS));
		referenceBox.add(referenceLabel);
		referenceBox.add(Box.createHorizontalGlue());
		referenceBox.add(referenceButton);

		JLabel textLabel = new JLabel("Synonym text");
		JLabel typeLabel = new JLabel("Synonym scope");
		JLabel categoryLabel = new JLabel("Synonym type");
		textLabel.setFont(getFont());
		typeLabel.setFont(getFont());
		categoryLabel.setFont(getFont());
		categoryList.setFont(getFont());

		textPanel.add(textLabel);
		textPanel.add(textField);
		textPanel.add(Box.createVerticalStrut(5));
		textPanel.add(typeLabel);
		textPanel.add(typeList);
		textPanel.add(Box.createVerticalStrut(5));
		textPanel.add(categoryLabel);
		textPanel.add(categoryList);
		textPanel.add(Box.createVerticalStrut(5));
		textPanel.add(referenceBox);

		typeList.setAlignmentX(LEFT_ALIGNMENT);
		categoryList.setAlignmentX(LEFT_ALIGNMENT);
		categoryLabel.setAlignmentX(LEFT_ALIGNMENT);
		referenceBox.setAlignmentX(LEFT_ALIGNMENT);
		textField.setAlignmentX(LEFT_ALIGNMENT);
		textLabel.setAlignmentX(LEFT_ALIGNMENT);
		textPanel.setAlignmentX(LEFT_ALIGNMENT);
		referenceButton.setAlignmentX(LEFT_ALIGNMENT);
		referenceList.setAlignmentX(LEFT_ALIGNMENT);
		referenceLabel.setAlignmentX(LEFT_ALIGNMENT);

		referenceList.setFont(getFont());

		JScrollPane referencePane = new JScrollPane(referenceList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		/*
		 * referencePane.setMinimumSize(new Dimension(50,300));
		 * referencePane.getVerticalScrollBar(). addAdjustmentListener(new
		 * AdjustmentListener() { public void
		 * adjustmentValueChanged(AdjustmentEvent e) {
		 * System.err.println("adjusted "+e); } });
		 * referencePane.getVerticalScrollBar().setVisibleAmount(1);
		 */
		add(textPanel, "North");
		add(referencePane, "Center");
	}

	protected void deleteSelectedReferences() {
		Object[] selected = referenceList.getSelectedValues();
		for (int i = 0; i < selected.length; i++) {
			references.removeElement(selected[i]);
		}
		referenceList.setListData(references);
	}

	public void attachListeners() {
		textField.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				commit();
			}
		});
		categoryList.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (categoryList.getSelectedIndex() < 1) {
					typeList.setEnabled(isEnabled());
				} else {
					SynonymCategory cat = (SynonymCategory) categoryList
							.getSelectedItem();
					if (cat.getScope() != Synonym.UNKNOWN_SCOPE) {
						typeList.setSelectedIndex(cat.getScope());
						typeList.setEnabled(false);
					} else {
						typeList.setEnabled(isEnabled());
					}
				}
			}
		});

		referenceList.registerKeyboardAction(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				deleteSelectedReferences();
			}
		}, KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0), WHEN_FOCUSED);
		referenceButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				showDbxrefEditor();
			}
		});
	}

	public void commit() {
		editor.doStore(obj);
		editor.refresh();
	}

	public void store(Object in) {
		Synonym syn = (Synonym) in;
		syn.setText(textField.getText());
		syn.getDbxrefs().clear();
		Iterator it = references.iterator();

		while (it.hasNext())
			syn.getDbxrefs().add((Dbxref) it.next());

		// syn.setDbxrefs(references);
		syn.setScope(typeList.getSelectedIndex());
		if (categoryList.getSelectedIndex() < 1)
			syn.setSynonymCategory(null);
		else
			syn.setSynonymCategory((SynonymCategory) categoryList
					.getSelectedItem());
	}

	public Object createNewValue() {
		return SessionManager.getManager().getSession().getObjectFactory()
				.createSynonym("<new synonym>", Synonym.RELATED_SYNONYM);
	}

	public void load(Object in) {
		obj = in;
		Synonym syn = (Synonym) obj;

		categoryList.removeAllItems();
		categoryList.addItem("<no synonym category>");
		Iterator it = SessionManager.getManager().getSession()
				.getSynonymCategories().iterator();
		while (it.hasNext()) {
			SynonymCategory cat = (SynonymCategory) it.next();
			categoryList.addItem(cat);
		}

		textField.setText(syn.getText());
		typeList.setSelectedIndex(syn.getScope());
		references = new Vector();
		references.addAll(syn.getDbxrefs());
		referenceList.setListData(references);

		if (syn.getSynonymCategory() == null)
			categoryList.setSelectedIndex(0);
		else
			categoryList.setSelectedItem(syn.getSynonymCategory());

		revalidate();
		repaint();
	}
}

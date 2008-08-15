package org.oboedit.gui.widget;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import org.bbop.swing.*;
import org.obo.datamodel.*;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;

import org.apache.log4j.*;

public class DbxrefListEditor extends JPanel implements GenericEditorComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DbxrefListEditor.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	JTextField dbnameField;
	JTextField idField;
	JTextField descField;

	int defaultType;
	Dbxref ref;
	ListEditor editor;

	public DbxrefListEditor() {
		this(-1);
	}

	public DbxrefListEditor(int defaultType) {
		this.defaultType = defaultType;

		dbnameField = new JTextField();
		idField = new JTextField();
		descField = new JTextField();

		buildGUI();
		attachListeners();
	}

	@Override
	public void setEnabled(boolean enable) {
		super.setEnabled(enable);
		dbnameField.setEnabled(enable);
		idField.setEnabled(enable);
		descField.setEnabled(enable);
	}

	public void setMasterComponent(Component c) {
		if (c instanceof ListEditor)
			editor = (ListEditor) c;
	}

	public void buildGUI() {
		JPanel viewable = new JPanel();
		viewable.setOpaque(false);
		viewable.setLayout(new BoxLayout(viewable, BoxLayout.Y_AXIS));
		setMinimumSize(new Dimension(0, 0));
		JLabel dbnameLabel = new JLabel("Database name");
		JLabel idLabel = new JLabel("ID");
		JLabel descLabel = new JLabel("Description");
//		JLabel typeLabel = new JLabel("Dbxref type");  // Not used

		idLabel.setAlignmentX(0);
		dbnameLabel.setAlignmentX(0);
		descLabel.setAlignmentX(0);
//		typeLabel.setAlignmentX(0);
		dbnameField.setAlignmentX(0);
		descField.setAlignmentX(0);
		idField.setAlignmentX(0);

		JPanel dbnamePanel = new JPanel();
		dbnamePanel.setLayout(new BoxLayout(dbnamePanel, BoxLayout.Y_AXIS));
		dbnamePanel.setOpaque(false);
		dbnamePanel.add(dbnameLabel);
		dbnamePanel.add(dbnameField);

		JPanel idPanel = new JPanel();
		idPanel.setOpaque(false);
		idPanel.setLayout(new BoxLayout(idPanel, BoxLayout.Y_AXIS));
		idPanel.add(idLabel);
		idPanel.add(idField);

		JPanel topPanel = new JPanel();
		topPanel.setLayout(new BoxLayout(topPanel, BoxLayout.X_AXIS));
		topPanel.setOpaque(false);
		topPanel.add(dbnamePanel);
		topPanel.add(Box.createHorizontalStrut(10));
		topPanel.add(idPanel);

		JPanel descPanel = new JPanel();
		descPanel.setLayout(new BoxLayout(descPanel, BoxLayout.Y_AXIS));
		descPanel.setOpaque(false);
		descPanel.add(descLabel);
		descPanel.add(descField);

		JPanel bottomPanel = new JPanel();
		bottomPanel.setLayout(new BoxLayout(bottomPanel, BoxLayout.X_AXIS));
		bottomPanel.setOpaque(false);
		bottomPanel.add(descPanel);

		viewable.add(topPanel);
		viewable.add(Box.createHorizontalStrut(20));
		viewable.add(bottomPanel);

		setLayout(new BorderLayout());
		add(viewable, "North");
		setOpaque(false);
		validate();
	}

	public void attachListeners() {
		try {
			ActionListener commitListener = new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					commit();
				}
			};
			FocusListener focusListener = new FocusListener() {
				public void focusGained(FocusEvent e) {
				}

				public void focusLost(FocusEvent e) {
					commit();
				}
			};
			dbnameField.addActionListener(commitListener);
			idField.addActionListener(commitListener);
			descField.addActionListener(commitListener);
			dbnameField.addFocusListener(focusListener);
			idField.addFocusListener(focusListener);
			descField.addFocusListener(focusListener);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public void commit() {
		if (editor != null) {
			editor.doStore(ref);
			editor.refresh();
		}
	}

	public void load(Object o) {
		ref = (Dbxref) o;
		dbnameField.setText(ref.getDatabase());
		idField.setText(ref.getDatabaseID());
		descField.setText(ref.getDesc());
	}

	public void store(Object saveme) {
		Dbxref ref = (Dbxref) saveme;
		ref.setDatabase(dbnameField.getText());
		ref.setDatabaseID(idField.getText());
		if (descField.getText().length() == 0)
			ref.setDesc(null);
		else
			ref.setDesc(descField.getText());
		if (defaultType == -1)
			ref.setType(Dbxref.ANALOG);
		else
			ref.setType(defaultType);
	}

	public Object createNewValue() {
		return SessionManager.getManager().getSession().getObjectFactory().createDbxref("XX",
				"<new dbxref>", null, Dbxref.ANALOG, null);
	}
}

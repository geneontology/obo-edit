package org.bbop.swing;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.undo.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

import org.bbop.util.*;

/**
 * A component for editing vectors of objects
 *
 */

import org.apache.log4j.*;

public class ListEditor extends JPanel {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ListEditor.class);

    /**
	 * 
	 */
	private static final long serialVersionUID = 1651813950977682834L;
	protected boolean listResizable;
    protected boolean editable;
    protected boolean vectorEditable;
    protected boolean undoable;
    protected GenericEditorComponent editor;
    protected Vector original_data;
    protected Vector data;
    protected ListSelectionListener listListener;
    protected JList valueList;
    protected Object currentEditValue;
    protected StateEdit currentEditTransaction;
    private JButton addButton;
    private JButton delButton;
    private JPanel listPanel;
    protected CompoundEdit masterUndolist;
    protected Component noSelectionComponent;
    private JSplitPane resizablePane;
    protected boolean autocommit;
    private boolean showingEditor;
    protected Color buttonColor;
    protected int minDividerSize = 30;
    protected int defaultDividerSize = 200;
    protected int dividerLoc = -1;
    protected JScrollPane listScrollPane;

    public static final int VERTICAL_SPLIT = JSplitPane.VERTICAL_SPLIT;
    public static final int HORIZONTAL_SPLIT = JSplitPane.HORIZONTAL_SPLIT;

    protected Vector listeners;

    @Override
	public void setEnabled(boolean enabled) {
	super.setEnabled(enabled);
//	valueList.setEnabled(enabled);
	addButton.setEnabled(enabled);
	delButton.setEnabled(enabled);
	valueList.setEnabled(enabled);
	if (editor instanceof Component)
	    ((Component) editor).setEnabled(enabled);
    }

    public ListEditor(GenericEditorComponent editor) {
	this(editor, new Vector(0));
    }

    public ListEditor(GenericEditorComponent editor,
		      boolean listResizable,
		      boolean editable,
		      boolean vectorEditable,
		      boolean autocommit,
		      boolean undoable) {
	this(editor, null, new Vector(0),
	     listResizable,
	     editable,
	     vectorEditable,
	     autocommit,
	     undoable);
    }

    public ListEditor(GenericEditorComponent editor, Vector data) {
	this(editor, null, data,
	     true,true,false,false,true);
    }

    public ListEditor(GenericEditorComponent editor,
		      Component noSelectionComponent,
		      Vector data,
		      boolean listResizable,
		      boolean editable,
		      boolean vectorEditable,
		      boolean autocommit,
		      boolean undoable) {
	this.autocommit = autocommit;
	this.listResizable = listResizable;
	this.editable = editable;
	this.vectorEditable = vectorEditable;
	this.editor = editor;
	editor.setMasterComponent(this);
	if (noSelectionComponent == null) {
	    this.noSelectionComponent = defaultNoSelectionComponent();
	    this.noSelectionComponent.setFont(getFont());
	} else
	    this.noSelectionComponent = noSelectionComponent;
	setData(data, false);
	this.undoable = undoable;
	valueList = new JList(data);
	listeners = new Vector();
	valueList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
	showingEditor = true;
	// dividerLoc = defaultDividerSize;
	buildGUI();
    }

    public void setOrientation(int orientation) {
	resizablePane.setOrientation(orientation);
    }

    public JList getList() {
	return valueList;
    }

    public void setDefaultDividerLoc(int loc) {
	defaultDividerSize = loc;
	updateGUI();
    }

    public void addListSelectionListener(ListSelectionListener in) {
	listeners.addElement(in);
    }

    public void removeListSelectionListener(ListSelectionListener in) {
	listeners.removeElement(in);
    }

    protected void fireListSelectionEvent(ListSelectionEvent e) {
	for(int i=0; i < listeners.size(); i++) {
	    ListSelectionListener lsl = (ListSelectionListener)
		listeners.elementAt(i);
	    lsl.valueChanged(e);
	}
    }

    @Override
	public void setBackground(Color color) {
	super.setBackground(color);
	if (listPanel != null)
	    listPanel.setBackground(color);
	if (resizablePane != null)
	    resizablePane.setBackground(color);
    }

    public void setButtonColor(Color buttonColor) {
	this.buttonColor = buttonColor;
	if (addButton != null)
	    addButton.setBackground(buttonColor);
	if (delButton != null)
	    delButton.setBackground(buttonColor);
    }

    public Object getSelectedItem() {
	return valueList.getSelectedValue();
    }

    public Vector getData() {
	return data;
    }

    public void setVectorEditable(boolean vectorEditable) {
	this.vectorEditable = vectorEditable;
	refresh();
    }

    private void buildUndoObjects() {
	masterUndolist = new CompoundEdit();
	masterUndolist.addEdit(new StateEdit(new VectorWrapper(original_data)));
    }

    public static Component defaultNoSelectionComponent() {
	JLabel label = new JLabel("Select an item from the list");
	return label;
    }

    @Override
	public void setFont(Font in) {
	super.setFont(in);
	fontify();
    }

    protected void fontify() {
	if (valueList != null)
	    valueList.setFont(getFont());
	if (addButton != null)
	    addButton.setFont(getFont());
	if (delButton != null)
	    delButton.setFont(getFont());
    }

    public void doStore(Object currentEditValue) {
	if (editable) {
	    if (currentEditValue != null) {
		editor.store(currentEditValue);
		// currentEditValue = null;
	    }
	    valueList.validate();
	}
	if (currentEditTransaction != null) {
	    currentEditTransaction.end();
	    masterUndolist.addEdit(currentEditTransaction);
	    currentEditTransaction = null;
	}
    }

    public void add() {
	if (editable) {
	    Object obj = editor.createNewValue();
	    data.addElement(obj);
	    valueList.setListData(data);
	    valueList.setSelectedValue(obj, true);	    
	    updateGUI();
	    /*
	    valueList.updateUI();
	    logger.info(data);
	    */
	}
    }

    public void select(int index) {
	valueList.setSelectedIndex(index);
    }

    public void del() {
	Object value = valueList.getSelectedValue();
	int index = valueList.getSelectedIndex();
	if (editable &&
	    value != null) {
	    data.removeElement(value);
	    valueList.setListData(data);
	    valueList.clearSelection();
	    if (data.size() > 0 && index >= data.size()) {
		valueList.setSelectedIndex(index - 1);
		valueList.ensureIndexIsVisible(index - 1);
	    } else if (data.size() > 0) {
		valueList.setSelectedIndex(index);
		valueList.ensureIndexIsVisible(index);
	    }
	    refresh();
	}
    }

    public void setShowListPanel(boolean show) {
	if (listResizable) {
	    if (show) {
		resizablePane.setLeftComponent(listPanel);
		resizablePane.setDividerLocation(dividerLoc);
	    } else {
		if (resizablePane.getLeftComponent() != null)
		    dividerLoc = resizablePane.getDividerLocation();
		resizablePane.setLeftComponent(null);
		resizablePane.setDividerLocation(0);
	    }
	} else {
	    if (show) {
		add("West", listPanel);
	    } else
		remove(listPanel);
	    validate();
	}
	repaint();
    }

    public void setData(Vector vector) {
	setData(vector, true);
    }

    protected void setData(Vector vector, boolean redraw) {
	this.original_data = vector;
	if (vectorEditable)
	    this.data = (Vector) original_data.clone();
	else
	    this.data = original_data;
	if (valueList != null) {
	    Object oldSelection = valueList.getSelectedValue();
	    valueList.setListData(data);
	    valueList.setSelectedValue(oldSelection, true);
	}
	if (editable && undoable) {
	    buildUndoObjects();
	}
	if (redraw)
	    refresh();
    }

    protected void updateGUI() {	
	boolean showEditor = (valueList.getSelectedIndex() != -1);
	if (listResizable) {
	    if (resizablePane.getLeftComponent() != null) {
		dividerLoc = resizablePane.getDividerLocation();
	    }
	    if (showEditor)
		resizablePane.setRightComponent((Component) editor);
	    else
		resizablePane.setRightComponent(noSelectionComponent);

	    int maxDividerSize;
	    if (resizablePane.getOrientation() == JSplitPane.HORIZONTAL_SPLIT)
		maxDividerSize = (int) resizablePane.getSize().getWidth() - 
		    (int) ((Component) editor).getPreferredSize().getWidth();
	    else
		maxDividerSize = (int) resizablePane.getSize().getHeight() -
		    (int) ((Component) editor).getPreferredSize().getHeight();
/*
	    if (dividerLoc > maxDividerSize)
		dividerLoc = maxDividerSize - 10;

	    if (dividerLoc < minDividerSize)
		dividerLoc = defaultDividerSize;
*/
	    if (resizablePane.getLeftComponent() != null) {
		resizablePane.setDividerLocation(dividerLoc);
	    }
	} else {
	    if (showEditor) {
		remove(noSelectionComponent);
		add((Component) editor, "Center");
	    } else if (!showEditor) {
		remove((Component) editor);
		add(noSelectionComponent, "Center");
	    }
	    validate();
	}
	repaint();
	if (vectorEditable) {
	    delButton.setEnabled(isEnabled() && !valueList.isSelectionEmpty());
	}
    }

    public void reload() {
	valueList.setSelectedIndex(valueList.getSelectedIndex());
	//	editor.load(currentEditValue);
	refresh();
    }

    public void handleEdit(Object object) {
	if (autocommit)
	    doStore(currentEditValue);
	updateGUI();
	if (object == null) {
	    return;
	}

	if (undoable && (object instanceof StateEditable))
	    currentEditTransaction = new StateEdit((StateEditable) object);
	currentEditValue = object;
	editor.load(currentEditValue);
    }

    public void refresh() {
	updateGUI();
	if (editor != null && currentEditValue != null)
	    editor.load(currentEditValue);
	repaint();
    }

    public UndoableEdit commit() {
	return commit(currentEditValue);
    }

    public UndoableEdit commit(Object commitme) {
	doStore(commitme);
	refresh();
	//currentEditValue = null;
	currentEditTransaction = null;
	if (vectorEditable) {
	    original_data.removeAllElements();
	    VectorUtil.mergeVectors(original_data, data);
	}
	if (undoable) {
	    CompoundEdit committedUndo = masterUndolist;
	    committedUndo.end();
	    buildUndoObjects();
	    return committedUndo;
	} else
	    return null;
    }

    protected void buildGUI() {
	setOpaque(false);
	listListener = new PickListener(this);
	valueList.addListSelectionListener(listListener);
	listPanel = getListPanel();
	setLayout(new BorderLayout(3,0));
	if (listResizable) {
	    resizablePane = new JSplitPane();
	    resizablePane.setDividerSize(3);	    
	    resizablePane.setLeftComponent(null);
	    resizablePane.setDividerLocation(dividerLoc);
	    add(resizablePane, "Center");
	}
	setShowListPanel(true);
	fontify();
	/*
	if (editor instanceof JComponent)
	    ((JComponent) editor).setMinimumSize(listPanel.getSize());
	    */
	updateGUI();
    }

    protected JPanel getListPanel() {
	JPanel listPanel = new JPanel();
	listPanel.setLayout(new BorderLayout());
	listScrollPane = new JScrollPane(valueList);
	listScrollPane.setPreferredSize(new Dimension(200, 300));
	listScrollPane.setMinimumSize(new Dimension(100, 300));
	listPanel.add(listScrollPane, "Center");
	if (vectorEditable) {
	    initButtons();
	    JPanel buttonPanel = new JPanel();
	    buttonPanel.setOpaque(false);
	    buttonPanel.add(addButton);
	    buttonPanel.add(delButton);
	    listPanel.add(buttonPanel, "South");
		listPanel.setMinimumSize(buttonPanel.getPreferredSize());
		minDividerSize = (int) buttonPanel.getPreferredSize().getWidth();
	}
	listPanel.setOpaque(false);
	return listPanel;
    }

    public void setListSize(int size) {
	listScrollPane.setPreferredSize(new Dimension(size, size));
	dividerLoc = size;
	resizablePane.setDividerLocation(dividerLoc);
	revalidate();
    }

    protected void initButtons() {
	if (addButton == null) {
	    addButton = new JButton("Add");
	    addButton.addActionListener(new AddButtonListener(this));
	}
	if (delButton == null) {
	    delButton = new JButton("Del");
	    delButton.addActionListener(new DelButtonListener(this));
	}
    }

    public void setEditable(boolean in) {
	editable = in;
    }

    public void hideList() {
	setShowListPanel(false);
    }

    public void showList() {
	setShowListPanel(true);
    }

    public void undo() {
	UndoManager undoman = new UndoManager();
	UndoableEdit edit = commit();
	undoman.addEdit(edit);
	undoman.undo();
	buildUndoObjects();
	refresh();
	repaint();
    }

    public static Vector junkVector() {
	Vector out = new Vector();
	out.addElement(new EditableString("Hello"));
	out.addElement(new EditableString("I"));
	out.addElement(new EditableString("love"));
	out.addElement(new EditableString("peaches"));
	out.addElement(new EditableString("huh?"));
	return out;
    }
}

class VectorWrapper implements StateEditable {
    private Vector vector;

    public VectorWrapper(Vector in) {
	vector = in;
    }

    public void storeState(Hashtable state) {
	state.put("vector", vector.clone());	
    }

    public void restoreState(Hashtable state) {
	if (state != null) {
	    Vector stored = (Vector) state.get("vector");
	    if (stored != null) {
		vector.removeAllElements();
		for(int i=0; i < stored.size(); i++)
		    vector.addElement(stored.elementAt(i));
	    }	    
	}
    }
}

class AddButtonListener implements ActionListener {
    ListEditor editor;

    public AddButtonListener(ListEditor editor) {
	this.editor = editor;
    }

    public void actionPerformed(ActionEvent e) {
	editor.add();
    }
}

class DelButtonListener implements ActionListener {
    ListEditor editor;

    public DelButtonListener(ListEditor editor) {
	this.editor = editor;
    }

    public void actionPerformed(ActionEvent e) {
	editor.del();
    }
}

class PickListener implements ListSelectionListener {

    ListEditor editor;

    public PickListener(ListEditor editor) {
	this.editor = editor;
    }
    
    public void valueChanged(ListSelectionEvent e) {
	JList list = (JList) e.getSource();
	Object selection = list.getSelectedValue();	
	editor.handleEdit(selection);
	editor.fireListSelectionEvent(e);
    }
}

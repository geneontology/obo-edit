package org.bbop.swing.widget;

import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.plaf.basic.*;
import java.io.*;
import java.awt.*;
import java.awt.event.*;

import org.bbop.io.AllFileFilter;
import org.bbop.swing.*;

public class MultiFileChooser extends JPanel {

    /**
	 * 
	 */
	private static final long serialVersionUID = 3285835745137459354L;
	protected static int MARGIN = 5;

    protected static class UIExtension extends BasicFileChooserUI {
	public UIExtension() {
	    super(new JFileChooser());
	}

	public Icon getFileIcon() {
	    return UIExtension.this.fileIcon;
	}

	public Icon getComputerIcon() {
	    return UIExtension.this.computerIcon;
	}

	public Icon getDirectoryIcon() {
	    return UIExtension.this.directoryIcon;
	}

	public Icon getHardDriveIcon() {
	    return UIExtension.this.hardDriveIcon;
	}

	public Icon getFloppyDriveIcon() {
	    return UIExtension.this.floppyDriveIcon;
	}

	public Icon getHomeFolderIcon() {
	    return UIExtension.this.homeFolderIcon;
	}

	public Icon getUpFolderIcon() {
	    return UIExtension.this.upFolderIcon;
	}
    }

    protected class FileCellRenderer extends DefaultTreeCellRenderer {

	/**
		 * 
		 */
		private static final long serialVersionUID = 4350638134436017539L;

	protected void setIcons(Icon icon) {
	    setOpenIcon(icon);
	    setClosedIcon(icon);
	    setLeafIcon(icon);
	}

	public Component getTreeCellRendererComponent(JTree tree,
						      Object value,
						      boolean sel,
						      boolean expanded,
						      boolean leaf,
						      int row,
						      boolean hasFocus) {
	    if (value instanceof FileSystemTreeModel.DummyRoot) {
		setIcons(computerIcon);
	    }
	    String fileName = value.toString();
	    if (value instanceof File) {
		File file = (File) value;
		if (file.isDirectory())
		    setIcons(directoryIcon);
		else
		    setIcons(fileIcon);
		if (!model.getRoot().equals(file))
		    fileName = file.getName();
	    }
	    return super.getTreeCellRendererComponent(tree, fileName,
						      sel, expanded, leaf,
						      row, hasFocus);
	}
    }

    protected class ButtonListener implements ActionListener {
	protected int option;
	protected JDialog dialog;

	public ButtonListener(int option, JDialog dialog) {
	    this.option = option;
	    this.dialog = dialog;
	}

	public void actionPerformed(ActionEvent e) {
	    choice = option;
	    dialog.hide();
	}
    }

    public final static int APPROVE = 1;
    public final static int CANCEL = 2;

    private UIExtension ext = new UIExtension();

    protected FileSystemTreeModel model;
    protected JTree tree;
    protected JScrollPane pane;

    protected Icon fileIcon = UIManager.getIcon("FileView.fileIcon");
    protected Icon directoryIcon = UIManager.getIcon("FileView.directoryIcon");
    protected Icon hardDriveIcon = UIManager.getIcon("FileView.hardDriveIcon");
    protected Icon floppyDriveIcon = UIManager.getIcon("FileView.floppyDriveIcon");
    protected Icon computerIcon = UIManager.getIcon("FileView.computerIcon");
    protected Icon homeFolderIcon = UIManager.getIcon("FileChooser.homeFolderIcon");
    protected Icon upFolderIcon = UIManager.getIcon("FileChooser.upFolderIcon");

    protected JButton openButton = new JButton();
    protected JButton cancelButton = new JButton("Cancel");
    protected JButton upFolderButton = new JButton(upFolderIcon);
    protected JButton rootFolderButton = new JButton(computerIcon);
    protected JButton homeFolderButton = new JButton(homeFolderIcon);

    protected JLabel fileNameLabel = new JLabel();

    protected JComboBox filterBox = new JComboBox();

    protected int choice = CANCEL;

    protected FilenameFilter [] filters;

    protected void attachListeners() {
	upFolderButton.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    showParent();
		}
	    });
	rootFolderButton.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    showRoot();
		}
	    });
	homeFolderButton.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    showHome();
		}
	    });
	filterBox.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    selectFilter();
		}
	    });
    }

    protected void selectFilter() {
	model.setFilter((FilenameFilter) filterBox.getSelectedItem());
    }

    protected void showParent() {
	File file = (File) model.getRoot();
	model.setRoot(file.getParentFile());
	initButtons();
    }

    protected void showRoot() {
	model.setRoot(model.getDefaultRoot());
	initButtons();
    }

    protected void showHome() {
	model.setRoot(new File(System.getProperty("user.home")));
	initButtons();
    }

    public File getRoot() {
	Object o = model.getRoot();
	if (!(o instanceof File))
	    return null;
	else
	    return (File) o;
    }

    public void setFilters(FilenameFilter [] filters) {
	this.filters = filters;
	filterBox.removeAllItems();
	filterBox.setSelectedIndex(-1);
	for(int i=0; i < filters.length; i++) {
	    filterBox.addItem(filters[i]);
	}
	filterBox.setSelectedIndex(0);
    }

    protected void initButtons() {
	fileNameLabel.setText(model.getRoot().toString());
	rootFolderButton.
	    setEnabled(!model.getRoot().equals(model.getDefaultRoot()));
	homeFolderButton.
	    setEnabled(!model.getRoot().
		       equals(new File(System.getProperty("user.home"))));
	upFolderButton.
	    setEnabled((model.getRoot() instanceof File) &&
		       (((File) model.getRoot()).getParentFile() != null));
    }

    public MultiFileChooser() {
	this(new File(System.getProperty("user.home")));
    }

    public void setCellRenderer(TreeCellRenderer renderer) {
	tree.setCellRenderer(renderer);
    }

    public MultiFileChooser(File root) {
	setLayout(new BorderLayout());

	setPreferredSize(new Dimension(400,350));

	FilenameFilter [] defaultFilters = { new AllFileFilter() };

	setFilters(defaultFilters);

	model = new FileSystemTreeModel(root);
	tree = new JTree(model);
	setCellRenderer(new FileCellRenderer());

	pane =
	    new JScrollPane(tree,
			    JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			    JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

	fileNameLabel.setText(root.toString());

	JPanel horzBox = new JPanel();
	horzBox.setLayout(new BoxLayout(horzBox, BoxLayout.X_AXIS));
	horzBox.add(Box.createHorizontalStrut(MARGIN));
	horzBox.add(Box.createHorizontalGlue());
	horzBox.add(openButton);
	horzBox.add(Box.createVerticalStrut(15));
	horzBox.add(cancelButton);
	horzBox.add(Box.createHorizontalGlue());
	horzBox.add(Box.createHorizontalStrut(MARGIN));

	JPanel filterSpacingBox = new JPanel();
	filterSpacingBox.setLayout(new BoxLayout(filterSpacingBox,
						 BoxLayout.X_AXIS));
	filterSpacingBox.add(Box.createHorizontalStrut(MARGIN));
	filterSpacingBox.add(filterBox);
	filterSpacingBox.add(Box.createHorizontalStrut(MARGIN));

	JPanel buttonBox = new JPanel();
	buttonBox.setLayout(new BoxLayout(buttonBox, BoxLayout.Y_AXIS));
	buttonBox.add(Box.createVerticalStrut(10));
	buttonBox.add(filterSpacingBox);
	buttonBox.add(Box.createVerticalStrut(5));
	buttonBox.add(horzBox);
	buttonBox.add(Box.createVerticalStrut(MARGIN));

	JPanel pathButtonBox = new JPanel();
	pathButtonBox.setLayout(new BoxLayout(pathButtonBox, BoxLayout.X_AXIS));
	pathButtonBox.add(Box.createHorizontalStrut(MARGIN));
	pathButtonBox.add(fileNameLabel);
	pathButtonBox.add(Box.createHorizontalStrut(10));
	pathButtonBox.add(Box.createHorizontalGlue());
	pathButtonBox.add(rootFolderButton);
	pathButtonBox.add(Box.createHorizontalStrut(10));
	pathButtonBox.add(upFolderButton);
	pathButtonBox.add(Box.createHorizontalStrut(10));
	pathButtonBox.add(homeFolderButton);
	pathButtonBox.add(Box.createHorizontalStrut(MARGIN));
	
	JPanel northBox = new JPanel();
	northBox.setLayout(new BoxLayout(northBox, BoxLayout.Y_AXIS));
	northBox.add(Box.createVerticalStrut(MARGIN));
	northBox.add(pathButtonBox);
	northBox.add(Box.createVerticalStrut(10));

	JPanel paneBox = new JPanel();
	paneBox.setLayout(new BorderLayout());
	paneBox.add(Box.createHorizontalStrut(MARGIN), "East");
	paneBox.add(pane, "Center");
	paneBox.add(Box.createHorizontalStrut(MARGIN), "West");

	filterSpacingBox.setAlignmentX(LEFT_ALIGNMENT);
	paneBox.setAlignmentX(LEFT_ALIGNMENT);
	pane.setAlignmentX(LEFT_ALIGNMENT);
	tree.setAlignmentX(LEFT_ALIGNMENT);
	openButton.setAlignmentX(LEFT_ALIGNMENT);
	cancelButton.setAlignmentX(LEFT_ALIGNMENT);
        fileNameLabel.setAlignmentX(LEFT_ALIGNMENT);
	rootFolderButton.setAlignmentX(LEFT_ALIGNMENT);
	upFolderButton.setAlignmentX(LEFT_ALIGNMENT);
	homeFolderButton.setAlignmentX(LEFT_ALIGNMENT);
	filterBox.setAlignmentX(LEFT_ALIGNMENT);
	horzBox.setAlignmentX(LEFT_ALIGNMENT);
	buttonBox.setAlignmentX(LEFT_ALIGNMENT);
	pathButtonBox.setAlignmentX(LEFT_ALIGNMENT);
	northBox.setAlignmentX(LEFT_ALIGNMENT);

	add(northBox, "North");
	add(paneBox, "Center");
	add(buttonBox, "South");

	attachListeners();
	initButtons();
    }

    public void setSelectedFiles(File [] files) {
	if (files == null)
	    return;
	TreePath [] paths = getTreePaths(files);
	tree.setSelectionPaths(paths);
	for(int i=0; i < paths.length; i++)
	    tree.makeVisible(paths[i]);
    }

    protected TreePath [] getTreePaths(File [] files) {
	TreePath [] paths = new TreePath[files.length];
	for(int i=0; i < files.length; i++) {
	    paths[i] = model.getTreePath(files[i]);
	}
	return paths;
    }

    protected void attachListeners(JDialog dialog) {
	cancelButton.addActionListener(new ButtonListener(CANCEL,
							  dialog));
	openButton.addActionListener(new ButtonListener(APPROVE,
							dialog));
    }

    public void setFont(Font font) {
	super.setFont(font);
	if (tree != null)
	    tree.setFont(font);
	if (cancelButton != null)
	    cancelButton.setFont(font);
	if (openButton != null)
	    openButton.setFont(font);
	if (filterBox != null)
	    filterBox.setFont(font);
	if (fileNameLabel != null)
	    fileNameLabel.setFont(font);
    }

    public int showDialog(Component parent, String approveButtonText) {
	openButton.setText(approveButtonText);
	JDialog dialog;

	Window w = SwingUtilities.getWindowAncestor(parent);
	if (w == null)
	    dialog = new JDialog();
	else if (w instanceof Dialog)
	    dialog = new JDialog((Dialog) w);
	else if (w instanceof Frame)
	    dialog = new JDialog((Frame) w);
	else
	    dialog = new JDialog();
	dialog.setModal(true);
	dialog.setFont(getFont());
	dialog.setContentPane(this);
	attachListeners(dialog);
	dialog.pack();
	SwingUtil.center(dialog);
	dialog.show();
	return choice;
    }

    public int showOpenDialog(Component parent) {
	return showDialog(parent, "Open");
    }
              
    public int showSaveDialog(Component parent) {
	return showDialog(parent, "Save");
    }

    public File [] getSelectedFiles() {
	TreePath [] paths = tree.getSelectionPaths();
	File [] files = new File[paths.length];
	for(int i=0; i < paths.length; i++) {
	    files[i] = (File) paths[i].getLastPathComponent();
	}
	return files;
    }
}

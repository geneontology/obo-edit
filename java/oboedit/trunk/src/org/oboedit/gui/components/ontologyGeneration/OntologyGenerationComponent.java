package org.oboedit.gui.components.ontologyGeneration;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Method;
import java.net.URLEncoder;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.LinkedBlockingQueue;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.text.JTextComponent;

import org.apache.axis2.AxisFault;
import org.apache.log4j.Logger;
import org.jdesktop.swingworker.SwingWorker;
import org.oboedit.gui.components.ontologyGeneration.extraction.DefinitionExtensionWorker;
import org.oboedit.gui.components.ontologyGeneration.extraction.PdfToTextExtraction;
import org.oboedit.gui.components.ontologyGeneration.interfaces.AbstractOntologyTermsTable;
import org.oboedit.gui.components.ontologyGeneration.interfaces.OntologyGenerationComponentServiceInterface;
import org.oboedit.gui.components.ontologyGeneration.interfaces.OntologyModelAdapterInterface;
import org.oboedit.gui.components.ontologyGeneration.interfaces.UpdateListenerInterface;
import org.oboedit.gui.components.ontologyGeneration.oboAdapter.OBOOntologyGenerationGUIComponent;
import org.oboedit.gui.components.ontologyGeneration.oboAdapter.OBOOntologyModelAdapter;
import org.oboedit.gui.components.ontologyGeneration.oboAdapter.ParentRelationEntry;

import de.tud.biotec.gopubmedDefinitionGeneration.client.GoPubMedDefinitionGeneratorStub;
import de.tud.biotec.gopubmedDefinitionGeneration.client.GoPubMedDefinitionGeneratorStub.DefinitionContainer;
import de.tud.biotec.gopubmedDefinitionGeneration.client.GoPubMedDefinitionGeneratorStub.GetDefinitions;
import de.tud.biotec.gopubmedDefinitionGeneration.client.GoPubMedDefinitionGeneratorStub.GetDefinitionsResponse;
import de.tud.biotec.gopubmedOntologyLookupService.OntologyLookupManagerPortTypeProxy;
import de.tud.biotec.gopubmedOntologyLookupService.xsd.OBOLookupRelation;
import de.tud.biotec.gopubmedOntologyLookupService.xsd.OBOLookupResult;
import de.tud.biotec.gopubmedOntologyLookupService.xsd.OBOLookupTerm;
import de.tud.biotec.gopubmedTermGenerationService.client.GoPubMedTermGenerationStub;
import de.tud.biotec.gopubmedTermGenerationService.client.GoPubMedTermGenerationStub.GenerateConceptsFromPubMedQuery;
import de.tud.biotec.gopubmedTermGenerationService.client.GoPubMedTermGenerationStub.GenerateConceptsFromPubMedQueryResponse;
import de.tud.biotec.gopubmedTermGenerationService.client.GoPubMedTermGenerationStub.GenerateConceptsFromText;
import de.tud.biotec.gopubmedTermGenerationService.client.GoPubMedTermGenerationStub.GenerateConceptsFromTextResponse;
import de.tud.biotec.gopubmedTermGenerationService.client.GoPubMedTermGenerationStub.GenerateConceptsFromWebQuery;
import de.tud.biotec.gopubmedTermGenerationService.client.GoPubMedTermGenerationStub.GenerateConceptsFromWebQueryResponse;
import de.tud.biotec.gopubmedTermGenerationService.client.GoPubMedTermGenerationStub.TextConceptRepresentation;

/**
 * Ontology Generation Component which supports the automatic generation of
 * candidate terms and candidate definitions to create candidate to be added to
 * the loaded ontologies.
 * <p>
 * Developed at the Bioinformatics Group, BIOTEC, TU Dresden, Dresden, Germany
 * </p>
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2008
 * @version 2.0, 25/06/2009
 */
public abstract class OntologyGenerationComponent<T, R> implements PropertyChangeListener, OntologyGenerationComponentServiceInterface<T, R>
{

	private final OntologyModelAdapterInterface<T, R> adapter;
	private final OBOOntologyGenerationGUIComponent guiComponent;

	public static final String PLUGIN_VERSION = "3.0";
	public static final String PLUGIN_VERSIONED_NAME = "OBO-Edit-" + OBOOntologyModelAdapter.getOboEditVersion() + "_" + PLUGIN_VERSION;
	private static final String SOURCE_PUBMED = "PUBMED";
	private static final String SOURCE_TEXT = "TEXT";
	private static final String SOURCE_FOLDER = "FOLDER";
	private static final String SOURCE_WEB = "WEB";
	// private final Color COLOR_FILTER_FIELDS = new Color(255, 255, 180);
	private final Color COLOR_TERMS_TABLE = new Color(255, 255, 240);
	private final Color COLOR_DEF_TABLE = new Color(255, 255, 240);
	private final Color COLOR_OBOTERMS_TABLE = new Color(255, 255, 240);
	// private final Color COLOR_TERMS_TABLE = new Color(255, 255, 240);
	// private final Color COLOR_DEF_TABLE = new Color(255, 240, 240);
	// private final Color COLOR_OBOTERMS_TABLE = new Color(230, 255, 230);

	private static final Logger logger = Logger.getLogger(OntologyGenerationComponent.class);
	private static final long serialVersionUID = -8206973805283628422L;

	// private String wordInclusionPattern =
	// "^<X>[\\s|-|\\.|,|;]|[\\s|-|\\.|,|;]<X>[\\s|-|\\.|,|;]|[\\s|-|\\.|,|;]<X>$";

	// Variables
	private CandidateTerm selectedCandidateTerm;
	private String id;

	// Tables
	private TermsTable candidateTermsTable;
	private TermsTable synonymTermsTable;
	private DefinitionsTable definitionTable;
	private AbstractOntologyTermsTable<T, R> ontologyTermsTable;

	// GUI related
	private ProgressBarDialog progressDlg;
	private JButton generateTermsFromPubMedButton;
	private JButton generateTermsFromWebButton;
	private JButton generateTermsFromTextButton;
	private JButton generateTermsFromFolderButton;
	private JButton generateManualDefinitionButton;
	private JButton folderChooseButton;
	private JButton showClipBoardButton;
	private JButton clearClipBoardButton;
	private JButton loadClipBoardButton;
	private JButton saveClipBoardButton;

	private JButton saveLabelButton;
	private JLabel saveLabelWarningLabel;
	private JButton saveDefButton;
	private JLabel saveDefWarningLabel;
	private JButton saveAbbrButton;
	private JButton addToOntologyButton;

	private JCheckBox checkboxShowTickedParents;
	private JCheckBox onlyShowExistingTerms = new JCheckBox();

	private JTextArea inputTextArea;
	private JTextArea editDefArea;

	private JTextField inputPubMedQueryField;
	private JTextField inputWebQueryField;
	private JTextField inputFolderLocationField;
	private FilterTextField filterTermsTextField;
	private FilterTextField searchTermsTextField;
	private FilterTextField filterDefTextField;
	private JTextField inputDefinitionGenerationField;
	private JTextField editNameTextField = new JTextField(20);
	private JTextField selectedLinkedObjectField = new JTextField(10);
	private FilterTextField filterPotentialParentsTextField;

	private JTabbedPane clipBoardTabPanel;
	private JScrollPane scrollPaneForTermsTable;
	private JScrollPane scrollPaneForSynonymTermsTable;

	// Caches and clipboard
	private static CandidateTermCache clipboard = new CandidateTermCache();
	private CandidateTermCache candidateTermCache = new CandidateTermCache();

	// Web service related
	private BlockingQueue<CandidateTerm> ontologyLookupQueue;
	private BlockingQueue<CandidateTerm> ontologyLookupChildrenQueue;
	private static GoPubMedTermGenerationStub termGenerationServiceStub;
	private static OntologyLookupManagerPortTypeProxy ontoLookupProxy;
	private static GoPubMedDefinitionGeneratorStub definitionGeneratorStub;

	private JCheckBox checkboxIncludeChildren;
	// private JCheckBox checkboxIncludeBranch;

	private OntologyLookupServiceWorker lookupServiceWorker;
	private OntologyLookupChildrenServiceWorker lookupChildrenServiceWorker;
	private JPanel termGenerationPanel;
	private JPanel definitionGenerationPanel;
	private JPanel addToOntologyPanel;

	private int selectedParentTermRow = -1;
	private String lastSource;

	/*
	 * ABSTRACT METHODS
	 */
	public abstract AbstractOntologyTermsTable<T, R> createOntologyTermsTable();

	/**
	 * Constructs a {@link OntologyGenerationComponent} instance
	 * 
	 * @param id
	 */
	public OntologyGenerationComponent(OntologyModelAdapterInterface<T, R> adapter, OBOOntologyGenerationGUIComponent guiComponent)
	{
		// this.id = id;
		this.adapter = adapter;
		this.guiComponent = guiComponent;

		this.ontologyLookupQueue = new LinkedBlockingQueue<CandidateTerm>(100)
		{
			private static final long serialVersionUID = 7448640043596470248L;
		};
		this.ontologyLookupChildrenQueue = new LinkedBlockingQueue<CandidateTerm>(3)
		{
			private static final long serialVersionUID = 1L;
		};

		// TERMS TABLE
		this.candidateTermsTable = new TermsTable(adapter, clipboard, 4, true);
		this.candidateTermsTable.setBackground(COLOR_TERMS_TABLE);
		this.candidateTermsTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

		candidateTermsTable.updateTermsTableStructure();

		// SYNONYMS TABLE
		this.synonymTermsTable = new TermsTable(adapter, clipboard, 2, false);
		this.synonymTermsTable.setBackground(COLOR_TERMS_TABLE);
		this.synonymTermsTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		this.synonymTermsTable.getColumnModel().getColumn(0).setMinWidth(50);
		this.synonymTermsTable.getColumnModel().getColumn(0).setMaxWidth(50);
		this.synonymTermsTable.getColumnModel().getColumn(0).setPreferredWidth(50);
		this.synonymTermsTable.getColumnModel().getColumn(0).setResizable(false);

		// DEFINITIONS TABLE
		this.definitionTable = new DefinitionsTable();
		this.definitionTable.setBackground(COLOR_DEF_TABLE);
		if (this.definitionTable.getColumnCount() > 2) {
			TableCellImageRenderer termInformationIconRenderer = new TableCellImageRenderer("resources/aboutIcon.png");
			termInformationIconRenderer.setToolTipText(Messages.getString("OntologyGenerationComponent.OpenDefinitionsPopupImage")); //$NON-NLS-1$
			this.definitionTable.getColumnModel().getColumn(2).setCellRenderer(termInformationIconRenderer);
		}

		// OBO TERMS TABLE
		ontologyTermsTable = createOntologyTermsTable();
		ontologyTermsTable.setBackground(COLOR_OBOTERMS_TABLE);
		ontologyTermsTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		// oboTermsTable.setPreferredScrollableViewportSize(new Dimension(200,
		// 200));
		ontologyTermsTable.setMinimumPreferedeScrollableViewportHeight(100);
		ontologyTermsTable.setMaximumPreferedeScrollableViewportHeight(300);

		this.selectedCandidateTerm = null;

		this.inputPubMedQueryField = new JTextField();
		this.inputPubMedQueryField.setMaximumSize(new Dimension(1000, 25));
		this.inputPubMedQueryField.setPreferredSize(new Dimension(100, 25));

		this.inputWebQueryField = new JTextField();
		this.inputWebQueryField.setMaximumSize(new Dimension(1000, 25));
		this.inputWebQueryField.setPreferredSize(new Dimension(100, 25));

		this.inputFolderLocationField = new JTextField();
		this.inputFolderLocationField.setMaximumSize(new Dimension(1000, 25));
		this.inputFolderLocationField.setPreferredSize(new Dimension(100, 25));
		// this.inputFolderLocationField.setEditable(false);

		this.generateTermsFromPubMedButton = new JButton("Generate Terms");
		this.generateTermsFromWebButton = new JButton("Generate Terms");
		this.generateTermsFromTextButton = new JButton("Generate Terms");
		this.generateTermsFromFolderButton = new JButton("Generate Terms");
		this.generateTermsFromFolderButton.setEnabled(false);
		this.generateManualDefinitionButton = new JButton("Generate Definitions");
		this.folderChooseButton = new JButton("Open...");
		this.showClipBoardButton = new JButton("Show");
		this.clearClipBoardButton = new JButton("Clear");
		this.loadClipBoardButton = new JButton("Load...");
		this.saveClipBoardButton = new JButton("Save...");

		this.saveAbbrButton = new JButton("Save Abbreviations");
		this.saveDefButton = new JButton("Save Definition");
		this.saveLabelButton = new JButton("Save Term Label");

		// TOOLTIPS
		this.generateTermsFromPubMedButton.setToolTipText(Messages.getString("OntologyGenerationComponent.GenerateTermsFromPubMedButton")); //$NON-NLS-1$
		this.generateTermsFromWebButton.setToolTipText(Messages.getString("OntologyGenerationComponent.GenerateTermsFromWebButton")); //$NON-NLS-1$
		this.generateTermsFromTextButton.setToolTipText(Messages.getString("OntologyGenerationComponent.GenerateTermsFromTextButton")); //$NON-NLS-1$
		this.generateTermsFromFolderButton.setToolTipText(Messages.getString("OntologyGenerationComponent.GenerateTermsFromPDFButton")); //$NON-NLS-1$
		this.generateManualDefinitionButton.setToolTipText(Messages.getString("OntologyGenerationComponent.GenerateDefinitionsButton")); //$NON-NLS-1$
		this.showClipBoardButton.setToolTipText(Messages.getString("OntologyGenerationComponent.ShowClipboardButton")); //$NON-NLS-1$
		this.clearClipBoardButton.setToolTipText(Messages.getString("OntologyGenerationComponent.ClearClipboardButton")); //$NON-NLS-1$
		this.loadClipBoardButton.setToolTipText(Messages.getString("OntologyGenerationComponent.LoadClipboardButton")); //$NON-NLS-1$
		this.saveClipBoardButton.setToolTipText(Messages.getString("OntologyGenerationComponent.SaveClipboardButton")); //$NON-NLS-1$

		this.saveAbbrButton.setToolTipText(Messages.getString("OntologyGenerationComponent.SaveAbbreviationsButton")); //$NON-NLS-1$
		this.saveDefButton.setToolTipText(Messages.getString("OntologyGenerationComponent.SaveDefinitionsButton")); //$NON-NLS-1$
		this.saveLabelButton.setToolTipText(Messages.getString("OntologyGenerationComponent.SaveLabelButton")); //$NON-NLS-1$

		this.saveDefWarningLabel = new JLabel();
		this.saveLabelWarningLabel = new JLabel();
	}

	public void initListener()
	{
		try {
			attachListeners();
		}
		catch (Exception exception) {
			throw new RuntimeException(exception);
		}
	}

	public void initClipboardAndWorker()
	{
		// init clipboard
		if (clipboard.getSize() > 0) {
			updateClipboard(candidateTermsTable);
		}

		// start worker and services
		ontoLookupProxy = new OntologyLookupManagerPortTypeProxy();
		lookupServiceWorker = new OntologyLookupServiceWorker();
		lookupServiceWorker.execute();
		lookupChildrenServiceWorker = new OntologyLookupChildrenServiceWorker();
		lookupChildrenServiceWorker.execute();
	}

	/**
	 * @param e
	 *            {@link PropertyChangeEvent}
	 * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
	 */
	public void propertyChange(PropertyChangeEvent e)
	{
		if ("progress" == e.getPropertyName()) {
			int prog = (Integer) e.getNewValue();
			if (prog == 100 && progressDlg != null) {
				progressDlg.setVisible(false);
			}
		}
	}

	/*
	 * PRIVATE METHODS
	 */
	/**
	 * Attach all listeners
	 */
	private void attachListeners() throws Exception
	{
		inputPubMedQueryField.addKeyListener(new KeyAdapter()
		{
			@Override
			public void keyPressed(KeyEvent event)
			{
				if (event.getKeyCode() == 10) {
					onClickGenerateTerms(inputPubMedQueryField);
				}
			}
		});

		inputWebQueryField.addKeyListener(new KeyAdapter()
		{
			@Override
			public void keyPressed(KeyEvent event)
			{
				if (event.getKeyCode() == 10) {
					onClickGenerateTerms(inputWebQueryField);
				}
			}
		});

		inputFolderLocationField.addKeyListener(new KeyAdapter()
		{
			@Override
			public void keyReleased(KeyEvent event)
			{
				File file = new File(inputFolderLocationField.getText());
				if (file.exists()) {
					generateTermsFromFolderButton.setEnabled(true);
				}
				else {
					generateTermsFromFolderButton.setEnabled(false);
				}
				if (event.getKeyCode() == 10) {
					onClickGenerateTerms(inputFolderLocationField);
				}
			}
		});

		inputDefinitionGenerationField.addKeyListener(new KeyAdapter()
		{
			@Override
			public void keyPressed(KeyEvent event)
			{
				if (event.getKeyCode() == 10) {
					onClickGenerateDefinitionsManually(inputDefinitionGenerationField);
				}
			}
		});

		generateTermsFromPubMedButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				onClickGenerateTerms(inputPubMedQueryField);
			}
		});

		generateTermsFromWebButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				onClickGenerateTerms(inputWebQueryField);
			}
		});

		generateTermsFromTextButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				onClickGenerateTerms(inputTextArea);
			}
		});

		folderChooseButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				onClickOpenFileChooser(inputFolderLocationField);
			}
		});

		generateTermsFromFolderButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				onClickGenerateTerms(inputFolderLocationField);
			}

		});

		generateManualDefinitionButton.addActionListener(new ActionListener()
		{

			public void actionPerformed(ActionEvent e)
			{
				onClickGenerateDefinitionsManually(inputDefinitionGenerationField);
			}

		});

		showClipBoardButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				onClickShowClipBoard();
			}
		});

		clearClipBoardButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				onClickClearClipBoard();
			}
		});

		loadClipBoardButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				onClickLoadClipBoard();
			}
		});

		saveClipBoardButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				onClickSaveClipBoard();
			}
		});

		candidateTermsTable.getSelectionModel().addListSelectionListener(new ListSelectionListener()
		{
			public void valueChanged(ListSelectionEvent event)
			{
				if (event.getValueIsAdjusting()) {
					return;
				}
				int selectedRow = candidateTermsTable.getSelectedRow();
				if (selectedRow >= 0) {
					selectedCandidateTerm = candidateTermsTable.getModel().getTermAt(selectedRow);
					logger.trace("Selected: " + selectedCandidateTerm);
					updateAllDependedOnSelectedTerm();
				}
			}
		});

		candidateTermsTable.addMouseListener(new MouseAdapter()
		{
			@Override
			public void mouseClicked(MouseEvent e)
			{
				if (!SwingUtilities.isLeftMouseButton(e)) {
					return;
				}
				Point p = e.getPoint();
				int column = candidateTermsTable.columnAtPoint(p);

				if (column == 2) {
					onClickGenerateDefinitions();
				}
				else if (column == 3) {
					if (lastSource != null) {
						onClickOpenExternalGoPubMedPage(lastSource);
					}
				}
			}
		});

		candidateTermsTable.addKeyListener(new KeyAdapter()
		{
			@Override
			public void keyTyped(KeyEvent e)
			{
				int row = candidateTermsTable.getSelectedRow();
				// set selectedCandidateTerm
				CandidateTerm term = candidateTermsTable.getModel().getTermAt(row);
				selectedCandidateTerm = term;
				// tick row
				if (KeyEvent.VK_SPACE == e.getKeyChar()) {
					boolean b = !(Boolean) candidateTermsTable.getValueAt(row, 0);
					candidateTermsTable.setValueAt(b, row, 0);
				}
			}
		});

		candidateTermsTable.getModel().addTableModelListener(new TableModelListener()
		{
			public void tableChanged(TableModelEvent e)
			{
				clipBoardTabPanel.setTitleAt(0, "Clipboard (" + clipboard.getSize() + ")");
			}
		});

		synonymTermsTable.addKeyListener(new KeyAdapter()
		{
			@Override
			public void keyTyped(KeyEvent e)
			{
				int row = synonymTermsTable.getSelectedRow();
				if (KeyEvent.VK_SPACE == e.getKeyChar()) {
					boolean b = !(Boolean) synonymTermsTable.getValueAt(row, 0);
					synonymTermsTable.setValueAt(b, row, 0);
				}
			}
		});

		synonymTermsTable.getModel().addTableModelListener(new TableModelListener()
		{
			public void tableChanged(TableModelEvent e)
			{
				clipBoardTabPanel.setTitleAt(0, "Clipboard (" + clipboard.getSize() + ")");
			}
		});

		onlyShowExistingTerms.addActionListener(new ActionListener()
		{

			public void actionPerformed(ActionEvent e)
			{
				updateTermsTableWithExistingTerms(onlyShowExistingTerms.isSelected());
			}
		});

		definitionTable.addMouseListener(new MouseAdapter()
		{
			@Override
			public void mouseClicked(MouseEvent e)
			{
				if (!SwingUtilities.isLeftMouseButton(e)) {
					return;
				}
				Point p = e.getPoint();
				int column = definitionTable.columnAtPoint(p);
				int row = definitionTable.rowAtPoint(p);
				// The autoscroller can generate drag events outside the Table's
				// range.
				if (column == 0) {
					updateOnClickAddDefinition(row);
				}
				else if (column == 2) {
					onClickOpenDefinitionsPopup();
				}
			}
		});

		definitionTable.addKeyListener(new KeyAdapter()
		{
			@Override
			public void keyTyped(KeyEvent e)
			{
				if (null == selectedCandidateTerm) {
					logger.error("No term selected");
				}
				else {
					int row = definitionTable.getSelectedRow();
					if (KeyEvent.VK_SPACE == e.getKeyChar()) {
						boolean b = !(Boolean) definitionTable.getValueAt(row, 0);
						definitionTable.setValueAt(b, row, 0);
					}
				}
			}
		});

		ontologyTermsTable.addKeyListener(new KeyAdapter()
		{
			@Override
			public void keyTyped(KeyEvent e)
			{
				int row = ontologyTermsTable.getSelectedRow();
				if (KeyEvent.VK_SPACE == e.getKeyChar()) {
					boolean ticked = (Boolean) ontologyTermsTable.getValueAt(row, 0);
					ontologyTermsTable.setValueAt(!ticked, row, 0);
				}
			}
		});

		scrollPaneForTermsTable.getViewport().addChangeListener(new ChangeListener()
		{
			public void stateChanged(ChangeEvent e)
			{
				updateTermsTableUsingOntologyLookup(candidateTermsTable);
			}
		});

		filterTermsTextField.getTextField().addCaretListener(new CaretListener()
		{
			public void caretUpdate(CaretEvent evt)
			{
				String text = filterTermsTextField.getTextField().getText();
				candidateTermsTable.getModel().applyFilter(text);

				candidateTermsTable.updateTermsTableStructure();
			}
		});

		filterPotentialParentsTextField.getTextField().addCaretListener(new CaretListener()
		{
			public void caretUpdate(CaretEvent evt)
			{
				String text = filterPotentialParentsTextField.getTextField().getText();
				ontologyTermsTable.getModel().applyFilter(text);
			}
		});

		searchTermsTextField.getTextField().addCaretListener(new CaretListener()
		{
			public void caretUpdate(CaretEvent evt)
			{
				String text = searchTermsTextField.getTextField().getText();
				candidateTermsTable.findTerm(text);
			}
		});

		inputDefinitionGenerationField.addCaretListener(new CaretListener()
		{
			public void caretUpdate(CaretEvent e)
			{
				if (selectedCandidateTerm != null) {
					if (inputDefinitionGenerationField.getText().equals("")) {
						generateManualDefinitionButton.setEnabled(false);
					}
					else {
						generateManualDefinitionButton.setEnabled(true);
					}
				}
			}
		});

		filterDefTextField.getTextField().addCaretListener(new CaretListener()
		{
			public void caretUpdate(CaretEvent evt)
			{
				String text = filterDefTextField.getTextField().getText();
				definitionTable.getModel().applyFilter(text);
			}
		});

		inputPubMedQueryField.addCaretListener(new CaretListener()
		{
			public void caretUpdate(CaretEvent evt)
			{
				generateTermsFromPubMedButton.setFocusable(true);
				generateTermsFromPubMedButton.setEnabled(true);
			}
		});

		inputWebQueryField.addCaretListener(new CaretListener()
		{
			public void caretUpdate(CaretEvent evt)
			{
				generateTermsFromWebButton.setFocusable(true);
				generateTermsFromWebButton.setEnabled(true);
			}
		});

		inputTextArea.addCaretListener(new CaretListener()
		{
			public void caretUpdate(CaretEvent evt)
			{
				generateTermsFromTextButton.setFocusable(true);
				generateTermsFromTextButton.setEnabled(true);
			}
		});

		inputDefinitionGenerationField.addCaretListener(new CaretListener()
		{
			public void caretUpdate(CaretEvent evt)
			{
				generateManualDefinitionButton.setFocusable(true);
				generateManualDefinitionButton.setEnabled(true);
			}
		});

		editDefArea.addCaretListener(new CaretListener()
		{
			public void caretUpdate(CaretEvent e)
			{
				String currentDefinition = editDefArea.getText().trim();
				if (currentDefinition.equals(selectedCandidateTerm.getUserDefinedDefinition())
						|| currentDefinition.equals(adapter.getDefinitionForExistingTerm(selectedCandidateTerm))) {
					updateSaveDefWarningLabel(false);
				}
				else {
					updateSaveDefWarningLabel(true);
				}
			}
		});

		saveDefButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				onClickSaveDefinition();
			}
		});

		editNameTextField.addCaretListener(new CaretListener()
		{

			public void caretUpdate(CaretEvent e)
			{
				if (selectedCandidateTerm != null) {
					String currentLabel = editNameTextField.getText().trim();
					if (currentLabel.equals(selectedCandidateTerm.getUserDefinedLabel()) || currentLabel.equals(selectedCandidateTerm.getLabel())) {
						updateSaveLabelWarningLabel(false);
					}
					else {
						updateSaveLabelWarningLabel(true);
					}
				}
			}
		});

		saveLabelButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				onClickSaveLabel();
			}
		});

		ontologyTermsTable.getSelectionModel().addListSelectionListener(new ListSelectionListener()
		{
			public void valueChanged(ListSelectionEvent e)
			{
				if (e.getValueIsAdjusting()) {
					return;
				}
				int selectedRow = ontologyTermsTable.getSelectedRow();
				if (selectedRow != selectedParentTermRow && selectedRow >= 0) {
					selectedParentTermRow = selectedRow;
					Set<String> tickedTerms = new HashSet<String>();
					tickedTerms.addAll(ontologyTermsTable.getModel().getTickedTerms());

					T term = ontologyTermsTable.getModel().getTermAt(selectedRow);
					String selectedObjectID = getOntologyTermsTable().getModel().getTermId(term);

					if (selectedRow >= 0) {
						adapter.selectOntologyTerm(selectedObjectID);
					}
					ontologyTermsTable.setRowSelectionInterval(ontologyTermsTable.getModel().getRowFromTerm(selectedObjectID), ontologyTermsTable.getModel()
							.getRowFromTerm(selectedObjectID));
					ontologyTermsTable.getModel().setTickedTerms(tickedTerms);
				}
			}
		});

		addToOntologyButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				onClickAddToOntology();
			}
		});

		checkboxShowTickedParents.addItemListener(new ItemListener()
		{

			public void itemStateChanged(ItemEvent e)
			{
				if (e.getStateChange() == ItemEvent.SELECTED) {
					ontologyTermsTable.getModel().setShowOnlyTicked(true);
				}
				else {
					ontologyTermsTable.getModel().setShowOnlyTicked(false);
				}
			}
		});

	}

	/**
	 * Open a fileChooser to get a location for pdf files
	 */
	private void onClickOpenFileChooser(JTextComponent textComponent)
	{
		if (textComponent == null) {
			return;
		}

		JFileChooser fileChooser = new JFileChooser();

		String sfolder = textComponent.getText();
		logger.trace("sFolder: " + sfolder);
		if (!sfolder.trim().equals("")) {
			File file = new File(sfolder);
			if (file.isDirectory()) {
				fileChooser.setCurrentDirectory(file.getAbsoluteFile());
			}
			else if (file.isFile()) {
				fileChooser.setCurrentDirectory(file.getParentFile());
			}
		}

		class pdfFileFilter extends FileFilter
		{

			@Override
			public boolean accept(File file)
			{
				String filename = file.getName();
				if (file.isDirectory()) {
					return true;
				}
				return filename.endsWith(".pdf");
			}

			@Override
			public String getDescription()
			{
				return "*.pdf";
			}
		}

		fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
		fileChooser.addChoosableFileFilter(new pdfFileFilter());

		if (fileChooser.showOpenDialog(new JFrame()) == JFileChooser.APPROVE_OPTION) {
			sfolder = fileChooser.getSelectedFile().getAbsolutePath();
			textComponent.setText(sfolder);

			generateTermsFromFolderButton.setEnabled(true);
			generateTermsFromFolderButton.setFocusable(true);
		}
		else {
			generateTermsFromFolderButton.setEnabled(false);
			generateTermsFromFolderButton.setFocusable(false);
		}
	}

	/**
	 * Shows the clip board in termsTable
	 */
	private void onClickShowClipBoard()
	{
		// clear first
		clearAllDependendOnSelectedTerm();
		filterTermsTextField.getTextField().setText(null);
		searchTermsTextField.getTextField().setText(null);
		updateClipboard(candidateTermsTable);
		// updateUI();
		updateTermsTableUsingOntologyLookup(candidateTermsTable);
	}

	/**
	 * Populate {@link TermsTable} with terms from clip board
	 * 
	 * @param table
	 */
	private void updateClipboard(TermsTable table)
	{
		ArrayList<CandidateTerm> list = new ArrayList<CandidateTerm>(clipboard.getAllCandidateTerms());
		Collections.sort(list, new Comparator<CandidateTerm>()
		{
			public int compare(CandidateTerm o1, CandidateTerm o2)
			{
				int compare = Double.compare(o1.getScore(), o2.getScore());
				if (compare != 0) {
					return compare;
				}
				return o1.getGeneratedLabel().compareTo(o2.getGeneratedLabel());
			}
		});
		table.setTerms(list);
		table.getColumnModel().getColumn(1).setHeaderValue("Showing Clipboard");
		// Scroll and select
		JTableHelper.scrollToTopAndSelectFirst(table);
	}

	/**
	 * Clears the clip board
	 */
	private void onClickClearClipBoard()
	{
		candidateTermsTable.getModel().unTickAll();
		clipboard.clear();

		// clear
		clearAllDependendOnSelectedTerm();
		clipBoardTabPanel.setTitleAt(0, "Clipboard " + "(" + clipboard.getSize() + ")");
		guiComponent.updateUI();
	}

	/**
	 * Save clip board to file
	 */
	private void onClickSaveClipBoard()
	{

		JFileChooser chooser = new JFileChooser();
		int option = chooser.showSaveDialog(guiComponent);
		if (option == JFileChooser.APPROVE_OPTION) {
			// statusbar.setText("You saved " +
			// ((chooser.getSelectedFile()!=null)?
			// chooser.getSelectedFile().getName():"nothing"));
		}
		else {
			// statusbar.setText("You canceled.");
		}
		File fileToSave = chooser.getSelectedFile();
		if (null == fileToSave) {
			return;
		}

		// generate content to save
		Collection<CandidateTerm> allTermsFromClipboard = clipboard.getAllCandidateTerms();
		StringBuffer outBuffer = new StringBuffer();
		for (CandidateTerm candidateTerm : allTermsFromClipboard) {
			if (candidateTerm.isTicked()) {
				outBuffer.append(candidateTerm.getGeneratedLabel().replace("\t", " "));
				outBuffer.append("\t");
				if (null != candidateTerm.getAbbreviations()) {
					Iterator<String> iterator = candidateTerm.getAbbreviations().iterator();
					outBuffer.append("[");
					while (iterator.hasNext()) {
						outBuffer.append(iterator.next().replace("\t", " ").replace("|", ";"));
						if (iterator.hasNext()) {
							outBuffer.append("|");
						}
					}

				}
				outBuffer.append("]");
				outBuffer.append("\t");
				if (null != candidateTerm.getLexicalRepresentations()) {
					Iterator<String> iterator = candidateTerm.getLexicalRepresentations().iterator();
					outBuffer.append("[");
					while (iterator.hasNext()) {
						outBuffer.append(iterator.next().replace("\t", " ").replace("|", ";"));
						if (iterator.hasNext()) {
							outBuffer.append("|");
						}
					}
				}
				outBuffer.append("]");
				outBuffer.append("\t");
				if (null != candidateTerm.getUserDefinedLabel()) {
					outBuffer.append(candidateTerm.getUserDefinedLabel().replace("\t", " "));
				}
				outBuffer.append("\t");
				if (null != candidateTerm.getUserDefinedDefinition()) {
					outBuffer.append(candidateTerm.getUserDefinedDefinition().replace("\t", " "));
				}
				outBuffer.append("\n");
			}
		}

		BufferedWriter bufferedWriter = null;
		try {
			bufferedWriter = new BufferedWriter(new FileWriter(fileToSave));
			bufferedWriter.write(outBuffer.toString());
		}
		catch (IOException exception) {
			logger.error("Saving terms in clipboard failed!", exception);
		}
		finally {
			if (bufferedWriter != null) {
				try {
					bufferedWriter.close();
				}
				catch (IOException exception) {
					logger.error("Saving terms in clipboard failed!", exception);
				}
			}
		}
	}

	/**
	 * Load clip board from file
	 */
	private void onClickLoadClipBoard()
	{
		JFileChooser chooser = new JFileChooser();
		chooser.setMultiSelectionEnabled(true);
		int option = chooser.showOpenDialog(guiComponent);
		if (option == JFileChooser.APPROVE_OPTION) {
			File[] sf = chooser.getSelectedFiles();
			String filelist = "nothing";
			if (sf.length > 0)
				filelist = sf[0].getName();
			for (int i = 1; i < sf.length; i++) {
				filelist += ", " + sf[i].getName();
			}
		}
		File fileToLoad = chooser.getSelectedFile();
		if (null == fileToLoad) {
			return;
		}

		// read file
		List<String> lines = new ArrayList<String>();
		BufferedReader bufferedReader = null;
		try {
			bufferedReader = new BufferedReader(new FileReader(fileToLoad));
			while (bufferedReader.ready()) {
				String readLine = bufferedReader.readLine();
				lines.add(readLine);
			}
		}
		catch (FileNotFoundException exception) {
			logger.error("Loading terms in clipboard failed!", exception);
		}
		catch (IOException exception) {
			logger.error("Loading terms in clipboard failed!", exception);
		}
		finally {
			if (bufferedReader != null) {
				try {
					bufferedReader.close();
				}
				catch (IOException exception) {
					logger.error("Loading terms in clipboard failed!", exception);
				}
			}
		}
		for (String line : lines) {
			String[] split = line.split("\t");
			if (split.length > 0) {
				CandidateTerm candidateTerm = new CandidateTerm();
				candidateTerm.setTicked(true);
				candidateTerm.setVisible(true);
				candidateTerm.setGeneratedLabel(split[0]);
				candidateTerm.addType(CandidateTerm.TYPE_LOADED);
				if (split.length > 1) {
					String abbreviationString = split[1];
					abbreviationString = abbreviationString.subSequence(1, abbreviationString.length() - 1).toString();
					String[] abbreviations = abbreviationString.split("\\|");
					for (String abbrev : abbreviations) {
						candidateTerm.addAbbreviation(abbrev);
					}
				}
				if (split.length > 2) {
					String lexicalString = split[2];
					lexicalString = lexicalString.subSequence(1, lexicalString.length() - 1).toString();
					String[] lexicalRepresentations = lexicalString.split("\\|");
					for (String lex : lexicalRepresentations) {
						candidateTerm.addLexicalRepresentation(lex);
					}
				}
				if (split.length > 3) {
					if (split[3].length() > 0) {
						candidateTerm.setUserDefinedLabel(split[3]);
					}
				}
				if (split.length > 4) {
					if (split[4].length() > 0) {
						candidateTerm.setUserDefinedDefinition(split[4]);
					}
				}
				candidateTermsTable.getModel().addTermToClipboard(candidateTerm);
			}
		}
		onClickShowClipBoard();
	}

	/**
	 * Invokes TermGenerationWorker Thread on "Generate Terms" Button
	 * 
	 * @param inputPubMedQueryField2
	 */
	private void onClickGenerateTerms(JTextComponent textComponent)
	{
		showProgressDlg(false);
		String inputData = textComponent.getText().trim();
		// call to term generation thread for fetching terms from web service

		ontologyLookupQueue.clear();
		ontologyLookupChildrenQueue.clear();

		String source = null;
		if (textComponent.equals(inputPubMedQueryField)) {
			source = SOURCE_PUBMED;
		}
		else if (textComponent.equals(inputWebQueryField)) {
			source = SOURCE_WEB;
		}
		else if (textComponent.equals(inputTextArea)) {
			source = SOURCE_TEXT;
		}
		else if (textComponent.equals(inputFolderLocationField)) {
			source = SOURCE_FOLDER;
		}
		lastSource = source;

		filterTermsTextField.getTextField().setText(null);
		searchTermsTextField.getTextField().setText(null);

		guiComponent.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

		TermGenerationServiceWorker worker = new TermGenerationServiceWorker(inputData, candidateTermsTable, source);
		worker.addPropertyChangeListener(this);
		worker.execute();
		showProgressDlg(true);
	}

	private void updateTermsTableHeader(String source, String inputData)
	{
		if (source.equals(SOURCE_PUBMED)) {
			String message = String.format("Terms generated for PubMed query '%s'", inputData);
			candidateTermsTable.getColumnModel().getColumn(1).setHeaderValue(message);
			logger.debug(message);
		}
		else if (source.equals(SOURCE_WEB)) {
			String message = String.format("Terms generate terms for WEB query '%s'", inputData);
			candidateTermsTable.getColumnModel().getColumn(1).setHeaderValue(message);
			logger.debug(message);
		}
		else if (source.equals(SOURCE_TEXT)) {
			String message = String.format("Terms generated for TEXT (%s kB)", inputData.length() / 1024);
			candidateTermsTable.getColumnModel().getColumn(1).setHeaderValue(message);
			logger.debug(message);
		}
		else if (source.equals(SOURCE_FOLDER)) {
			String message = String.format("Terms generated from PDF documents: '%s'", inputData);
			candidateTermsTable.getColumnModel().getColumn(1).setHeaderValue(message);
			logger.debug(message);
		}
		candidateTermsTable.getTableHeader().repaint();

		filterTermsTextField.getTextField().setText(null);
		searchTermsTextField.getTextField().setText(null);

	}

	/**
	 * Launches web browser to get results from www.gopubmed.org for queryTerm
	 * and termsTable selected term.
	 */
	@SuppressWarnings("unchecked")
	private void onClickOpenExternalGoPubMedPage(String lastSource)
	{
		String goURL;
		JTextField textField;
		if (lastSource == SOURCE_PUBMED) {
			textField = inputPubMedQueryField;
			goURL = "http://www.gopubmed.org/search?q=";
		}
		else if (lastSource == SOURCE_WEB) {
			textField = inputWebQueryField;
			goURL = "http://www.gopubmed.org/web/goweb/search?q=";
		}
		else {
			return;
		}

		String encodedGeneratedTerm;
		String encodedqServiceTerm;

		int colNumber = OntologyGenerationComponent.this.candidateTermsTable.getSelectedColumn();
		int rowNumber = OntologyGenerationComponent.this.candidateTermsTable.getSelectedRow();

		String errMsg = "Error attempting to launch web browser";
		if (colNumber == 3 && rowNumber >= 0) {
			CandidateTerm candidateTerm = candidateTermsTable.getModel().getTermAt(rowNumber);
			String generatedTerm = putInQuotes(candidateTerm.getGeneratedLabel());
			String qServiceTerm = putInQuotes(textField.getText());

			try {
				encodedGeneratedTerm = URLEncoder.encode(generatedTerm, "UTF-8");
				encodedqServiceTerm = URLEncoder.encode(qServiceTerm, "UTF-8");
			}
			catch (UnsupportedEncodingException exception) {
				throw new RuntimeException(exception);
			}

			StringBuffer qBuffer = new StringBuffer();
			qBuffer.append(goURL);
			qBuffer.append(encodedGeneratedTerm);
			qBuffer.append("+AND+");
			qBuffer.append(encodedqServiceTerm);
			qBuffer.append("&t=oboedit");

			String url = qBuffer.toString();
			String osName = System.getProperty("os.name");

			try {

				if (osName.startsWith("Mac OS")) {

					Class fileMgr = Class.forName("com.apple.eio.FileManager");
					Method openURL = fileMgr.getDeclaredMethod("openURL", new Class[] { String.class });
					openURL.invoke(null, new Object[] { url });

				}
				else if (osName.startsWith("Windows"))
					Runtime.getRuntime().exec("rundll32 url.dll,FileProtocolHandler " + url);

				else { // assume Unix or Linux
					String[] browsers = { "firefox", "opera", "konqueror", "epiphany", "mozilla", "netscape" };
					String browser = null;
					for (int count = 0; count < browsers.length && browser == null; count++) {
						if (Runtime.getRuntime().exec(new String[] { "which", browsers[count] }).waitFor() == 0) {
							browser = browsers[count];
						}
					}
					if (browser == null) {
						logger.error("Could not find web browser");
					}
					else {
						Runtime.getRuntime().exec(new String[] { browser, url });
					}
				}
			}
			catch (Exception e) {
				JOptionPane.showMessageDialog(guiComponent, errMsg + ":\n" + e.getLocalizedMessage());
			}

		}
	}

	/**
	 * Instantiates DefinitionWorker thread responsible to call
	 * definitionGeneration Service
	 */
	private void onClickGenerateDefinitions()
	{
		int rowNumber = OntologyGenerationComponent.this.candidateTermsTable.getSelectedRow();
		int colNumber = OntologyGenerationComponent.this.candidateTermsTable.getSelectedColumn();

		// For each definition query first clear the termsInDefComboBox
		if (colNumber == 2 && rowNumber >= 0) {
			// find DEFINITIONS
			// call to definition web service to fetch definitions
			String label = selectedCandidateTerm.getLabel();
			logger.debug(String.format("Generate definitions for term '%s'", label));
			startGenerateDefinitions(label);
		}
	}

	private void startGenerateDefinitions(String label)
	{
		showProgressDlg(false);
		guiComponent.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

		DefinitionGenerationServiceWorker defworker = new DefinitionGenerationServiceWorker(label, null, definitionTable);
		defworker.addPropertyChangeListener(this);
		defworker.execute();
		showProgressDlg(true);
	}

	/**
	 * generated a new CandidateDefinition object
	 * 
	 * @param definitionField
	 */
	/*
	 * SPEC: generate definition for the term, if the term is visible, move term
	 * to the top, if not make it visible and move it to the top and select
	 * (selection should make the definition, if exist, listed first)
	 */
	private void onClickGenerateDefinitionsManually(JTextField definitionField)
	{
		String inputLabel = definitionField.getText().trim();
		CandidateTerm termToGenerateDefinitionsFor;
		if (candidateTermCache.hasCandidateTermWithLabel(inputLabel)) {
			termToGenerateDefinitionsFor = candidateTermCache.get(inputLabel);
		}
		else if (clipboard.hasCandidateTermWithLabel(inputLabel)) {
			termToGenerateDefinitionsFor = clipboard.get(inputLabel);
		}
		else {
			CandidateTerm candidateTermWithDefinition = new CandidateTerm(inputLabel, null, null, 1, CandidateTerm.TYPE_MANUAL);
			candidateTermWithDefinition.setUserDefinedLabel(inputLabel);
			candidateTermWithDefinition.addLexicalRepresentation(inputLabel);
			termToGenerateDefinitionsFor = candidateTermWithDefinition;
			candidateTermCache.addTerm(termToGenerateDefinitionsFor);
		}
		List<CandidateTerm> allTerms = candidateTermsTable.getModel().getAllTerms();
		for (CandidateTerm candidateTerm : allTerms) {
			if (candidateTerm.getLabel().equals(termToGenerateDefinitionsFor.getLabel())) {
				termToGenerateDefinitionsFor = candidateTerm;
			}
			else if (candidateTerm.getLexicalRepresentations().contains(termToGenerateDefinitionsFor.getLabel())) {
				termToGenerateDefinitionsFor = candidateTerm;
			}
		}
		// if not selected, put term for which definitions are generated to top
		// of the termsTable
		if (!termToGenerateDefinitionsFor.equals(selectedCandidateTerm)) {
			List<CandidateTerm> newTermsList = new ArrayList<CandidateTerm>(allTerms.size() + 1);
			newTermsList.add(termToGenerateDefinitionsFor);
			for (CandidateTerm candidateTerm : allTerms) {
				if (!candidateTerm.equals(termToGenerateDefinitionsFor)) {
					newTermsList.add(candidateTerm);
				}
			}
			candidateTermsTable.setTerms(newTermsList);

			// scroll top and select first
			if (onlyShowExistingTerms.isSelected() && !termToGenerateDefinitionsFor.isInLoadedOntology()) {
				onlyShowExistingTerms.setSelected(false);
				updateTermsTableWithExistingTerms(false);
			}
			else {
				JTableHelper.scrollToTopAndSelectFirst(candidateTermsTable);
			}
		}
		// trigger update ontology lookup
		updateTermsTableUsingOntologyLookup(candidateTermsTable);
		startGenerateDefinitions(inputLabel);
	}

	/**
	 * Picks the clicked definition, sets it ticked and updates depended GUI
	 * components
	 * 
	 * @param rowIndex
	 */
	private void updateOnClickAddDefinition(int rowIndex)
	{
		logger.info("onClickSelectDefinition " + rowIndex);
		if (null == selectedCandidateTerm) {
			logger.error("No term selected");
		}
		else {
			CandidateDefinition definition = definitionTable.getModel().getDefinitionAt(rowIndex);
			definition.setTicked();

			StringBuffer defString = new StringBuffer();
			String defAreaText = editDefArea.getText();
			defString.append(defAreaText.trim());

			if (!defAreaText.contains("----See Newly Generated Definition Below-----")) {
				defString.append("\n");
				defString.append("----See Newly Generated Definition Below-----");
				defString.append("\n- ");
				defString.append(definition.getDefinition().trim());

			}
			else {
				defString.append("\n- ");
				defString.append(definition.getDefinition().trim());
			}

			editDefArea.setText(defString.toString());
		}
	}

	/**
	 * Opens a popup showing the URLs for the definition and its similar
	 * definitions.
	 */
	private void onClickOpenDefinitionsPopup()
	{
		int rowNumber = OntologyGenerationComponent.this.definitionTable.getSelectedRow();

		DefinitionsPopup definitionsPopup = new DefinitionsPopup(guiComponent);
		definitionsPopup.initPopup(definitionTable.getModel().getDefinitionAt(rowNumber));
		definitionsPopup.setVisible(true);
	}

	private void updateSaveDefWarningLabel(boolean enableWarning)
	{
		if (enableWarning) {
			saveDefWarningLabel.setText("Definition not saved!");
		}
		else {
			saveDefWarningLabel.setText("");
		}
	}

	/**
	 * Save definition for {@link CandidateTerm} and update add-to-ontology
	 * table
	 */
	private void onClickSaveDefinition()
	{
		if (null == selectedCandidateTerm) {
			logger.error("No term selected");
		}
		else {
			String defText = editDefArea.getText();
			selectedCandidateTerm.setUserDefinedDefinition(defText);
			candidateTermCache.addTerm(selectedCandidateTerm);
			updateParentAsTermFromDefinition(selectedCandidateTerm, candidateTermsTable, ontologyTermsTable, definitionTable);
			// commit to OBOClass if term is known
			adapter.commitDefinition(selectedCandidateTerm);
			updateSaveDefWarningLabel(false);
		}
	}

	private void updateSaveLabelWarningLabel(boolean enableWarning)
	{
		if (enableWarning) {
			saveLabelWarningLabel.setText("Label not saved!");
		}
		else {
			saveLabelWarningLabel.setText("");
		}
	}

	/**
	 * Save definition for {@link CandidateTerm} and update add-to-ontology
	 * table
	 */
	private void onClickSaveLabel()
	{
		if (null == selectedCandidateTerm) {
			logger.error("No term selected");
		}
		else {
			String label = editNameTextField.getText();
			selectedCandidateTerm.setUserDefinedLabel(label);
			candidateTermCache.addTerm(selectedCandidateTerm);
			updateParentAsTermFromDefinition(selectedCandidateTerm, candidateTermsTable, ontologyTermsTable, definitionTable);
			// commit to OBOClass if term is known
			adapter.commitLabel(selectedCandidateTerm);
			updateSaveLabelWarningLabel(false);

		}
	}

	/**
	 * Add selected terms to application ontology model
	 */
	private void onClickAddToOntology()
	{
		boolean includeChildren = checkboxIncludeChildren.isSelected();
		boolean includeBranch = false; // checkboxIncludeBranch.isSelected();

		ArrayList<ParentRelationEntry<R>> parentRelationList = new ArrayList<ParentRelationEntry<R>>(10);
		for (String parentId : ontologyTermsTable.getModel().getTickedTerms()) {
			R relationType = ontologyTermsTable.getModel().getRelationType(parentId);
			ParentRelationEntry<R> parentRelationEntry = new ParentRelationEntry<R>(parentId, relationType);
			parentRelationList.add(parentRelationEntry);
		}
		adapter.commitAddToOntologyAsChildOfLinkedObject(selectedCandidateTerm, parentRelationList, includeChildren, includeBranch);
	}

	/**
	 * Displays term selected in the termsTable and updates all depending gui
	 * components
	 */
	public synchronized void updateAllDependedOnSelectedTerm()
	{
		logger.trace("UPDATE updatedAllDependedOnSelectedTerm() for :" + selectedCandidateTerm);

		if (null == selectedCandidateTerm) {
			logger.warn("Invalid selection in termsTable, term is assumed to be selected");
		}
		else {
			clearAllDependendOnSelectedTerm();
			List<CandidateDefinition> generatedDefinitions = selectedCandidateTerm.getGeneratedDefinitions();

			inputDefinitionGenerationField.setText(selectedCandidateTerm.getLabel());
			if (null != generatedDefinitions) {
				definitionTable.removeAll();
				definitionTable.setDefinitions(generatedDefinitions);

			}
			updateEditDefArea();

			if (selectedCandidateTerm.getGeneratedDefinitions() != null && !selectedCandidateTerm.getGeneratedDefinitions().isEmpty()) {
				definitionTable.getColumnModel().getColumn(1).setHeaderValue("Definitions for \"" + selectedCandidateTerm.getLabel() + "\"");
				definitionTable.getTableHeader().repaint();
			}

			if (!editNameTextField.getText().equals(selectedCandidateTerm.getLabel())) {
				editNameTextField.setText(selectedCandidateTerm.getLabel());
				editNameTextField.updateUI();
			}

			updateParentAsTermFromDefinition(selectedCandidateTerm, candidateTermsTable, ontologyTermsTable, definitionTable);
			updateSynonymOrChildTable();
			updateParentAsSimiliarTerm();
			updateParentAsExistingTerm();

			logger.info("SelectedCandidateTerm Name: " + selectedCandidateTerm.getLabel());
		}
	}

	/**
	 * Add similar (based on substring inclusion) terms to similiarTermsComboBox
	 * based on substring comparison
	 */
	public void updateParentAsSimiliarTerm()
	{
		logger.trace("UPDATE SIMILAR TERMS for :" + selectedCandidateTerm.getLabel());

		// clearing
		ontologyTermsTable.getModel().clearSameAsCandidateTerms();
		ontologyTermsTable.getModel().clearSimiliarToCandidateTerm();

		Collection<String> labels = new HashSet<String>();
		labels.addAll(selectedCandidateTerm.getLexicalRepresentations());
		labels.addAll(selectedCandidateTerm.getAbbreviations());
		if (null != selectedCandidateTerm.getLabel())
			labels.add(selectedCandidateTerm.getLabel());
		if (null != selectedCandidateTerm.getUserDefinedLabel())
			labels.add(selectedCandidateTerm.getUserDefinedLabel());
		if (null != selectedCandidateTerm.getGeneratedLabel())
			labels.add(selectedCandidateTerm.getGeneratedLabel());

		List<String> idsOfSimilarTerms = adapter.lookupOntologyTermIdsFromIndexFuzzy(labels);
		ontologyTermsTable.getModel().updateSimilarTerms(labels, idsOfSimilarTerms);
	}

	/**
	 * Updated the parent terms in the existing loaded ontology model
	 */
	private void updateParentAsExistingTerm()
	{
		if (selectedCandidateTerm != null) {
			Map<String, String> parents = adapter.getParentsForExistingTerm(selectedCandidateTerm);
			ontologyTermsTable.getModel().addParentsTermsOfExistingCandidateTerm(parents);
		}
	}

	/**
	 * Update the table holding synonyms and children of the currently selected
	 * terms
	 */
	private void updateSynonymOrChildTable()
	{
		// update synonymous terms
		List<CandidateTerm> synonymousOrChildTerms = new ArrayList<CandidateTerm>();
		Iterable<String> userDefinedAbbreviations = selectedCandidateTerm.getAbbreviations();
		for (String string : userDefinedAbbreviations) {
			CandidateTerm abbreviationTerm = new CandidateTerm();
			abbreviationTerm.setGeneratedLabel(string);
			abbreviationTerm.addLexicalRepresentation(string);
			abbreviationTerm.addType(CandidateTerm.TYPE_ABBREVIATION);
			synonymousOrChildTerms.add(abbreviationTerm);
			abbreviationTerm.setVisible(true);
		}
		// update children
		if (selectedCandidateTerm.getExistingChildTerms() == null) {
			// no terms looked up yet, add to queue
			if (!ontologyLookupChildrenQueue.contains(selectedCandidateTerm)) {
				logger.trace("ADD to queue: " + selectedCandidateTerm);
				if (ontologyLookupChildrenQueue.remainingCapacity() == 0) {
					CandidateTerm remove = ontologyLookupChildrenQueue.remove();
					logger.trace("remove " + remove.getLabel());
				}
				ontologyLookupChildrenQueue.offer(selectedCandidateTerm);
			}
			List<CandidateTerm> clipboardTerms = candidateTermsTable.getModel().getAllTerms();
			for (CandidateTerm candidateTerm : clipboardTerms) {
				if (candidateTermsTable.getModel().isInClipboard(candidateTerm) && !ontologyLookupChildrenQueue.contains(candidateTerm)) {
					logger.trace("ADD to queue clipboard term: " + candidateTerm);
					if (ontologyLookupChildrenQueue.remainingCapacity() == 0) {
						CandidateTerm remove = ontologyLookupChildrenQueue.remove();
						logger.trace("remove " + remove.getLabel());
					}
					ontologyLookupChildrenQueue.offer(candidateTerm);
				}
			}
		}
		else {
			// render terms
			logger.trace("RENDER: " + selectedCandidateTerm);
			List<OBOLookupTerm> existingChildTerms = selectedCandidateTerm.getExistingChildTerms();
			List<OBOLookupRelation> existingChildRelations = selectedCandidateTerm.getExistingChildRelations();
			for (OBOLookupRelation lookupRelation : existingChildRelations) {
				String oboChildTermID = lookupRelation.getOboChildTermID();
				String oboRelationShipType = lookupRelation.getOboRelationShipType();
				for (OBOLookupTerm childTerm : existingChildTerms) {
					if (childTerm.getOboID().equals(oboChildTermID)) {
						if (childTerm.getLabel() != null && !selectedCandidateTerm.getLabel().equals(childTerm.getLabel())) {
							CandidateTerm oboCandidateTerm = new CandidateTerm();
							oboCandidateTerm.setGeneratedLabel(childTerm.getLabel());
							oboCandidateTerm.addLexicalRepresentation(childTerm.getLabel());
							oboCandidateTerm.addType(oboRelationShipType);
							oboCandidateTerm.setVisible(true);
							synonymousOrChildTerms.add(oboCandidateTerm);
						}
					}
				}
			}
		}

		// render the table for synonym terms and children
		this.synonymTermsTable.setTerms(synonymousOrChildTerms);
		for (CandidateTerm generatedTerm : synonymousOrChildTerms) {
			if (synonymTermsTable.getModel().isInClipboard(generatedTerm)) {
				// add term merged from eventually existing term and newly
				// generated in clipboard, if already in
				// there.
				synonymTermsTable.getModel().addTermToClipboard(generatedTerm);
			}
		}
	}

	private void updateTermsTableUsingOntologyLookup(TermsTable pTermsTable)
	{
		Rectangle visibleRect = pTermsTable.getVisibleRect();
		int firstVisibleRow = pTermsTable.rowAtPoint(new Point(0, visibleRect.y));
		int lastVisibleRow = pTermsTable.rowAtPoint(new Point(0, visibleRect.y + visibleRect.height - 1));
		logger.trace(String.format("UPDATE updateTermsTableUsingOntologyLookup; FEED %s %s", firstVisibleRow, lastVisibleRow));
		if (firstVisibleRow >= 0 && lastVisibleRow >= 0
				&& (candidateTermsTable.getCurrentFirstVisibleRow() != firstVisibleRow || candidateTermsTable.getCurrentLastVisibleRow() != lastVisibleRow)) {
			ontologyLookupQueue.clear();
			for (int i = firstVisibleRow; i <= lastVisibleRow; i++) {
				CandidateTerm candidateTerm = pTermsTable.getModel().getTermAt(i);
				ontologyLookupQueue.offer(candidateTerm);
			}
		}
		candidateTermsTable.setCurrentFirstVisibleRow(firstVisibleRow);
		candidateTermsTable.setCurrentLastVisibleRow(lastVisibleRow);
	}

	private void updateTermsTableWithExistingTerms(boolean showExistingTerms)
	{
		candidateTermsTable.setOnlyShowExistingTerms(showExistingTerms);

		// Scroll and select
		JTableHelper.scrollToTopAndSelectFirst(candidateTermsTable);

		// remove existing lookup queries
		ontologyLookupQueue.clear();
		ontologyLookupChildrenQueue.clear();

		// reinitialize visible table rows
		candidateTermsTable.setCurrentFirstVisibleRow(-1);
		candidateTermsTable.setCurrentLastVisibleRow(-1);

		// restart ontology terms lookup
		updateTermsTableUsingOntologyLookup(candidateTermsTable);
	}

	/**
	 * Recursively get all the descendants
	 * 
	 * @param children
	 */
	private void lookedUpChildTerm(List<String> children)
	{
		try {
			HashMap<String, String> idToName = new HashMap<String, String>();

			for (String childId : children) {
				OBOLookupResult lookupResult = ontoLookupProxy.getChildren(childId, 1);
				if (lookupResult.getTerms() != null) {
					for (OBOLookupTerm lookupTerm : lookupResult.getTerms()) {
						idToName.put(lookupTerm.getOboID(), lookupTerm.getLabel());
					}
				}
				List<String> children2 = new ArrayList<String>();
				if (lookupResult.getRelations() != null) {
					for (OBOLookupRelation relation : lookupResult.getRelations()) {
						String parent = idToName.get(relation.getOboTermId());
						String child = idToName.get(relation.getOboChildTermID());
						children2.add(relation.getOboChildTermID());
						String relationString = relation.getOboRelationShipType();
						logger.error(String.format("%s -%s-> %s", parent, relationString, child));
					}
				}
				lookedUpChildTerm(children2);
			}

		}
		catch (RemoteException exception) {
			logger.error("Lookup for known children via ontology lookup service failed.", exception);
		}

	}

	private void clearAllDependendOnSelectedTerm()
	{
		clearFieldsInLowerGUI();
	}

	/**
	 * Update the lower part of GUI
	 */
	private void clearFieldsInLowerGUI()
	{
		definitionTable.removeAll();
		definitionTable.getColumnModel().getColumn(1).setHeaderValue("Definitions");
		definitionTable.updateUI();
		filterDefTextField.getTextField().setText(null);
		editDefArea.setText(null);
		saveDefWarningLabel.setText("");
		saveLabelWarningLabel.setText("");
		synonymTermsTable.getModel().removeAll();
		editNameTextField.setText(null);
		filterPotentialParentsTextField.getTextField().setText(null);
	}

	/*
	 * HELPERS
	 */
	private String putInQuotes(String term)
	{
		StringBuffer qBuffer = new StringBuffer();
		qBuffer.append("\"");
		qBuffer.append(term);
		qBuffer.append("\"");
		return qBuffer.toString();
	}

	/**
	 * Replace invalid character (prepare text to send with Axis)
	 * 
	 * @param string
	 * @param builder
	 */
	private static final String prepareTextReplaceInvalidCharacter(String string)
	{
		StringBuilder builder = new StringBuilder();
		for (int i = 0; i < string.length(); i++) {
			char c = string.charAt(i);
			if ((c < 0x001F || (c >= 0x007F && c <= 0x00A0)) && c != 0x000A) {
				builder.append(' ');
			}
			else {
				builder.append(c);
			}
		}
		return builder.toString();
	}

	public void updateInputFieldsForSelectedLinkedObjectLabel(String label)
	{
		inputPubMedQueryField.setText(label);
		inputWebQueryField.setText(label);
		inputDefinitionGenerationField.setText(label);
	}

	public void updateParentAsTermFromDefinition(CandidateTerm selectedCandidateTerm, TermsTable termsTable,
			AbstractOntologyTermsTable<T, R> ontologyTermsTable, DefinitionsTable definitionsTable)
	{
		logger.trace("UPDATE TERMS FROM DEFINITION for :" + selectedCandidateTerm);

		if (null == selectedCandidateTerm) {
			int row = termsTable.rowAtPoint(termsTable.getMousePosition());
			selectedCandidateTerm = termsTable.getModel().getTermAt(row);
			logger.warn("Selection lost in terms table, recovered though mouse position");
		}
		// clear
		ontologyTermsTable.getModel().clearTermsFromDefinitions();

		// process user defined definition
		if (null != selectedCandidateTerm.getUserDefinedDefinition()) {
			List<String> ids = searchForOntologyTermsInStrings(Collections.singletonList(selectedCandidateTerm.getUserDefinedDefinition()));
			int rank = 1;
			for (String id : ids) {
				ontologyTermsTable.getModel().addFromUserDefinedDefinition(id, rank);
				rank++;
			}
		}

		// process generated definitions
		List<CandidateDefinition> definitions = definitionsTable.getModel().getDefinitions();
		Set<String> definitionStringsToCheck = new HashSet<String>(definitions.size());
		for (CandidateDefinition definition : definitions) {
			String definitionalContext = definition.getDefinitionalContext();
			if (definitionalContext != null && definitionalContext.length() < definition.getDefinition().length()) {
				definitionStringsToCheck.add(definitionalContext);
			}
		}
		if (definitionStringsToCheck.size() > 0) {
			List<String> ids = searchForOntologyTermsInStrings(definitionStringsToCheck);
			int rank = 1;
			for (String id : ids) {
				ontologyTermsTable.getModel().addFromCandidateDefinition(id, rank);
				rank++;

			}
		}
	}

	/**
	 * Create all ngrams contained in the texts in the specified list and
	 * looking up the term these ngrams match.
	 * 
	 * @param ngrams
	 * @return
	 */
	private List<String> searchForOntologyTermsInStrings(Collection<String> ngrams)
	{
		int RANGE_FOR_PARENT_IN_DEFINITION = 5;

		List<String> queries = new ArrayList<String>();
		List<String> queriesHead = new ArrayList<String>();
		List<String[]> listOfArrays = new ArrayList<String[]>(ngrams.size());

		for (String string : ngrams) {
			listOfArrays.add(string.trim().split(" |-"));
		}

		for (String[] stringArray : listOfArrays) {
			StringBuffer buffer = new StringBuffer();
			for (int i = 0; i < RANGE_FOR_PARENT_IN_DEFINITION; i++) {
				if (i < stringArray.length) {
					buffer.append(stringArray[i]);
					buffer.append(" ");
				}
			}
			queriesHead.add(0, buffer.toString().trim());
		}
		queries.addAll(queriesHead);
		return adapter.lookupOntologyTermIdsFromIndex(queries);
	}

	/**
	 * Updates the <code>definition</code>, based on the selected and user
	 * defined definitions.
	 */
	public void updateEditDefArea()
	{
		StringBuffer defString = new StringBuffer();
		if (null != selectedCandidateTerm) {
			if (null != selectedCandidateTerm.getUserDefinedDefinition()) {
				defString.append(selectedCandidateTerm.getUserDefinedDefinition());
			}
			String definition = adapter.getDefinitionForExistingTerm(selectedCandidateTerm);
			if (null != definition && !defString.toString().equals(definition)) {
				if (defString.length() > 0) {
					defString.append("\n-----\n");
				}
				defString.append(definition);
			}
		}
		editDefArea.setText(defString.toString());
	}

	public void setTextSelectedLinkedObjectField(String s)
	{
		selectedLinkedObjectField.setText(s);
	}

	public OntologyLookupManagerPortTypeProxy getOntologyLookupProxy()
	{
		return ontoLookupProxy;
	}

	public String getId()
	{
		return id;
	}

	public CandidateTerm getSelectedCandidateTerm()
	{
		return selectedCandidateTerm;
	}

	public DefinitionsTable getDefinitionsTable()
	{
		return definitionTable;
	}

	public AbstractOntologyTermsTable<T, R> getOntologyTermsTable()
	{
		return ontologyTermsTable;
	}

	public TermsTable getTermsTable()
	{
		return candidateTermsTable;
	}

	/**
	 * Lays out the GUI
	 */
	public JComponent buildGUI()
	{
		// Set the layout of main window
		guiComponent.setLayout(new BoxLayout(guiComponent, BoxLayout.Y_AXIS));
		// 
		JPanel allStuffPanel = new JPanel();
		allStuffPanel.setLayout(new BoxLayout(allStuffPanel, BoxLayout.Y_AXIS));

		//
		// 1-Term Generation panel
		//
		termGenerationPanel = new JPanel(new BorderLayout(7, 7));

		// Set the Border with Bold font face
		final TitledBorder titledBorderTermGenerationPanel = new TitledBorder("Step 1: Term Generation");
		titledBorderTermGenerationPanel.setTitleFont(new Font(titledBorderTermGenerationPanel.getTitleFont().getFontName(), Font.BOLD, 18));
		titledBorderTermGenerationPanel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));

		// 1 sub panels
		// Input Panel

		JPanel inputPanel = new JPanel();
		inputPanel.setLayout(new BoxLayout(inputPanel, BoxLayout.X_AXIS));
		inputPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		inputPanel.setAlignmentY(Component.BOTTOM_ALIGNMENT);

		JTabbedPane inputTabPanel = new JTabbedPane();
		clipBoardTabPanel = new JTabbedPane();
		clipBoardTabPanel.setMaximumSize(new Dimension(200, 200));

		GridBagConstraints inputTextAreaConstraints = new GridBagConstraints();
		inputTextAreaConstraints.fill = GridBagConstraints.BOTH;
		inputTextAreaConstraints.ipady = 0; // make this component tall
		inputTextAreaConstraints.ipadx = 100;
		inputTextAreaConstraints.weightx = 0.5;
		inputTextAreaConstraints.gridwidth = 3;
		inputTextAreaConstraints.gridx = 0;
		inputTextAreaConstraints.gridy = 0;
		inputTextArea = new JTextArea(3, 4);
		JScrollPane inputTextScrollPane = new JScrollPane(inputTextArea);
		inputTextArea.setLineWrap(true);
		inputTextArea.setWrapStyleWord(true);
		inputTextScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);

		// Primary input panel
		JPanel primaryInputBoardPanelForPubMed = new JPanel();
		primaryInputBoardPanelForPubMed.setLayout(new BoxLayout(primaryInputBoardPanelForPubMed, BoxLayout.X_AXIS));

		// add components
		JLabel inputPubMedLabel = new JLabel("Query PubMed: ");
		Dimension spacer = new Dimension(3, 3);
		primaryInputBoardPanelForPubMed.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPubMed.add(inputPubMedLabel);
		primaryInputBoardPanelForPubMed.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPubMed.add(inputPubMedQueryField);
		primaryInputBoardPanelForPubMed.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPubMed.add(generateTermsFromPubMedButton);
		primaryInputBoardPanelForPubMed.add(Box.createRigidArea(spacer));

		JPanel primaryInputBoardPanelForWeb = new JPanel();
		primaryInputBoardPanelForWeb.setLayout(new BoxLayout(primaryInputBoardPanelForWeb, BoxLayout.X_AXIS));

		JLabel inputWebLabel = new JLabel("Query Web: ");
		primaryInputBoardPanelForWeb.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForWeb.add(inputWebLabel);
		primaryInputBoardPanelForWeb.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForWeb.add(inputWebQueryField);
		primaryInputBoardPanelForWeb.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForWeb.add(generateTermsFromWebButton);
		primaryInputBoardPanelForWeb.add(Box.createRigidArea(spacer));

		JPanel primaryInputBoardPanelForText = new JPanel();
		primaryInputBoardPanelForText.setLayout(new BoxLayout(primaryInputBoardPanelForText, BoxLayout.X_AXIS));
		// Set the Border with Bold font face
		// TitledBorder titledBorderPrimaryInputBoardPanelForText = new
		// TitledBorder("Generated From Text");
		// titledBorderPrimaryInputBoardPanelForText.setTitleFont(new
		// Font(titledBorderTermGenerationPanel.getTitleFont().getFontName(),
		// Font.PLAIN, 10));
		// titledBorderPrimaryInputBoardPanelForText.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));

		JLabel inputTextLabel = new JLabel("Paste Text: ");
		primaryInputBoardPanelForText.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForText.add(inputTextLabel);
		primaryInputBoardPanelForText.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForText.add(inputTextScrollPane, inputTextAreaConstraints);
		primaryInputBoardPanelForText.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForText.add(generateTermsFromTextButton);
		primaryInputBoardPanelForText.add(Box.createRigidArea(spacer));

		// primary input panel for Folder of pdf files
		JPanel primaryInputBoardPanelForPdfFolder = new JPanel();
		primaryInputBoardPanelForPdfFolder.setLayout(new BoxLayout(primaryInputBoardPanelForPdfFolder, BoxLayout.X_AXIS));

		JLabel inputFolderLabel = new JLabel("Folder/File: ");
		primaryInputBoardPanelForPdfFolder.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPdfFolder.add(inputFolderLabel);
		primaryInputBoardPanelForPdfFolder.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPdfFolder.add(inputFolderLocationField);
		primaryInputBoardPanelForPdfFolder.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPdfFolder.add(folderChooseButton);
		primaryInputBoardPanelForPdfFolder.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPdfFolder.add(generateTermsFromFolderButton);
		primaryInputBoardPanelForPdfFolder.add(Box.createRigidArea(spacer));

		inputTabPanel.addTab("PubMed", null, primaryInputBoardPanelForPubMed, Messages.getString("OntologyGenerationComponent.PubMedTab")); //$NON-NLS-2$
		inputTabPanel.addTab("Web", null, primaryInputBoardPanelForWeb, Messages.getString("OntologyGenerationComponent.WebTab")); //$NON-NLS-2$
		inputTabPanel.addTab("Text", null, primaryInputBoardPanelForText, Messages.getString("OntologyGenerationComponent.TextTab")); //$NON-NLS-2$
		inputTabPanel.addTab("PDF", null, primaryInputBoardPanelForPdfFolder, Messages.getString("OntologyGenerationComponent.PDFTab")); //$NON-NLS-2$
		inputPanel.add(inputTabPanel);
		inputPanel.setBorder(BorderFactory.createEmptyBorder(7, 0, 0, 0));

		// Clipboard panel
		JPanel clipBoardPanel = new JPanel();
		clipBoardPanel.setLayout(new BoxLayout(clipBoardPanel, BoxLayout.X_AXIS));
		clipBoardPanel.add(Box.createRigidArea(spacer));
		clipBoardPanel.add(showClipBoardButton);
		clipBoardPanel.add(Box.createRigidArea(spacer));
		clipBoardPanel.add(clearClipBoardButton);
		clipBoardPanel.add(Box.createRigidArea(spacer));
		clipBoardPanel.add(loadClipBoardButton);
		clipBoardPanel.add(Box.createRigidArea(spacer));
		clipBoardPanel.add(saveClipBoardButton);
		clipBoardPanel.add(Box.createRigidArea(spacer));

		primaryInputBoardPanelForPubMed.add(Box.createRigidArea(spacer));
		clipBoardTabPanel.addTab("Clipboard", clipBoardPanel);

		inputPanel.add(Box.createRigidArea(spacer));
		inputPanel.add(clipBoardTabPanel);

		if (inputPubMedQueryField.getText().trim().length() == 0) {
			generateTermsFromPubMedButton.setEnabled(false);
		}
		else {
			generateTermsFromPubMedButton.setEnabled(true);
		}

		if (inputWebQueryField.getText().trim().length() == 0) {
			generateTermsFromWebButton.setEnabled(false);
		}
		else {
			generateTermsFromWebButton.setEnabled(true);
		}

		if (inputTextArea.getText().trim().length() == 0) {
			generateTermsFromTextButton.setEnabled(false);
		}
		else {
			generateTermsFromTextButton.setEnabled(true);
		}

		if (inputFolderLocationField.getText().trim().length() == 0) {
			generateTermsFromFolderButton.setEnabled(false);
		}
		else {
			generateTermsFromFolderButton.setEnabled(true);
		}

		// Filter Term Panel to be added to InputPanel containing regular
		// expressions filtration
		JPanel filterTermPanel = new JPanel();
		filterTermPanel.setLayout(new BoxLayout(filterTermPanel, BoxLayout.Y_AXIS));
		filterTermPanel.setAlignmentX(Component.TOP_ALIGNMENT);

		JPanel filterTermUpperPanel = new JPanel();
		filterTermUpperPanel.setLayout(new BoxLayout(filterTermUpperPanel, BoxLayout.X_AXIS));
		filterTermUpperPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		JLabel searchTermLabel = new JLabel("Search: ");
		filterTermUpperPanel.add(searchTermLabel);
		searchTermsTextField = new FilterTextField(false);
		searchTermsTextField.setSize(200, 25);
		searchTermsTextField.setMaximumSize(new Dimension(200, 25));
		searchTermsTextField.setPreferredSize(new Dimension(150, 25));
		filterTermUpperPanel.add(searchTermsTextField);

		JLabel filterLabel = new JLabel(" Filter: ");
		filterTermUpperPanel.add(filterLabel);
		filterTermsTextField = new FilterTextField(true);
		filterTermsTextField.setSize(200, 25);
		filterTermsTextField.setMaximumSize(new Dimension(200, 25));
		filterTermsTextField.setPreferredSize(new Dimension(200, 25));
		filterTermUpperPanel.add(filterTermsTextField);
		onlyShowExistingTerms.setText("Show existing terms only.");
		onlyShowExistingTerms.setBackground(guiComponent.getBackground());
		onlyShowExistingTerms.setToolTipText(Messages.getString("OntologyGenerationComponent.onlyShowExistingTermsCheckbox")); //$NON-NLS-1$
		filterTermUpperPanel.add(Box.createRigidArea(spacer));
		filterTermUpperPanel.add(onlyShowExistingTerms);
		filterTermPanel.add(Box.createRigidArea(new Dimension(7, 0)));
		filterTermPanel.add(filterTermUpperPanel);
		filterTermPanel.add(Box.createRigidArea(spacer));

		// Add the subPanels to the Term Generation Panel
		termGenerationPanel.add(inputPanel, BorderLayout.NORTH);

		candidateTermsTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		candidateTermsTable.setPreferredScrollableViewportSize(new Dimension(200, 200));

		JPanel termsTableContainer = new JPanel();
		termsTableContainer.setLayout(new BoxLayout(termsTableContainer, BoxLayout.X_AXIS));
		termsTableContainer.add(Box.createRigidArea(new Dimension(7, 0)));
		scrollPaneForTermsTable = new JScrollPane(candidateTermsTable);
		termsTableContainer.add(scrollPaneForTermsTable);
		termsTableContainer.add(Box.createRigidArea(new Dimension(7, 0)));
		termGenerationPanel.setBorder(titledBorderTermGenerationPanel);
		termGenerationPanel.add(termsTableContainer, BorderLayout.CENTER);
		termGenerationPanel.add(filterTermPanel, BorderLayout.SOUTH);

		//
		// 2 Definition Generation panel
		//
		definitionGenerationPanel = new JPanel(new BorderLayout(7, 7));
		// set the Border with Bold font face
		final TitledBorder titledBorderDefPanel = new TitledBorder("Step 2: Definition Generation");
		titledBorderDefPanel.setTitleFont(new Font(titledBorderDefPanel.getTitleFont().getFontName(), Font.BOLD, 18));
		titledBorderDefPanel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
		definitionGenerationPanel.setBorder(titledBorderDefPanel);

		JPanel manualDefGenerationPanel = new JPanel();
		manualDefGenerationPanel.setLayout(new BoxLayout(manualDefGenerationPanel, BoxLayout.X_AXIS));

		JLabel manualDefGenLabel = new JLabel("Find Definitions for: ");
		inputDefinitionGenerationField = new JTextField();
		inputDefinitionGenerationField.setSize(200, 25);
		inputDefinitionGenerationField.setMaximumSize(new Dimension(200, 25));
		inputDefinitionGenerationField.setPreferredSize(new Dimension(200, 25));
		inputDefinitionGenerationField.setToolTipText(Messages.getString("OntologyGenerationComponent.ManualDefinitionField")); //$NON-NLS-1$
		generateManualDefinitionButton.setEnabled(false);
		manualDefGenerationPanel.add(Box.createRigidArea(new Dimension(7, 0)));
		manualDefGenerationPanel.add(manualDefGenLabel);
		manualDefGenerationPanel.add(inputDefinitionGenerationField);
		manualDefGenerationPanel.add(Box.createRigidArea(spacer));
		manualDefGenerationPanel.add(generateManualDefinitionButton);

		//
		// 2-Definition Generation sub panels
		//
		// Filter definitions
		JPanel filterDefPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		JLabel filterDefLabel = new JLabel("Filter: ");
		filterDefPanel.add(filterDefLabel);

		filterDefTextField = new FilterTextField(true);
		filterDefTextField.setSize(200, 25);
		filterDefTextField.setMaximumSize(new Dimension(200, 25));
		filterDefTextField.setPreferredSize(new Dimension(200, 25));
		filterDefPanel.add(filterDefTextField);

		// editorPanel to contain editDef and editAbbr panels
		JPanel editorPanel = new JPanel();
		editorPanel.setLayout(new GridBagLayout());

		JPanel editDefPanel = new JPanel(new GridBagLayout());
		editDefPanel.setBorder(new TitledBorder("Edit Definition"));

		JPanel editAbbrPanel = new JPanel();

		editAbbrPanel.setLayout(new GridBagLayout());
		editAbbrPanel.setBorder(new TitledBorder("Abbreviations, synonyms and known child terms (OBO)"));

		GridBagConstraints editDefAreaConstraints = new GridBagConstraints();
		editDefAreaConstraints.fill = GridBagConstraints.BOTH;
		editDefAreaConstraints.ipady = 0; // make this component tall
		editDefAreaConstraints.ipadx = 100;
		editDefAreaConstraints.weightx = 0.5;
		editDefAreaConstraints.gridwidth = 3;
		editDefAreaConstraints.gridx = 0;
		editDefAreaConstraints.gridy = 0;

		editDefArea = new JTextArea();
		JScrollPane scrollPaneForDefinition = new JScrollPane(editDefArea);
		editDefArea.setLineWrap(true);
		editDefArea.setWrapStyleWord(true);
		scrollPaneForDefinition.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		scrollPaneForDefinition.setPreferredSize(new Dimension(200, 80));
		editDefPanel.add(scrollPaneForDefinition, editDefAreaConstraints);

		GridBagConstraints saveDefButtonConstraints = new GridBagConstraints();
		saveDefButtonConstraints.fill = GridBagConstraints.HORIZONTAL;
		saveDefButtonConstraints.ipady = 0; // reset to default
		saveDefButtonConstraints.weighty = 0.0; // request any extra vertical
		saveDefButtonConstraints.anchor = GridBagConstraints.PAGE_END; // bottom
		saveDefButtonConstraints.insets = new Insets(5, 225, 0, 0); // top
		saveDefButtonConstraints.gridwidth = 1; // 1 columns wide
		saveDefButtonConstraints.gridheight = 2;
		saveDefButtonConstraints.gridx = 1; // second column
		saveDefButtonConstraints.gridy = 1; // second row
		editDefPanel.add(saveDefButton, saveDefButtonConstraints);

		GridBagConstraints saveDefWarningLabelConstraints = new GridBagConstraints();
		saveDefWarningLabelConstraints.insets = new Insets(5, 0, 0, 0); // top
		saveDefWarningLabelConstraints.gridheight = 2;
		saveDefWarningLabelConstraints.anchor = GridBagConstraints.CENTER;
		saveDefWarningLabelConstraints.gridx = 2; // second column
		saveDefWarningLabelConstraints.gridy = 1; // second row

		saveDefWarningLabel.setFont(saveDefWarningLabel.getFont().deriveFont(Font.BOLD));
		saveDefWarningLabel.setForeground(Color.red);
		saveDefWarningLabel.setPreferredSize(new Dimension(140, 15));

		editDefPanel.add(saveDefWarningLabel, saveDefWarningLabelConstraints);

		synonymTermsTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		synonymTermsTable.getTableHeader().setVisible(false);
		synonymTermsTable.getTableHeader().setPreferredSize(new Dimension(200, 0));
		scrollPaneForSynonymTermsTable = new JScrollPane(synonymTermsTable);
		scrollPaneForSynonymTermsTable.setPreferredSize(new Dimension(150, 80));
		editDefAreaConstraints.ipadx = 0;
		editDefAreaConstraints.gridwidth = 2;
		editAbbrPanel.add(scrollPaneForSynonymTermsTable, editDefAreaConstraints);
		GridBagConstraints abbrButtonConstraints = new GridBagConstraints();
		abbrButtonConstraints.gridwidth = 1;
		abbrButtonConstraints.insets = new Insets(5, 0, 0, 0);
		abbrButtonConstraints.gridx = 1;
		abbrButtonConstraints.gridy = 1;
		saveAbbrButton.setEnabled(false); // TODO make abbreviations editable,
		// and enable adding
		editAbbrPanel.add(saveAbbrButton, abbrButtonConstraints);

		GridBagConstraints defPaneConstraints = new GridBagConstraints();
		defPaneConstraints.fill = GridBagConstraints.HORIZONTAL;
		defPaneConstraints.gridwidth = 2;
		defPaneConstraints.gridx = 0;
		defPaneConstraints.gridy = 0;
		defPaneConstraints.weightx = 0.5;

		editorPanel.add(editDefPanel, defPaneConstraints);
		defPaneConstraints.gridwidth = 1;
		defPaneConstraints.gridx = 2;
		defPaneConstraints.gridy = 0;
		defPaneConstraints.weightx = 0.5;
		editorPanel.add(editAbbrPanel, defPaneConstraints);

		// southPanel will contain filterDefPanel and editorPanel
		JPanel southDefPanel = new JPanel();
		southDefPanel.setLayout(new BoxLayout(southDefPanel, BoxLayout.Y_AXIS));
		southDefPanel.add(filterDefPanel);
		southDefPanel.add(editorPanel);

		// Add the subPanels to the Definition Generation Panel
		definitionTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		definitionTable.setMinimumPreferedeScrollableViewportHeight(40);
		definitionTable.setMaximumPreferedeScrollableViewportHeight(200);
		definitionTable.setPreferredScrollableViewportSize(new Dimension(200, 40));

		JPanel definitionTableContainer = new JPanel();
		definitionTableContainer.setLayout(new BoxLayout(definitionTableContainer, BoxLayout.X_AXIS));
		definitionTableContainer.add(Box.createRigidArea(new Dimension(7, 0)));
		JScrollPane scrollPaneForDefinitionsTable = new JScrollPane(definitionTable);
		definitionTableContainer.add(scrollPaneForDefinitionsTable);
		definitionTableContainer.add(Box.createRigidArea(new Dimension(7, 0)));
		definitionGenerationPanel.add(Box.createRigidArea(new Dimension(0, 5)), BorderLayout.NORTH);
		manualDefGenerationPanel.setBorder(BorderFactory.createEmptyBorder(10, 0, 0, 0));
		definitionGenerationPanel.add(manualDefGenerationPanel, BorderLayout.NORTH);
		definitionGenerationPanel.add(definitionTableContainer, BorderLayout.CENTER);
		definitionGenerationPanel.add(southDefPanel, BorderLayout.SOUTH);

		//
		// 3-Add child to ontology Panel and 3 sub panels
		//
		final TitledBorder titledBorderAddToOntologyPanel = new TitledBorder("Step 3: Add to Ontology");
		titledBorderAddToOntologyPanel.setTitleFont(new Font(titledBorderAddToOntologyPanel.getTitleFont().getFontName(), Font.BOLD, 18));
		titledBorderAddToOntologyPanel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));

		addToOntologyPanel = new JPanel();
		addToOntologyPanel.setLayout(new BoxLayout(addToOntologyPanel, BoxLayout.Y_AXIS));

		JPanel candidateToAddPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		JLabel label = new JLabel("Add Term: ");
		Font boldFont = new Font(label.getFont().getName(), Font.BOLD, label.getFont().getSize());
		label.setFont(boldFont);
		editNameTextField.setToolTipText(Messages.getString("OntologyGenerationComponent.ManualTermNameField")); //$NON-NLS-1$
		candidateToAddPanel.add(label);
		candidateToAddPanel.add(editNameTextField);
		candidateToAddPanel.add(saveLabelButton);
		candidateToAddPanel.add(saveLabelWarningLabel);

		saveLabelWarningLabel.setFont(saveDefWarningLabel.getFont().deriveFont(Font.BOLD));
		saveLabelWarningLabel.setForeground(Color.red);
		saveLabelWarningLabel.setPreferredSize(new Dimension(140, 15));

		checkboxIncludeChildren = new JCheckBox("Include children");
		checkboxIncludeChildren.setBackground(guiComponent.getBackground());
		checkboxIncludeChildren.setToolTipText(Messages.getString("OntologyGenerationComponent.IncludeChildrenCheckbox")); //$NON-NLS-1$
		candidateToAddPanel.add(checkboxIncludeChildren);
		/*
		 * checkboxIncludeBranch = new JCheckBox();
		 * candidateToAddPanel.add(checkboxIncludeBranch); JLabel label3 = new
		 * JLabel("include sub-branch"); candidateToAddPanel.add(label3);
		 */
		JPanel oboClassTableHeaderPanel = new JPanel(new BorderLayout());

		JPanel parentLabelPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		JLabel parentLabel = new JLabel("Potential parent terms (existing in OBO-Edit):");
		parentLabel.setFont(boldFont);
		parentLabelPanel.add(parentLabel);

		oboClassTableHeaderPanel.add(parentLabelPanel, BorderLayout.WEST);

		JPanel oboClassTablePanelContainer = new JPanel();
		oboClassTablePanelContainer.setLayout(new BoxLayout(oboClassTablePanelContainer, BoxLayout.Y_AXIS));
		JPanel oboClassTablePanel = new JPanel();
		oboClassTablePanel.setLayout(new BoxLayout(oboClassTablePanel, BoxLayout.X_AXIS));

		JScrollPane scrollPaneForPotentialParents = new JScrollPane(this.ontologyTermsTable);
		oboClassTablePanel.add(Box.createRigidArea(new Dimension(7, 0)));
		oboClassTablePanel.add(scrollPaneForPotentialParents);
		addToOntologyButton = new JButton("<html><center>Add term<br>to<br>Ontology</center></html>");
		addToOntologyButton.setMaximumSize(new Dimension(110, 330));
		addToOntologyButton.setFont(boldFont);
		addToOntologyButton.setToolTipText(Messages.getString("OntologyGenerationComponent.AddTermToOntologyButton")); //$NON-NLS-1$
		oboClassTablePanel.add(Box.createRigidArea(new Dimension(7, 0)));
		oboClassTablePanel.add(this.addToOntologyButton);
		oboClassTablePanel.add(Box.createRigidArea(new Dimension(7, 0)));

		// Filter Term Panel to be added to InputPanel containing regular
		// expressions filtration
		JPanel filterPotentialParentTermsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		filterPotentialParentTermsPanel.add(new JLabel("Filter: "));
		filterPotentialParentsTextField = new FilterTextField(true);
		filterPotentialParentsTextField.setSize(200, 25);
		filterPotentialParentsTextField.setMaximumSize(new Dimension(200, 25));
		filterPotentialParentsTextField.setPreferredSize(new Dimension(200, 25));

		checkboxShowTickedParents = new JCheckBox("Show ticked parent terms only.");
		checkboxShowTickedParents.setBackground(guiComponent.getBackground());
		checkboxShowTickedParents.setToolTipText(Messages.getString("OntologyGenerationComponent.ShowTickedParentTermsOnlyCheckbox")); //$NON-NLS-1$
		checkboxShowTickedParents.setEnabled(true);

		filterPotentialParentTermsPanel.add(filterPotentialParentsTextField);
		filterPotentialParentTermsPanel.add(checkboxShowTickedParents);

		oboClassTablePanelContainer.add(oboClassTableHeaderPanel);
		oboClassTablePanelContainer.add(oboClassTablePanel);

		addToOntologyPanel.setBorder(titledBorderAddToOntologyPanel);

		addToOntologyPanel.add(candidateToAddPanel, BorderLayout.NORTH);
		addToOntologyPanel.add(oboClassTablePanelContainer, BorderLayout.CENTER);
		addToOntologyPanel.add(filterPotentialParentTermsPanel, BorderLayout.SOUTH);

		final JPanel termGenerationPanelContainer = new JPanel();
		termGenerationPanelContainer.setLayout(new BoxLayout(termGenerationPanelContainer, BoxLayout.Y_AXIS));
		final JPanel definitonGenerationPanelContainer = new JPanel();
		definitonGenerationPanelContainer.setLayout(new BoxLayout(definitonGenerationPanelContainer, BoxLayout.Y_AXIS));
		final JPanel addToOntologyPanelContainer = new JPanel();
		addToOntologyPanelContainer.setLayout(new BoxLayout(addToOntologyPanelContainer, BoxLayout.Y_AXIS));
		final JPanel termGenerationPanelForTab = new JPanel();
		termGenerationPanelForTab.setLayout(new BoxLayout(termGenerationPanelForTab, BoxLayout.Y_AXIS));
		final JPanel definitonGenerationPanelForTab = new JPanel();
		definitonGenerationPanelForTab.setLayout(new BoxLayout(definitonGenerationPanelForTab, BoxLayout.Y_AXIS));
		final JPanel addToOntologyPanelForTab = new JPanel();
		addToOntologyPanelForTab.setLayout(new BoxLayout(addToOntologyPanelForTab, BoxLayout.Y_AXIS));
		final JPanel aboutPanelForTab = new JPanel();
		aboutPanelForTab.setLayout(new FlowLayout());

		String url = "http://www.biotec.tu-dresden.de/~waechter/obo-edit-ontogen/";
		JScrollPane scrollPaneForHelp = null;
		try {
			JEditorPane htmlPane = new JEditorPane(url);
			htmlPane.setEditable(false);
			scrollPaneForHelp = new JScrollPane(htmlPane);
			scrollPaneForHelp.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		}
		catch (IOException ioe) {
			logger.error("Error displaying " + url);
		}
		JPanel splashPanel = BiotecSplashScreen.getSplashPanel();
		aboutPanelForTab.add(splashPanel);

		// TODO scrolling to slow, increase step size
		allStuffPanel.add(termGenerationPanelContainer);
		allStuffPanel.add(Box.createRigidArea(new Dimension(0, 15)));
		allStuffPanel.add(definitonGenerationPanelContainer);
		allStuffPanel.add(Box.createRigidArea(new Dimension(0, 15)));
		allStuffPanel.add(addToOntologyPanelContainer);

		// create scroll panes
		JScrollPane scrollPane1 = new JScrollPane(allStuffPanel);
		scrollPane1.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		scrollPane1.getVerticalScrollBar().setUnitIncrement(10);

		JScrollPane scrollPane2 = new JScrollPane(termGenerationPanelForTab);
		scrollPane2.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		scrollPane2.getVerticalScrollBar().setUnitIncrement(10);

		JScrollPane scrollPane3 = new JScrollPane(definitonGenerationPanelForTab);
		scrollPane3.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		scrollPane3.getVerticalScrollBar().setUnitIncrement(10);

		final JTabbedPane mainTabbedPane = new JTabbedPane();
		mainTabbedPane.addTab("All Steps", null, scrollPane1, Messages.getString("OntologyGenerationComponent.AllStepsTab")); //$NON-NLS-2$
		mainTabbedPane.setForegroundAt(mainTabbedPane.getComponentCount() - 1, Color.BLACK);
		mainTabbedPane.addTab("Terms Generation", null, scrollPane2, Messages.getString("OntologyGenerationComponent.TermGenerationTab")); //$NON-NLS-2$
		mainTabbedPane.setForegroundAt(mainTabbedPane.getComponentCount() - 1, Color.BLACK);
		mainTabbedPane.addTab("Definition Generation", null, scrollPane3, Messages.getString("OntologyGenerationComponent.DefinitionGenerationTab")); //$NON-NLS-2$
		mainTabbedPane.setForegroundAt(mainTabbedPane.getComponentCount() - 1, Color.BLACK);
		mainTabbedPane.addTab("Help", scrollPaneForHelp);
		mainTabbedPane.setForegroundAt(mainTabbedPane.getComponentCount() - 1, Color.DARK_GRAY);
		mainTabbedPane.addTab("About", aboutPanelForTab);
		mainTabbedPane.setForegroundAt(mainTabbedPane.getComponentCount() - 1, Color.DARK_GRAY);

		// init for all panels in one tab
		termGenerationPanelContainer.add(termGenerationPanel);
		definitonGenerationPanelContainer.add(definitionGenerationPanel);
		addToOntologyPanelContainer.add(addToOntologyPanel);

		mainTabbedPane.addChangeListener(new ChangeListener()
		{

			public void stateChanged(ChangeEvent e)
			{
				termGenerationPanelContainer.removeAll();
				termGenerationPanelForTab.removeAll();
				definitonGenerationPanelContainer.removeAll();
				definitonGenerationPanelForTab.removeAll();
				addToOntologyPanelContainer.removeAll();
				addToOntologyPanelForTab.removeAll();

				if (mainTabbedPane.getSelectedIndex() == 0) {
					termGenerationPanelContainer.add(termGenerationPanel);
					definitonGenerationPanelContainer.add(definitionGenerationPanel);
					addToOntologyPanelContainer.add(addToOntologyPanel);
					termGenerationPanel.setBorder(titledBorderTermGenerationPanel);
					definitionGenerationPanel.setBorder(titledBorderDefPanel);
					addToOntologyPanel.setBorder(titledBorderAddToOntologyPanel);
				}
				else {
					termGenerationPanel.setBorder(BorderFactory.createEmptyBorder());
					definitionGenerationPanel.setBorder(BorderFactory.createEmptyBorder());
					addToOntologyPanel.setBorder(BorderFactory.createEmptyBorder());
					termGenerationPanelForTab.add(termGenerationPanel);
					definitonGenerationPanelForTab.add(definitionGenerationPanel);
					addToOntologyPanelForTab.add(addToOntologyPanel);
				}
			}
		});

		JPanel mainTabbedPaneContainer = new JPanel();
		mainTabbedPaneContainer.setLayout(new BoxLayout(mainTabbedPaneContainer, BoxLayout.Y_AXIS));
		mainTabbedPaneContainer.add(mainTabbedPane);

		return mainTabbedPane;
	}

	/**
	 * Displays or hides the progress bar dialog with the default wait text
	 * 
	 * @param displayDlg
	 *            if true, display progress bar dialog, hide otherwise
	 */
	public void showProgressDlg(boolean displayDlg)
	{
		showProgressDlg(displayDlg, null);
	}

	/**
	 * Displays or hides the progress bar dialog
	 * 
	 * @param displayDlg
	 *            if true, display progress bar dialog, hide otherwise
	 * @param displayMsg
	 *            The wait text inside the dialog. If null, display default wait
	 *            text
	 */
	public void showProgressDlg(boolean displayDlg, String displayMsg)
	{
		if (displayDlg) {
			progressDlg = new ProgressBarDialog(guiComponent, displayMsg);
			progressDlg.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			progressDlg.setVisible(true);
		}
		else {
			if (progressDlg != null) {
				progressDlg.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
				progressDlg.setVisible(false);
				progressDlg = null;
			}
		}

	}

	private class OntologyLookupServiceWorker extends SwingWorker<Void, Void>
	{
		/**
		 * Constructs an {@link OntologyLookupServiceWorker}
		 */
		public OntologyLookupServiceWorker()
		{
			super();
			if (ontoLookupProxy == null) {
				ontoLookupProxy = new OntologyLookupManagerPortTypeProxy();
			}
		}

		/**
		 * @return {@link Void}
		 * @throws Exception
		 * @see org.jdesktop.swingworker.SwingWorker#doInBackground()
		 */
		@SuppressWarnings("unchecked")
		@Override
		protected Void doInBackground() throws Exception
		{
			while (true) {
				try {
					final CandidateTerm candidateTerm = ontologyLookupQueue.take();
					// logger.trace(candidateTerm.getLabel());
					if (candidateTerm.getExistingOntologyTerms() == null) {
						final Set<String> lexicalRepresentations = candidateTerm.getLexicalRepresentations();
						String[] labels = new String[lexicalRepresentations.size()];
						labels = lexicalRepresentations.toArray(labels);
						final OBOLookupTerm[] existingOntologyTerms = ontoLookupProxy.lookupExactConcept(null, labels);
						if (existingOntologyTerms == null) {
							candidateTerm.setExistingOntologyTerms(Collections.EMPTY_LIST);
						}
						else {
							candidateTerm.setExistingOntologyTerms(Arrays.asList(existingOntologyTerms));
							synchronized (OntologyGenerationComponent.class) {
								for (int i = candidateTermsTable.getCurrentFirstVisibleRow(); i <= candidateTermsTable.getCurrentLastVisibleRow(); i++) {
									// TODO destroys table witdh (BUG) /
									// synchronize with resize and set terms
									candidateTermsTable.getModel().fireTableCellUpdated(i, 1);
								}
							}
						}
					}
				}
				catch (Throwable exception) {
					logger.error("Ontology lookup service failure!", exception);
				}
			}
		}
	}

	private class OntologyLookupChildrenServiceWorker extends SwingWorker<Void, Void>
	{
		/**
		 * Constructs an {@link OntologyLookupChildrenServiceWorker}
		 */
		public OntologyLookupChildrenServiceWorker()
		{
			super();
			if (ontoLookupProxy == null) {
				ontoLookupProxy = new OntologyLookupManagerPortTypeProxy();
			}
		}

		/**
		 * @return {@link Void}
		 * @throws Exception
		 * @see org.jdesktop.swingworker.SwingWorker#doInBackground()
		 */
		@SuppressWarnings("unchecked")
		@Override
		protected Void doInBackground() throws Exception
		{
			while (true) {
				logger.trace("TAKE: ");
				final CandidateTerm candidateTerm = ontologyLookupChildrenQueue.take();
				logger.trace("TAKEN: " + candidateTerm.getLabel());

				guiComponent.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

				try {
					if (candidateTerm.getExistingChildTerms() == null) {
						final Set<String> lexicalRepresentations = candidateTerm.getLexicalRepresentations();
						String[] labels = new String[lexicalRepresentations.size()];
						labels = lexicalRepresentations.toArray(labels);
						// update existing OBO terms
						if (candidateTerm.getExistingOntologyTerms() == null) {
							logger.trace("OBO Lookup: ");
							final OBOLookupTerm[] existingOntologyTerms = ontoLookupProxy.lookupExactConcept(null, labels);
							logger.trace(candidateTerm.getLabel());
							if (existingOntologyTerms == null) {
								candidateTerm.setExistingOntologyTerms(Collections.EMPTY_LIST);
							}
							else {
								List<OBOLookupTerm> list = Arrays.asList(existingOntologyTerms);
								candidateTerm.setExistingOntologyTerms(list);
							}
						}
						// update existing OBO child terms
						if (candidateTerm.getExistingChildTerms() == null) {
							Map<String, OBOLookupTerm> allChildrenMap = new HashMap<String, OBOLookupTerm>();
							List<OBOLookupTerm> allChildren = new ArrayList<OBOLookupTerm>();
							List<OBOLookupRelation> allRelations = new ArrayList<OBOLookupRelation>();
							if (candidateTerm.getExistingOntologyTerms().size() > 0) {
								for (OBOLookupTerm term : candidateTerm.getExistingOntologyTerms()) {
									OBOLookupResult result = ontoLookupProxy.getChildren(term.getOboID(), 1);
									if (result != null) {
										if (result.getTerms() != null) {
											for (OBOLookupTerm childTerm : result.getTerms()) {
												allChildrenMap.put(childTerm.getOboID(), childTerm);
											}
										}
										if (result.getRelations() != null) {
											for (OBOLookupRelation relation : result.getRelations()) {
												allRelations.add(relation);
												OBOLookupTerm childTerm = allChildrenMap.get(relation.getOboChildTermID());
												allChildren.add(childTerm);
											}
										}
									}
								}
							}
							candidateTerm.setExistingChildTerms(allChildren);
							candidateTerm.setExistingChildRelations(allRelations);
							updateSynonymOrChildTable();
						}
					}
				}
				catch (Throwable exception) {
					logger.error("Ontology children lookup service failure!", exception);
				}
				finally {
					// Reset the cursor in normal mode
					if (ontologyLookupChildrenQueue.isEmpty()) {
						guiComponent.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
					}
				}
			}
		}
	}

	/**
	 * Inner Class to invoke {@link GoPubMedTermGenerationStub} in a separate
	 * worker thread
	 */
	private class TermGenerationServiceWorker extends SwingWorker<TextConceptRepresentation[], Void>
	{
		private String inputData = new String();
		private final TermsTable table;
		private final String source;
		private StringBuffer errorMessage;

		/**
		 * Constructs a {@link TermGenerationServiceWorker}
		 * 
		 * @param inputData
		 *            , the query or text uses for the generation
		 * @param destinationTable
		 *            , the table where to place the terms
		 * @param textSourceName
		 *            , the source type (e.g. TEXT,PUBMED)
		 */
		public TermGenerationServiceWorker(String inputData, TermsTable destinationTable, String textSourceName)
		{
			this.inputData = prepareTextReplaceInvalidCharacter(inputData);
			this.table = destinationTable;
			this.source = textSourceName;
			if (termGenerationServiceStub == null) {
				try {
					termGenerationServiceStub = new GoPubMedTermGenerationStub();
				}
				catch (AxisFault exception) {
					logger.warn("Term generation initialization failed.", exception);
				}
			}
			else {
				try {
					termGenerationServiceStub.cleanup();
				}
				catch (AxisFault exception) {
					try {
						termGenerationServiceStub = new GoPubMedTermGenerationStub();
					}
					catch (AxisFault exception2) {
						logger.warn("Term generation initialization failed.", exception);
					}
				}
			}

		}

		@Override
		public TextConceptRepresentation[] doInBackground()
		{
			this.setProgress(0);
			TextConceptRepresentation[] concepts = new TextConceptRepresentation[1000];
			try {
				if (source.equals(SOURCE_PUBMED)) {
					candidateTermsTable.setShowInformationIcon(true);
					GenerateConceptsFromPubMedQuery query = new GoPubMedTermGenerationStub.GenerateConceptsFromPubMedQuery();
					query.setMaxNumberOfTerms(1000);
					query.setQueryString(inputData);
					query.setApplicationCode(PLUGIN_VERSIONED_NAME);
					GenerateConceptsFromPubMedQueryResponse response = termGenerationServiceStub.generateConceptsFromPubMedQuery(query);
					concepts = response.get_return();
				}
				if (source.equals(SOURCE_WEB)) {
					candidateTermsTable.setShowInformationIcon(true);
					GenerateConceptsFromWebQuery query = new GoPubMedTermGenerationStub.GenerateConceptsFromWebQuery();
					query.setMaxNumberOfTerms(1000);
					query.setQueryString(inputData);
					query.setApplicationCode(PLUGIN_VERSIONED_NAME);
					GenerateConceptsFromWebQueryResponse response = termGenerationServiceStub.generateConceptsFromWebQuery(query);
					concepts = response.get_return();
				}
				if (source.equals(SOURCE_TEXT)) {
					candidateTermsTable.setShowInformationIcon(false);
					String[] lines = inputData.split("\n");
					GenerateConceptsFromText query = new GoPubMedTermGenerationStub.GenerateConceptsFromText();
					query.setMaxNumberOfTerms(1000);
					query.setTexts(lines);
					query.setApplicationCode(PLUGIN_VERSIONED_NAME);
					GenerateConceptsFromTextResponse response = termGenerationServiceStub.generateConceptsFromText(query);
					concepts = response.get_return();
				}
				if (source.equals(SOURCE_FOLDER)) {
					candidateTermsTable.setShowInformationIcon(false);
					File file = new File(inputData);
					String data = "";
					PdfToTextExtraction pdfParser = new PdfToTextExtraction();
					errorMessage = new StringBuffer();
					if (file.isDirectory()) {
						String extractedString = "";
						File[] fileArray = file.listFiles();

						for (int i = 0; i < fileArray.length; i++) {
							if (fileArray[i].isFile()) {
								extractedString = pdfParser.fileExtraction(fileArray[i]);
								if (extractedString.equals(PdfToTextExtraction.ERROR)) {
									errorMessage.append(fileArray[i] + "]");
								}
								else {
									data += extractedString;
								}
							}
						}
					}
					else if (file.isFile()) {
						data += pdfParser.fileExtraction(file);
						if (data.equals(PdfToTextExtraction.ERROR)) {
							errorMessage.append(file);
							data = "";
						}
					}

					if (!data.equals("")) {
						String[] pdfText = data.split("\n");
						GenerateConceptsFromText query = new GoPubMedTermGenerationStub.GenerateConceptsFromText();
						query.setMaxNumberOfTerms(1000);
						query.setTexts(pdfText);
						query.setApplicationCode(PLUGIN_VERSIONED_NAME);
						GenerateConceptsFromTextResponse response = termGenerationServiceStub.generateConceptsFromText(query);
						concepts = response.get_return();
					}
				}
			}
			catch (RemoteException exception) {
				logger.warn("Term generation failed.", exception);
			}
			this.setProgress(50);
			return concepts;
		}

		@Override
		public void done()
		{
			List<CandidateTerm> termsFromService = new ArrayList<CandidateTerm>();
			TextConceptRepresentation[] concepts = new TextConceptRepresentation[1000];
			try {
				concepts = get();
			}
			catch (InterruptedException ignore) {
				this.setProgress(100);
			}
			catch (java.util.concurrent.ExecutionException exception) {
				this.setProgress(100);
				JOptionPane.showMessageDialog(guiComponent, "Error retrieving terms. Are you connected to the Internet?");
				logger.error("Error retrieving terms!", exception);
				// reset the cursor to normal mode
				guiComponent.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			}

			if (source.equals(SOURCE_FOLDER)) {
				String errors = errorMessage.toString();
				if (!errors.equals("")) {
					StringBuffer message = new StringBuffer();
					message.append("Unfortunately, it was not possible to extract information from the given files.\n");
					String files[] = errors.split("]");
					for (int i = 0; i < files.length; ++i) {
						message.append(files[i] + "\n");
					}
					this.setProgress(100);
					JOptionPane.showMessageDialog(guiComponent, message.toString());
				}
			}

			if (concepts == null) {
				// close the progress bar
				this.setProgress(100);
				JOptionPane.showMessageDialog(guiComponent, "No candidate terms found for query '" + inputData + "'.");
				// reset the cursor to normal mode
				guiComponent.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			}
			else {
				for (TextConceptRepresentation concept : concepts) {
					if (concept != null) {
						// set Each OBoTermlabel
						CandidateTerm candidateTerm = null;
						if (candidateTermCache.hasCandidateTermWithLabel(concept.getLabel())) {
							candidateTerm = candidateTermCache.get(concept.getLabel());
							candidateTerm = updateCandidateTermWithConcept(concept, candidateTerm);
						}
						else if (clipboard.hasCandidateTermWithLabel(concept.getLabel())) {
							candidateTerm = clipboard.get(concept.getLabel());
							candidateTerm = updateCandidateTermWithConcept(concept, candidateTerm);
						}
						else {
							candidateTerm = new CandidateTerm(concept.getLabel(), concept.getKnownAbbreviation(), concept.getLexicalRepresentation(), concept
									.getScore(), CandidateTerm.TYPE_GENERATED);
						}
						termsFromService.add(candidateTerm);
					}
				}
				updateTermsTableHeader(source, inputData);

				this.table.setTerms(termsFromService);

				for (CandidateTerm generatedTerm : termsFromService) {
					if (table.getModel().isInClipboard(generatedTerm)) {
						// add merged existing from clipboard with generated
						table.getModel().addTermToClipboard(generatedTerm);
					}
				}
				// Scroll and select
				JTableHelper.scrollToTopAndSelectFirst(this.table);

				// trigger update ontology lookup
				updateTermsTableUsingOntologyLookup(this.table);

				// finish progessbar
				this.setProgress(100);

			}
		}

		/**
		 * Update an given candidate term with the information
		 * 
		 * @param concept
		 * @param candidateTerm
		 */
		private CandidateTerm updateCandidateTermWithConcept(TextConceptRepresentation concept, CandidateTerm candidateTerm)
		{
			if (null != concept.getKnownAbbreviation()) {
				for (String abbr : concept.getKnownAbbreviation()) {
					candidateTerm.addAbbreviation(abbr);
				}
			}
			if (null != concept.getLexicalRepresentation()) {
				for (String lex : concept.getLexicalRepresentation()) {
					candidateTerm.addLexicalRepresentation(lex);
				}
			}
			candidateTerm.setScore(Math.max(candidateTerm.getScore(), concept.getScore()));
			return candidateTerm;
		}
	}

	/**
	 * Worker to invoke the {@link GoPubMedDefinitionGeneratorStub} in a
	 * separate thread
	 */
	private class DefinitionGenerationServiceWorker extends SwingWorker<DefinitionContainer[], Void>
	{
		private String qTerm = new String();
		private final String[] parents;
		private final DefinitionsTable table;

		/**
		 * Constructs a {@link DefinitionGenerationServiceWorker}
		 * 
		 * @param term
		 *            , the term to be defined
		 * @param relatedTerms
		 *            , known terms associated with the term (e.g. known parent
		 *            terms)
		 */
		public DefinitionGenerationServiceWorker(String term, String[] relatedTerms, DefinitionsTable destinationTable)
		{
			this.qTerm = prepareTextReplaceInvalidCharacter(term);
			this.parents = relatedTerms;
			this.table = destinationTable;
			if (definitionGeneratorStub == null) {
				try {
					definitionGeneratorStub = new GoPubMedDefinitionGeneratorStub();
				}
				catch (AxisFault exception) {
					logger.error("Definition generation initialization failed.", exception);
				}
			}
			else {
				try {
					definitionGeneratorStub.cleanup();
				}
				catch (AxisFault exception) {
					try {
						definitionGeneratorStub = new GoPubMedDefinitionGeneratorStub();
					}
					catch (AxisFault exception2) {
						logger.error("Definition generation initialization failed.", exception2);
					}
				}
			}
		}

		@Override
		public DefinitionContainer[] doInBackground()
		{
			this.setProgress(0);
			DefinitionContainer[] defs = null;
			try {
				GetDefinitions query = new GoPubMedDefinitionGeneratorStub.GetDefinitions();
				String[] termLabels = { qTerm };
				query.setApplicationCode(PLUGIN_VERSIONED_NAME);
				query.setTermLabels(termLabels);
				query.setKnownTerms(parents);
				GetDefinitionsResponse response = definitionGeneratorStub.getDefinitions(query);
				defs = response.get_return();
			}
			catch (Exception exception) {
				logger.error("Definition generation failed", exception);
			}
			finally {
				this.setProgress(50);
			}
			return defs;

		}

		@Override
		public void done()
		{
			List<CandidateDefinition> defList = new ArrayList<CandidateDefinition>();
			DefinitionContainer[] defs = null;
			// add manual generated definitions
			if (table.getModel().getDefinitions() != null) {
				List<CandidateDefinition> manDefinitions = table.getModel().getDefinitions();
				for (CandidateDefinition definition : manDefinitions) {
					defList.add(definition);
				}
			}
			try {
				defs = get();
			}
			catch (InterruptedException ignore) {
				logger.error("Definition Generation interuped.", ignore);
			}
			catch (ExecutionException exception) {
				logger.error("Definition Generation failed.", exception);
			}
			if (defs != null) {
				int index = 0;

				for (final DefinitionContainer def : defs) {
					organizeDefinition(def, defList, index);
				}
				CandidateTerm termToDefine = selectedCandidateTerm;
				if (termToDefine != null) {
					this.table.getColumnModel().getColumn(1).setHeaderValue("Definitions for \"" + termToDefine.getLabel() + "\"");
					selectedCandidateTerm.setGeneratedDefinitions(defList);
					if (defList.size() > 0) {
						candidateTermCache.addTerm(selectedCandidateTerm);
					}
				}
				else {
					logger.trace("Invalid selection in termsTable, term is assumed to be selected");
				}
				this.table.setDefinitions(defList);
			}
			else {
				this.setProgress(100);
				JOptionPane.showMessageDialog(guiComponent, "No definitions found for term: \"" + selectedCandidateTerm.getLabel() + "\"");
			}

			// start extension of definitions by parsing from web pages (only
			// check the first 3 incomplete
			int extendCount = 0;
			for (Iterator<CandidateDefinition> iterator = defList.iterator(); iterator.hasNext();) {
				CandidateDefinition candidateDefinition = iterator.next();
				if (candidateDefinition.getDefinition().endsWith("...") || !candidateDefinition.getDefinition().endsWith(".")) {
					DefinitionExtensionWorker worker = new DefinitionExtensionWorker(candidateDefinition, this.table);
					worker.execute();
					extendCount++;
					if (extendCount % 10 == 0) {
						break;
					}
				}
			}
			this.setProgress(100);

			// Reset the cursor in normal mode
			updateParentAsTermFromDefinition(selectedCandidateTerm, candidateTermsTable, ontologyTermsTable, definitionTable);
			guiComponent.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}

		private void organizeDefinition(DefinitionContainer def, List<CandidateDefinition> defList, int index)
		{
			if (def != null) {
				boolean duplicateDefinition = false;

				String defStr = def.getDefinition();
				if (defStr.endsWith(" ...")) {
					defStr = defStr.substring(0, defStr.length() - 4);
				}

				for (int i = 0; i < defList.size(); i++) {
					CandidateDefinition candDef = defList.get(i);

					String candDefStr = candDef.getDefinition();
					if (candDefStr.endsWith(" ...")) {
						candDefStr = candDefStr.substring(0, candDefStr.length() - 4);
					}

					if (defStr.equals(candDefStr)) {
						candDef.addURL(def.getUrl());
						candDef.addCachedURL(def.getCachedURL());
						duplicateDefinition = true;
					}
					else if (candDefStr.contains(defStr) || defStr.contains(candDefStr)) {
						duplicateDefinition = true;

						boolean duplicateAlternativeDefinition = false;
						if (candDef.getAlternativeDefinitions() != null) {
							for (CandidateDefinition definition : candDef.getAlternativeDefinitions()) {
								// Try to find identical alternative
								// definition.
								if (definition.getDefinition().equals(def.getDefinition())) {
									duplicateAlternativeDefinition = true;

									definition.addURL(def.getUrl());
									definition.addCachedURL(def.getCachedURL());
								}
							}
						}

						// If no identical alternative definition is
						// found,
						// add a new alternative definition.
						if (!duplicateAlternativeDefinition) {
							final CandidateDefinition candidateDefinition = new CandidateDefinition(index, def.getDefinition(), def.getFormattedDefinition(),
									def.getUrl(), def.getCachedURL(), def.getParentTermCount(), false);

							if (def.getDefinition().length() > candDef.getDefinition().length()) {
								// swap candidateDefinition and
								// alternative Definition
								candidateDefinition.addAlternativeDefinition(candDef);
								if (candDef.getAlternativeDefinitions() != null) {
									for (CandidateDefinition candDefAltDef : candDef.getAlternativeDefinitions()) {
										candidateDefinition.addAlternativeDefinition(candDefAltDef);
									}
									candDef.resetAlternativeDefinitions();
								}
								candDef.removeListeners();
								int pos = defList.indexOf(candDef);
								defList.remove(candDef);

								defList.add(pos, candidateDefinition);

								candidateDefinition.addListener(new UpdateListenerInterface()
								{
									public void update()
									{
										updateParentAsTermFromDefinition(selectedCandidateTerm, candidateTermsTable, ontologyTermsTable, definitionTable);
									}
								});

							}
							else {
								candDef.addAlternativeDefinition(candidateDefinition);
							}
						}
					}
				}
				// Otherwise, add new definition to list.
				if (!duplicateDefinition) {
					final CandidateDefinition candidateDefinition = new CandidateDefinition(index, def.getDefinition(), def.getFormattedDefinition(), def
							.getUrl(), def.getCachedURL(), def.getParentTermCount(), false);
					index++;
					candidateDefinition.addListener(new UpdateListenerInterface()
					{
						public void update()
						{
							updateParentAsTermFromDefinition(selectedCandidateTerm, candidateTermsTable, ontologyTermsTable, definitionTable);
						}
					});
					defList.add(candidateDefinition);
				}
			}
			else {
				logger.trace("A retrieved definition was null");
			}

		}
	}

}

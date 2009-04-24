package org.oboedit.gui.components.ontologyGeneration;

import java.awt.BorderLayout;
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
import java.io.InputStream;
import java.io.InputStreamReader;
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
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.LinkedBlockingQueue;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
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
import org.bbop.framework.AbstractGUIComponent;
import org.jdesktop.swingworker.SwingWorker;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.PathCapable;
import org.obo.datamodel.impl.DbxrefImpl;
import org.obo.history.AddDbxrefHistoryItem;
import org.obo.history.AddSynonymHistoryItem;
import org.obo.history.ChangeSynScopeHistoryItem;
import org.obo.history.CreateLinkHistoryItem;
import org.obo.history.CreateObjectHistoryItem;
import org.obo.history.DefinitionChangeHistoryItem;
import org.obo.history.DelSynonymHistoryItem;
import org.obo.history.DeleteLinkHistoryItem;
import org.obo.history.DestroyObjectHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.history.NameChangeHistoryItem;
import org.obo.history.NamespaceHistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.util.IDUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.event.HistoryAppliedEvent;
import org.oboedit.gui.event.HistoryListener;
import org.oboedit.gui.event.OntologyReloadListener;
import org.oboedit.gui.event.RootChangeEvent;
import org.oboedit.gui.event.RootChangeListener;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;
import org.oboedit.util.GUIUtil;

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
import de.tud.biotec.gopubmedTermGenerationService.client.GoPubMedTermGenerationStub.TextConceptRepresentation;

/**
 * Ontology Generation Plugin for OBOEdit which supports the automatic generation of candidate terms and candidate
 * definitions to create candidate to be added to the loaded ontologies.
 * <p>
 * Developed at the Bioinformatics Group, BIOTEC, TU Dresden, Dresden, Germany
 * </p>
 * 
 * @author Atif Iqbal, 2008
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2008
 * @version 1.2, 24/04/2009
 */
public class OntologyGenerationComponent extends AbstractGUIComponent implements PropertyChangeListener
{
	public static final String PLUGIN_VERSION = "1.2";
	public static final String PLUGIN_VERSIONED_NAME = "OBO-Edit-" + Preferences.getVersion().toString()+"_"+PLUGIN_VERSION;
	private static final String SOURCE_PUBMED = "PUBMED";
	private static final String SOURCE_TEXT = "TEXT";
	private static final String SOURCE_FOLDER = "FOLDER";

	private static final Logger logger = Logger.getLogger(OntologyGenerationComponent.class);
	private static final long serialVersionUID = -8206973805283628422L;

	// private String wordInclusionPattern =
	// "^<X>[\\s|-|\\.|,|;]|[\\s|-|\\.|,|;]<X>[\\s|-|\\.|,|;]|[\\s|-|\\.|,|;]<X>$";

	// Variables
	private LinkedObject selectedLinkedObject;

	private CandidateTerm selectedCandidateTerm;

	// Tables
	private TermsTable termsTable;
	private TermsTable synonymTermsTable;
	private DefinitionsTable definitionTable;
	private OBOTermsTable oboTermsTable;

	// GUI related
	private ProgressBarDialog progressDlg;
	// private TermLabelOrSynonymAutoCompletionBox autoCompletionBox = new TermLabelOrSynonymAutoCompletionBox();

	private JButton generateTermsFromPubMedButton;
	private JButton generateTermsFromTextButton;
	private JButton generateTermsFormFolderButton;
	private JButton generateManualDefinitionButton;
	private JButton folderChooseButton;
	private JButton showClipBoardButton;
	private JButton clearClipBoardButton;
	private JButton loadClipBoardButton;
	private JButton saveClipBoardButton;

	private JButton saveDefButton;
	private JButton saveAbbrButton;
	private JButton openSplashScreenButton;
	private JButton openHelpPageButton;
	private JButton addToOntologyButton;
	private JCheckBox checkboxShowTickedParents;
	
	private JCheckBox onlyShowExistingTerms = new JCheckBox();

	private JLabel editSelectedTermLabel = new JLabel();
    private JLabel manualDefInfo = new JLabel();

	private JTextArea inputTextArea;
	private JTextArea editDefArea;

	private JTextField inputPubMedQueryField;
	private JTextField inputFolderLocationField;
	private JTextField filterTermsTextField;
	private JTextField searchTermsTextField;
	private JTextField filterDefTextField;
	private JTextField manualDefinitionField;
	private JTextField editNameTextField = new JTextField(20);
	private JTextField selectedLinkedObjectField = new JTextField(10);
	private JTextField filterPotentialParentsTextField;

	private JScrollPane scrollPane;
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

	private BiotecSplashScreen biotecSplashScreen;
	
	/*
	 * Instanciate listeners to OBOEdit
	 */
	private SessionManager sessionManager = SessionManager.getManager();

	private SelectionListener selectionListener = new SelectionListener()
	{
		public void selectionChanged(SelectionEvent e)
		{
			updateSelectedLinkedObjectAndParents();
		}
	};

	private OntologyReloadListener ontologyReloadListener = new OntologyReloadListener()
	{
		public void reload()
		{
			updateParentAsAnyExistingLinkedObject();

		}
	};

	private HistoryListener historyListener = new HistoryListener()
	{
		public void applied(HistoryAppliedEvent arg0)
		{
			Set<String> idsUndergoingChange = new HashSet<String>();
			HistoryItem historyItem = arg0.getHistoryItem();
			collectAllHistoryItems(idsUndergoingChange, historyItem);
			updateParentAsAnyExistingLinkedObject(idsUndergoingChange);
		}

		/**
		 * @param idsUndergoingChange
		 * @param historyItem
		 */
		@SuppressWarnings("unchecked")
		private void collectAllHistoryItems(Set<String> idsUndergoingChange, HistoryItem historyItem)
		{
			if (historyItem instanceof TermMacroHistoryItem) {
				TermMacroHistoryItem termMacroHistoryItem = (TermMacroHistoryItem) historyItem;
				Iterator<HistoryItem> iter = termMacroHistoryItem.getHistoryItems();
				while (iter.hasNext()) {
					HistoryItem hi = iter.next();
					String targetID = hi.getTarget();
					if (targetID != null) {
						if (hi instanceof NameChangeHistoryItem //
						                || hi instanceof CreateLinkHistoryItem //
						                || hi instanceof DeleteLinkHistoryItem //
						                || hi instanceof CreateObjectHistoryItem //
						                || hi instanceof DestroyObjectHistoryItem //
						                || hi instanceof AddSynonymHistoryItem //
						                || hi instanceof DelSynonymHistoryItem // 
						                || hi instanceof NameChangeHistoryItem //
						                || hi instanceof DelSynonymHistoryItem // 
						                || hi instanceof ChangeSynScopeHistoryItem) {
							idsUndergoingChange.add(targetID);
						}
						else if (hi instanceof TermMacroHistoryItem) {
							collectAllHistoryItems(idsUndergoingChange, hi);
						}
					}
				}
			}
		}

		public void reversed(HistoryAppliedEvent arg0)
		{
			System.err.println("REVERSED");
		}
	};

	private RootChangeListener rootChangeListener = new RootChangeListener()
	{
		public void changeRoot(RootChangeEvent arg0)
		{
			updateParentAsAnyExistingLinkedObject();
		}
	};
	private JCheckBox checkboxIncludeChildren;
	private JCheckBox checkboxIncludeBranch;
	
	private OntologyLookupServiceWorker lookupServiceWorker;
	private OntologyLookupChildrenServiceWorker lookupChildrenServiceWorker;
	
	/**
	 * Constructs a {@link OntologyGenerationComponent} instance
	 * 
	 * @param id
	 */
	public OntologyGenerationComponent(String id)
	{
		super(id);

		this.ontologyLookupQueue = new LinkedBlockingQueue<CandidateTerm>(100)
		{
			private static final long serialVersionUID = 7448640043596470248L;
		};
		this.ontologyLookupChildrenQueue = new LinkedBlockingQueue<CandidateTerm>(3)
		{
			private static final long serialVersionUID = 1L;
		};

		// TERMS TABLE
		this.termsTable = new TermsTable(clipboard, 4, true);
		this.termsTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

		this.termsTable.getColumnModel().getColumn(0).setMinWidth(50);
		this.termsTable.getColumnModel().getColumn(0).setMaxWidth(50);
		this.termsTable.getColumnModel().getColumn(0).setPreferredWidth(50);
		this.termsTable.getColumnModel().getColumn(0).setResizable(false);

		this.termsTable.getColumnModel().getColumn(2).setMinWidth(18);
		this.termsTable.getColumnModel().getColumn(2).setMaxWidth(18);
		this.termsTable.getColumnModel().getColumn(2).setPreferredWidth(18);
		this.termsTable.getColumnModel().getColumn(2).setResizable(false);

		this.termsTable.getColumnModel().getColumn(3).setMinWidth(30);
		this.termsTable.getColumnModel().getColumn(3).setMaxWidth(30);
		this.termsTable.getColumnModel().getColumn(3).setPreferredWidth(18);
		this.termsTable.getColumnModel().getColumn(3).setResizable(false);

		TableCellImageRenderer definitionGenerationImageRenderer = new TableCellImageRenderer(
		    "resources/iconDefinitionGeneration.png");
		this.termsTable.getColumnModel().getColumn(2).setCellRenderer(definitionGenerationImageRenderer);

		TableCellImageRenderer termInformationIconRenderer = new TableCellImageRenderer("resources/aboutIcon.png");
		this.termsTable.getColumnModel().getColumn(3).setCellRenderer(termInformationIconRenderer);

		// SYNONYMS TABLE
		this.synonymTermsTable = new TermsTable(clipboard, 2, false);
		this.synonymTermsTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		this.synonymTermsTable.getColumnModel().getColumn(0).setMinWidth(50);
		this.synonymTermsTable.getColumnModel().getColumn(0).setMaxWidth(50);
		this.synonymTermsTable.getColumnModel().getColumn(0).setPreferredWidth(50);
		this.synonymTermsTable.getColumnModel().getColumn(0).setResizable(false);

		// DEFINITIONS TABLE
		this.definitionTable = new DefinitionsTable();
		if (this.definitionTable.getColumnCount() > 2) {
			this.definitionTable.getColumnModel().getColumn(2).setCellRenderer(termInformationIconRenderer);
		}

		this.selectedLinkedObject = null;
		this.selectedCandidateTerm = null;

		// this.directParentTerms = new HashSet<LinkedObject>();

		this.inputPubMedQueryField = new JTextField();
		// this.inputPubMedQueryField.setSize(200, 25);
		this.inputPubMedQueryField.setMaximumSize(new Dimension(1000, 25));
		this.inputPubMedQueryField.setPreferredSize(new Dimension(100, 25));

		this.inputFolderLocationField = new JTextField();
		this.inputFolderLocationField.setMaximumSize(new Dimension(1000, 25));
		this.inputFolderLocationField.setPreferredSize(new Dimension(100, 25));
		this.inputFolderLocationField.setEditable(false);
		
		this.generateTermsFromPubMedButton = new JButton("Generate");
		this.generateTermsFromTextButton = new JButton("Generate");
		this.generateTermsFormFolderButton = new JButton("Generate");
		this.generateManualDefinitionButton = new JButton("Add Definition");
		this.folderChooseButton = new JButton("Open");
		this.showClipBoardButton = new JButton("Show");
		this.clearClipBoardButton = new JButton("Clear");
		this.loadClipBoardButton = new JButton("Load");
		this.saveClipBoardButton = new JButton("Save");

		this.saveAbbrButton = new JButton("Save Abbreviations");
		this.saveDefButton = new JButton("Save Definition");

		this.openSplashScreenButton = new JButton();
		ImageIcon icon = new ImageIcon(getClass().getResource("resources/aboutIconBigger.png"));
		this.openSplashScreenButton.setIcon(icon);
		this.openSplashScreenButton.setForeground(null);
		this.openSplashScreenButton.setBorderPainted(false);
		this.openSplashScreenButton.setBackground(null);
		this.openSplashScreenButton.setSize(new Dimension(43, 33));
		this.openSplashScreenButton.setPreferredSize(new Dimension(43, 33));
		this.openSplashScreenButton.setMaximumSize(new Dimension(43, 33));
		this.openSplashScreenButton.setMargin(new Insets(13, 0, 0, 0));

		this.openHelpPageButton = new JButton();
		ImageIcon icon2 = new ImageIcon(getClass().getResource("resources/helpIconBigger.png"));
		this.openHelpPageButton.setIcon(icon2);
		this.openHelpPageButton.setForeground(null);
		this.openHelpPageButton.setBorderPainted(false);
		this.openHelpPageButton.setBackground(null);
		this.openHelpPageButton.setSize(new Dimension(43, 33));
		this.openHelpPageButton.setPreferredSize(new Dimension(43, 33));
		this.openHelpPageButton.setMaximumSize(new Dimension(43, 33));
		this.openHelpPageButton.setMargin(new Insets(13, 0, 0, 0));
		
		this.setTitle("Ontology Generation view");
	}

	@Override
	public void init()
	{
		buildGUI();
		try {
			attachListeners();
		}
		catch (Exception exception) {
			throw new RuntimeException(exception);
		}
		validate();

		// init clipboard
		if (clipboard.getSize() > 0) {
			updateClipboard(termsTable);
		}

		// start worker and services
		ontoLookupProxy = new OntologyLookupManagerPortTypeProxy();
		lookupServiceWorker = new OntologyLookupServiceWorker();
		lookupServiceWorker.execute();
		lookupChildrenServiceWorker = new OntologyLookupChildrenServiceWorker();
		lookupChildrenServiceWorker.execute();

		// update selelected term in OBOEdit
		updateParentAsAnyExistingLinkedObject();
		updateSelectedLinkedObjectAndParents();
	}

	@Override
	public void cleanup()
	{
		removeListeners();
	}

	/**
	 * @param e {@link PropertyChangeEvent}
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
					// setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
					onClickGenerateTerms(inputPubMedQueryField);
				}
			}
		});
		
		inputFolderLocationField.addKeyListener(new KeyAdapter()
		{
			@Override
			public void keyPressed(KeyEvent event) 
			{
				if (event.getKeyCode() == 10) {
					onClickGenerateTerms(inputPubMedQueryField);
				}
			}
		});

        manualDefinitionField.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent event) {
                if(event.getKeyCode() == 10) {
                    onClickGenerateManualDefinition(manualDefinitionField);
                }
            }
        });

		generateTermsFromPubMedButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				// setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
				onClickGenerateTerms(inputPubMedQueryField);
			}
		});

		generateTermsFromTextButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				// setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
				onClickGenerateTerms(inputTextArea);
			}
		});
		
		folderChooseButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				onClickOpenFileChooser(inputFolderLocationField);
			}
		});
		
		generateTermsFormFolderButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e) {
				onClickGenerateTerms(inputFolderLocationField);
			}
			
		});
		
		generateManualDefinitionButton.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				onClickGenerateManualDefinition(manualDefinitionField);
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

		openSplashScreenButton.addActionListener(new ActionListener()
		{

			public void actionPerformed(ActionEvent e)
			{
				biotecSplashScreen = new BiotecSplashScreen(OntologyGenerationComponent.this);
				biotecSplashScreen.setVisible(true);
			}
		});

		openHelpPageButton.addActionListener(new ActionListener()
		{

			public void actionPerformed(ActionEvent e)
			{
				onClickOpenExternalHelpPage();
			}
		});

		termsTable.getSelectionModel().addListSelectionListener(new ListSelectionListener()
		{
			public void valueChanged(ListSelectionEvent event)
			{
				if (event.getValueIsAdjusting()) {
					return;
				}
				int selectedRow = termsTable.getSelectedRow();
				if (selectedRow >= 0) {
					selectedCandidateTerm = termsTable.getModel().getTermAt(selectedRow);
					logger.trace("Selected: " + selectedCandidateTerm);
					updatedAllDependedOnSelectedTerm();
				}
			}
		});

		termsTable.addMouseListener(new MouseAdapter()
		{
			@Override
			public void mouseClicked(MouseEvent e)
			{
				if (!SwingUtilities.isLeftMouseButton(e)) {
					return;
				}
				Point p = e.getPoint();
				int row = termsTable.rowAtPoint(p);
				int column = termsTable.columnAtPoint(p);
				// CandidateTerm term = termsTable.getModel().getTermAt(row);
				// if (!term.equals(selectedCandidateTerm)) {
				// // select if the selection listener was not faster
				// selectedCandidateTerm = term;
				// }
				if ((column == -1) || (row == -1)) {
					return;
				}
				else if (column == 2) {
					onClickGeneratedDefinitions();
				}
				else if (column == 3) {
					onClickOpenExternalGoPubMedPage();
				}
				// updatedAllDependedOnSelectedTerm();
			}
		});

		termsTable.addKeyListener(new KeyAdapter()
		{
			@Override
			public void keyTyped(KeyEvent e)
			{
				int row = termsTable.getSelectedRow();
				CandidateTerm term = termsTable.getModel().getTermAt(row);
				selectedCandidateTerm = term;
				if (KeyEvent.VK_SPACE == e.getKeyChar()) {
					e.consume();
					if (selectedCandidateTerm.isTicked()) {
						termsTable.getModel().removeTermFromClipboard(selectedCandidateTerm);
					}
					else {
						termsTable.getModel().addTermToClipboard(selectedCandidateTerm);
					}
					termsTable.getModel().fireTableCellUpdated(row, 0);
				}
			}
		});

		termsTable.getModel().addTableModelListener(new TableModelListener()
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
				CandidateTerm term = synonymTermsTable.getModel().getTermAt(row);
				if (KeyEvent.VK_SPACE == e.getKeyChar()) {
					e.consume();
					if (term.isTicked()) {
						synonymTermsTable.getModel().removeTermFromClipboard(term);
					}
					else {
						synonymTermsTable.getModel().addTermToClipboard(term);
					}
					synonymTermsTable.getModel().fireTableCellUpdated(row, 0);
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
		
		onlyShowExistingTerms.addActionListener(new ActionListener() {

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
				int row = definitionTable.rowAtPoint(p);
				int column = definitionTable.columnAtPoint(p);
				// The autoscroller can generate drag events outside the Table's
				// range.
				if ((column == -1) || (row == -1)) {
					return;
				}
				else if (column == 2) {
					onClickOpenDefinitionsPopup();
				}
				else if (column == 0) {
					onClickSelectDefinition(row, column);
				}
			}
		});

		definitionTable.addKeyListener(new KeyAdapter()
		{
			@Override
			public void keyTyped(KeyEvent e)
			{
				if (null == selectedCandidateTerm) {
					throw new RuntimeException("No term selected");
				}

				if (KeyEvent.VK_SPACE == e.getKeyChar()) {
					int selectedRow = definitionTable.getSelectedRow();
					CandidateDefinition candidateDefinition = selectedCandidateTerm.getGeneratedDefinitions().get(
					    selectedRow);
					if (candidateDefinition.isTicked()) {
						candidateDefinition.setTicked(false);
					}
					else {
						candidateDefinition.setTicked(true);
					}
					definitionTable.getModel().fireTableCellUpdated(selectedRow, 0);
				}
			}
		});

		oboTermsTable.addKeyListener(new KeyAdapter()
		{
			@Override
			public void keyTyped(KeyEvent e)
			{
				int row = oboTermsTable.getSelectedRow();
				LinkedObject term = oboTermsTable.getModel().getTermAt(row);
				if (KeyEvent.VK_SPACE == e.getKeyChar()) {
					e.consume();
					if (oboTermsTable.getModel().isTicked(term)) {
						oboTermsTable.getModel().setTicked(term, false);
					}
					else {
						oboTermsTable.getModel().setTicked(term, true);
					}
					oboTermsTable.getModel().fireTableCellUpdated(row, 0);
				}
			}
		});

		scrollPaneForTermsTable.getViewport().addChangeListener(new ChangeListener()
		{
			public void stateChanged(ChangeEvent e)
			{
				updateTermsTableUsingOntologyLookup(termsTable);
			}
		});

		filterTermsTextField.addCaretListener(new CaretListener()
		{
			public void caretUpdate(CaretEvent evt)
			{
				String text = filterTermsTextField.getText();
				termsTable.getModel().applyFilter(text);
			}
		});

		filterPotentialParentsTextField.addCaretListener(new CaretListener()
		{
			public void caretUpdate(CaretEvent evt)
			{
				String text = filterPotentialParentsTextField.getText();
				oboTermsTable.getModel().applyFilter(text);
			}
		});

		searchTermsTextField.addCaretListener(new CaretListener()
		{
			public void caretUpdate(CaretEvent evt)
			{
				String text = searchTermsTextField.getText();
				termsTable.findTerm(text);
			}
		});
		
		manualDefinitionField.addCaretListener(new CaretListener() {

			public void caretUpdate(CaretEvent e) {
                if(selectedCandidateTerm != null) {
                    if(manualDefinitionField.getText().equals("")) {
                        generateManualDefinitionButton.setEnabled(false);
                    }
                    else {
                        generateManualDefinitionButton.setEnabled(true);
                    }
                }
                manualDefInfo.setText("");
			}
		});
		
		filterDefTextField.addCaretListener(new CaretListener()
		{
			public void caretUpdate(CaretEvent evt)
			{
				String text = filterDefTextField.getText();
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
		
//		inputFolderLocationField.addCaretListener(new CaretListener()
//		{
//			public void caretUpdate(CaretEvent evt)
//			{
//				generateTermsFormFolderButton.setFocusable(true);
//				generateTermsFormFolderButton.setEnabled(true);
//			}
//		});
		
		

		inputTextArea.addCaretListener(new CaretListener()
		{
			public void caretUpdate(CaretEvent evt)
			{
				generateTermsFromTextButton.setFocusable(true);
				generateTermsFromTextButton.setEnabled(true);
			}
		});

		saveDefButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				if (null == selectedCandidateTerm) {
					throw new RuntimeException("No term selected");
				}
				String defText = editDefArea.getText();
				selectedCandidateTerm.setUserDefinedDefinition(defText);
				candidateTermCache.addTerm(selectedCandidateTerm);
				updateParentAsTermFromDefinition();
			}
		});

		editNameTextField.addCaretListener(new CaretListener()
		{
			public void caretUpdate(CaretEvent evt)
			{
				String labelText = editNameTextField.getText();
				if (labelText != null && labelText.trim().length() > 0) {
					selectedCandidateTerm.setUserDefinedLabel(labelText.trim());
				}
				// TODO is called 3 times on each change of selection
				updateParentAsSimiliarTerm();
			}

		});

		addToOntologyButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				boolean includeChildren = checkboxIncludeChildren.isSelected();
				boolean includeBranch = checkboxIncludeBranch.isSelected();
				addToOntologyAsChildOfLinkedObject(oboTermsTable.getModel().getTickedTerms(), includeChildren,
				    includeBranch);
			}
		});

		checkboxShowTickedParents.addItemListener(new ItemListener()
		{

			public void itemStateChanged(ItemEvent e)
			{
				if (e.getStateChange() == ItemEvent.SELECTED) {
					oboTermsTable.getModel().setShowOnlyTicked(true);
				}
				else {
					oboTermsTable.getModel().setShowOnlyTicked(false);
				}
			}
		});

		SelectionManager.getManager().addSelectionListener(selectionListener);
		SessionManager.getManager().addOntologyReloadListener(ontologyReloadListener);
		SessionManager.getManager().addHistoryListener(historyListener);
		SessionManager.getManager().addRootChangeListener(rootChangeListener);
	}

	/**
	 * Remove listeners from {@link SessionManager}
	 */
	private void removeListeners()
	{
		SelectionManager.getManager().removeSelectionListener(selectionListener);
		SessionManager.getManager().removeOntologyReloadListener(ontologyReloadListener);
		SessionManager.getManager().removeHistoryListener(historyListener);
		SessionManager.getManager().removeRootChangeListener(rootChangeListener);
	}

	/**
	 * Open a fileChooser to get a location for pdf files
	 */
	private void onClickOpenFileChooser(JTextComponent textComponent) {
		if(textComponent==null) {
			return;
		}
		
		JFileChooser fileChooser = new JFileChooser();
		
		String sfolder = textComponent.getText();
		System.out.println("sFolder: " + sfolder);
		if(!sfolder.trim().equals("")) {
			File file = new File(sfolder);
			if(file.isDirectory()) {
				fileChooser.setCurrentDirectory(file.getAbsoluteFile());
			}
			else if(file.isFile()){
				fileChooser.setCurrentDirectory(file.getParentFile());
			}
			
		}
		
		class pdfFileFilter extends FileFilter {

			@Override
			public boolean accept(File file) {
				String filename = file.getName();
				if(file.isDirectory()) {
					return true;
				}
				return filename.endsWith(".pdf");
			}

			@Override
			public String getDescription() {
				return "*.pdf";
			}
		}
		
		fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
		fileChooser.addChoosableFileFilter(new pdfFileFilter());
		
		if(fileChooser.showOpenDialog(new JFrame()) == JFileChooser.APPROVE_OPTION) {
			sfolder = fileChooser.getSelectedFile().getAbsolutePath();
			textComponent.setText(sfolder);
			
			generateTermsFormFolderButton.setEnabled(true);
			generateTermsFormFolderButton.setFocusable(true);
		}
		else {
			generateTermsFormFolderButton.setEnabled(false);
			generateTermsFormFolderButton.setFocusable(false);
		}
	}
	
	
	/**
	 * Shows the clip board in termsTable
	 */
	private void onClickShowClipBoard()
	{
		// clear first
		clearAllDependendOnSelectedTerm();
		filterTermsTextField.setText(null);
		searchTermsTextField.setText(null);
		updateClipboard(termsTable);
		// updateUI();
		updateTermsTableUsingOntologyLookup(termsTable);
	}

	/**
	 * Populate {@link TermsTable} with terms from clip board
	 * 
	 * @param pTermsTable
	 */
	private void updateClipboard(TermsTable pTermsTable)
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
		pTermsTable.setTerms(list);
		pTermsTable.getColumnModel().getColumn(1).setHeaderValue("Showing Clipboard");
		// Scroll and select
		JTableHelper.scrollToTopAndSelectFirst(pTermsTable);
	}

	/**
	 * Clears the clip board
	 */
	private void onClickClearClipBoard()
	{
		termsTable.getModel().unTickAll();

		// clear
		clearAllDependendOnSelectedTerm();
		clipBoardTabPanel.setTitleAt(0, "Clipboard " + "(" + clipboard.getSize() + ")");
		updateUI();
	}

	/**
	 * Save clip board to file
	 */
	private void onClickSaveClipBoard()
	{

		JFileChooser chooser = new JFileChooser();
		int option = chooser.showSaveDialog(OntologyGenerationComponent.this);
		if (option == JFileChooser.APPROVE_OPTION) {
			// statusbar.setText("You saved " + ((chooser.getSelectedFile()!=null)?
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
			throw new RuntimeException(exception);
		}
		finally {
			if (bufferedWriter != null) {
				try {
					bufferedWriter.close();
				}
				catch (IOException exception) {
					throw new RuntimeException(exception);
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
		int option = chooser.showOpenDialog(OntologyGenerationComponent.this);
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
			throw new RuntimeException(exception);
		}
		catch (IOException exception) {
			throw new RuntimeException(exception);
		}
		finally {
			if (bufferedReader != null) {
				try {
					bufferedReader.close();
				}
				catch (IOException exception) {
					throw new RuntimeException(exception);
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
				termsTable.getModel().addTermToClipboard(candidateTerm);
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
		String inputData = textComponent.getText().trim();
		// call to term generation thread for fetching terms from web service

		String source = null;
		if (textComponent.equals(inputPubMedQueryField)) {
			source = SOURCE_PUBMED;
			logger.debug(String.format("Generate terms for Pubmed Query '%s'", inputData));
		}
		else if (textComponent.equals(inputTextArea)) {
			source = SOURCE_TEXT;
			logger.debug(String.format("Generate terms for Text (%s kB)", inputData.length() / 1024));
		}
		else if(textComponent.equals(inputFolderLocationField)) {
			source = SOURCE_FOLDER;
			logger.debug("Generate terms for folder...");
		}

		filterTermsTextField.setText(null);
		searchTermsTextField.setText(null);

		this.progressDlg = new ProgressBarDialog(OntologyGenerationComponent.this);

		TermGenerationServiceWorker worker = new TermGenerationServiceWorker(inputData, termsTable, source);
		worker.addPropertyChangeListener(this);
		worker.execute();

		progressDlg.setVisible(true);
	}
	
	/**
	 * generated a new CandidateDefinition object 
	 * @param definitionField
	 */
	private void onClickGenerateManualDefinition(JTextField definitionField)
	{
		int index=0;
		String definition = definitionField.getText();
		String htmlDefinition;
		
		htmlDefinition = "<html>" + definitionField.getText() + "</html>";
		CandidateDefinition candidateDefinition = new CandidateDefinition(index, definition, htmlDefinition);
		
		List<CandidateDefinition> definitions = new ArrayList<CandidateDefinition>();
        definitions.add(candidateDefinition);
        definitions.addAll(selectedCandidateTerm.getGeneratedDefinitions());
        selectedCandidateTerm.setGeneratedDefinitions(definitions);
        definitionTable.getModel().setDefinitions(definitions);

        definitionField.setText("");
        manualDefInfo.setText("Definition added on top of the definition table.");
	}

	/**
	 * Launches web browser to get results from www.gopubmed.org for queryTerm and termsTable selected term.
	 */
	@SuppressWarnings("unchecked")
	private void onClickOpenExternalGoPubMedPage()
	{
		String encodedGeneratedTerm;
		String encodedqServiceTerm;

		int colNumber = OntologyGenerationComponent.this.termsTable.getSelectedColumn();
		int rowNumber = OntologyGenerationComponent.this.termsTable.getSelectedRow();

		String errMsg = "Error attempting to launch web browser";
		if (colNumber == 3 && rowNumber >= 0) {
			CandidateTerm candidateTerm = termsTable.getModel().getTermAt(rowNumber);
			String generatedTerm = putINQoutes(candidateTerm.getGeneratedLabel());
			String qServiceTerm = putINQoutes(inputPubMedQueryField.getText());
			String goURL = "http://www.gopubmed.org/search?q=";

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
					for (int count = 0; count < browsers.length && browser == null; count++)
						if (Runtime.getRuntime().exec(new String[] { "which", browsers[count] }).waitFor() == 0)
							browser = browsers[count];
					if (browser == null)
						throw new Exception("Could not find web browser");
					else
						Runtime.getRuntime().exec(new String[] { browser, url });
				}
			}
			catch (Exception e) {
				JOptionPane.showMessageDialog(null, errMsg + ":\n" + e.getLocalizedMessage());
			}

		}
	}

	/**
	 * Launches web browser to get help page.
	 */
	@SuppressWarnings("unchecked")
	private void onClickOpenExternalHelpPage()
	{
		String errMsg = "Error attempting to launch web browser";
		String url = "http://www.biotec.tu-dresden.de/schroeder/obo-edit-ontogen/";
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
				for (int count = 0; count < browsers.length && browser == null; count++)
					if (Runtime.getRuntime().exec(new String[] { "which", browsers[count] }).waitFor() == 0)
						browser = browsers[count];
				if (browser == null)
					throw new Exception("Could not find web browser");
				else
					Runtime.getRuntime().exec(new String[] { browser, url });
			}
		}
		catch (Exception e) {
			JOptionPane.showMessageDialog(null, errMsg + ":\n" + e.getLocalizedMessage());
		}

	}

	/**
	 * Instantiates DefinitionWorker thread responsible to call definitionGeneration Service
	 */
	private void onClickGeneratedDefinitions()
	{
		int rowNumber = OntologyGenerationComponent.this.termsTable.getSelectedRow();
		int colNumber = OntologyGenerationComponent.this.termsTable.getSelectedColumn();

		// For each definition query first clear the termsInDefComboBox
		if (colNumber == 2 && rowNumber >= 0) {
			// find DEFINITIONS
			// call to definition web service to fetch definitions
			String label = selectedCandidateTerm.getGeneratedLabel();
			logger.debug(String.format("Generate definitions for term '%s'", label));
			// TODO give parent terms to influence ranking
			DefinitionGenerationServiceWorker defworker = new DefinitionGenerationServiceWorker(label, null,
			    definitionTable);
			defworker.addPropertyChangeListener(this);
			defworker.execute();
			this.progressDlg = new ProgressBarDialog(OntologyGenerationComponent.this);
			progressDlg.setVisible(true);
		}
	}

	/**
	 * Picks the clicked definition, sets it selected/unselected and updates depended GUI components
	 * 
	 * @param rowIndex
	 * @param colIndex
	 */
	private void onClickSelectDefinition(int rowIndex, int colIndex)
	{
		if (null == selectedCandidateTerm) {
			throw new RuntimeException("No term selected");
		}

		if (colIndex == 0 && rowIndex >= 0) {
			CandidateDefinition definition = definitionTable.getModel().getDefinitionAt(rowIndex);
			List<CandidateDefinition> definitions = selectedCandidateTerm.getGeneratedDefinitions();

			for (CandidateDefinition candidateDefinition : definitions) {
				if (candidateDefinition.equals(definition)) {
					candidateDefinition.setTicked(definition.isTicked());
				}
			}
			StringBuffer defString = new StringBuffer();

			/*
			 * for each def in gen if def.isSelected() defString.append(def.getDefinition());
			 */
			String editedDef = selectedCandidateTerm.getUserDefinedDefinition();
			String defAreaText = editDefArea.getText();

			if (definition.isTicked()) {
				if (editedDef != null) {
					defString.append(defAreaText.trim());
					defString.append("\n");
					if (defAreaText.contains("----See Newly Generated Definition Below-----")) {
						defString.append("\n");
						defString.append(definition.getDefinition().trim());
					}
					else {
						defString.append("----See Newly Generated Definition Below-----");
						defString.append("\n");
						defString.append(definition.getDefinition().trim());

					}
				}
				else {
					if (defAreaText.length() == 0) {
						defString.append("\n");
						defString.append(definition.getDefinition().trim());
					}
					else {
						defString.append(defAreaText.trim());
						defString.append("\n");
						defString.append(definition.getDefinition().trim());
					}
				}
				editDefArea.setText(defString.toString());
			}
			else {
				editDefArea.setText(defAreaText);
			}

			// Set the label if term is selected from OBOterm Panel tree
			if (selectedLinkedObject != null) {
				editSelectedTermLabel.setText("of " + "\"" + selectedCandidateTerm.getGeneratedLabel() + " \"");
				editSelectedTermLabel.setVisible(true);
			}
		}
	}

	/**
	 * Opens a popup showing the URLs for the definition and its similar definitons.
	 * 
	 * @author Goetz Fabian
	 */
	private void onClickOpenDefinitionsPopup()
	{
		int rowNumber = OntologyGenerationComponent.this.definitionTable.getSelectedRow();
		
		DefinitionsPopup definitionsPopup = new DefinitionsPopup(OntologyGenerationComponent.this);
		definitionsPopup.initPopup(definitionTable.getModel().getDefinitionAt(rowNumber));
		
		definitionsPopup.setVisible(true);
	}

	
	/**
	 * Displays term selected in the termsTable and updates all depending gui components
	 * 
	 * @param term
	 */
	private synchronized void updatedAllDependedOnSelectedTerm()
	{
		logger.trace("UPDATE updatedAllDependedOnSelectedTerm() for :" + selectedCandidateTerm);

		if (null == selectedCandidateTerm) {
			logger.warn("Invalid selection in termsTable, term is assumed to be selected");
		}
		else {
			clearAllDependendOnSelectedTerm();
			if (candidateTermCache.hasCandidateTerm(selectedCandidateTerm)) {
				List<CandidateDefinition> generatedDefinitions = selectedCandidateTerm.getGeneratedDefinitions();
				if (null != generatedDefinitions) {
					definitionTable.removeAll();
					definitionTable.setDefinitions(generatedDefinitions);

					for (CandidateDefinition definition : generatedDefinitions) {
						if (definition.isTicked()) {
							definitionTable.getModel().selectDefinition(definition);
						}
					}
				}
				if (null != selectedCandidateTerm.getUserDefinedDefinition()) {
					editDefArea.setText(selectedCandidateTerm.getUserDefinedDefinition());
				}
			}
			definitionTable.getColumnModel().getColumn(1).setHeaderValue(
			    "Definitions for \"" + selectedCandidateTerm.getGeneratedLabel() + "\"");
			definitionTable.getTableHeader().updateUI();

			if (!editNameTextField.getText().equals(selectedCandidateTerm.getGeneratedLabel())) {
				editNameTextField.setText(selectedCandidateTerm.getGeneratedLabel());
				editNameTextField.updateUI();
			}

			// TODO check order of update operations
			updateParentAsTermFromDefinition();
			updateParentAsSimiliarTerm();
			updateSynonymOrChildTable();
		}
	}

	/**
	 * Update the table holding synonyms and children of the currently selected terms
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
					logger.trace("remove " + remove.getGeneratedLabel());
				}
				ontologyLookupChildrenQueue.offer(selectedCandidateTerm);
			}
			List<CandidateTerm> clipboardTerms = termsTable.getModel().getAllTerms();
			for (CandidateTerm candidateTerm : clipboardTerms) {
				if (termsTable.getModel().isInClipboard(candidateTerm) && !ontologyLookupChildrenQueue
				    .contains(candidateTerm)) {
					logger.trace("ADD to queue clipboard term: " + candidateTerm);
					if (ontologyLookupChildrenQueue.remainingCapacity() == 0) {
						CandidateTerm remove = ontologyLookupChildrenQueue.remove();
						logger.trace("remove " + remove.getGeneratedLabel());
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
						if (childTerm.getLabel() != null && !selectedCandidateTerm.getGeneratedLabel().equals(
						    childTerm.getLabel())) {
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
				// add term merged from eventually existing term and newly generated in clipboard, if already in
				// there.
				synonymTermsTable.getModel().addTermToClipboard(generatedTerm);
			}
		}
	}

	private void updateParentAsTermFromDefinition()
	{
		logger.trace("UPDATE TERMS FROM DEFINITION for :" + selectedCandidateTerm);

		if (null == selectedCandidateTerm) {
			int row = termsTable.rowAtPoint(termsTable.getMousePosition());
			selectedCandidateTerm = termsTable.getModel().getTermAt(row);
			logger.warn("Selection lost in terms table, recovered though mouse position");
		}
		// clear
		oboTermsTable.getModel().clearTermsFromDefinitions();
		Collection<IdentifiedObject> allIdentifiedObjects = sessionManager.getSession().getObjects();

		// process user definded definition
		SortedSet<LinkedObject> objectsFromUserDefindedDef = new TreeSet<LinkedObject>();
		for (IdentifiedObject identifiedObject : allIdentifiedObjects) {
			if (null != selectedCandidateTerm.getUserDefinedDefinition()) {
				if (contains(selectedCandidateTerm.getUserDefinedDefinition(), identifiedObject.getName())) {
					if (!objectsFromUserDefindedDef.contains(identifiedObject)) {
						if (identifiedObject instanceof LinkedObject) {
							LinkedObject linkedObject = (LinkedObject) identifiedObject;
							oboTermsTable.getModel().addFromUserDefinedDefinition(linkedObject);
							objectsFromUserDefindedDef.add(linkedObject);
						}
					}
				}
			}
		}
		// process selected definitions
		SortedSet<LinkedObject> objectsFromTickedDef = new TreeSet<LinkedObject>();
		List<CandidateDefinition> definitions = definitionTable.getModel().getDefinitions();
		for (CandidateDefinition candidateDefinition : definitions) {
			if (candidateDefinition.isTicked()) {
				for (IdentifiedObject identifiedObject : allIdentifiedObjects) {
					if (contains(candidateDefinition.getDefinition(), identifiedObject.getName())) {
						if (!objectsFromUserDefindedDef.contains(identifiedObject) && !objectsFromTickedDef
						    .contains(identifiedObject)) {
							if (identifiedObject instanceof LinkedObject) {
								LinkedObject linkedObject = (LinkedObject) identifiedObject;
								oboTermsTable.getModel().addFromTickedDefinition(linkedObject);
							}
						}
					}
				}
			}
		}
	}

	/**
	 * Add similar (based on substring inclusion) terms to similiarTermsComboBox based on substring comparison
	 */
	private void updateParentAsSimiliarTerm()
	{
		logger.trace("UPDATE SIMILAR TERMS for :" + selectedCandidateTerm);

		if (null == selectedCandidateTerm) {
			throw new RuntimeException("No term selected");
		}

		// clearing
		oboTermsTable.getModel().clearSameAsCandidateTerms();
		oboTermsTable.getModel().clearSimiliarToCandidateTerm();

		// String savedDef;
		String selectedTermLabel = selectedCandidateTerm.getGeneratedLabel();
		if (selectedTermLabel.length() != 0) {
			Collection<IdentifiedObject> allIdentifiedObjects = sessionManager.getSession().getObjects();
			for (IdentifiedObject identifiedObject : allIdentifiedObjects) {
				if (identifiedObject instanceof LinkedObject) {
					LinkedObject linkedObject = (LinkedObject) identifiedObject;
					if (linkedObject.getName().equalsIgnoreCase(selectedTermLabel)) {
						oboTermsTable.getModel().addSameAsCandidateTerm(linkedObject);
					}
					else if (contains(linkedObject.getName(), selectedTermLabel) || contains(selectedTermLabel,
					    linkedObject.getName())) {
						oboTermsTable.getModel().addSimilarToCandidateTerm(linkedObject);
					}
				}
			}
		}
	}

	// /**
	// * Adds ancestor terms of selected term to parentTermsComboBox
	// */
	// private void updateParentAsParentOfSelectedLinkedObject()
	// {
	// logger.trace("UPDATE PARENTS for :" + selectedCandidateTerm);
	// oboTermsTable.getModel().clearParentsOfSelectedLinkedObject();
	// if (null != selectedLinkedObject) {
	// String linkedObjectLabel = selectedLinkedObject.getName();
	//
	// Selection selection = SelectionManager.getGlobalSelection();
	// TreePath[] paths = selection.getPaths();
	//
	// if (paths == null || paths.length == 0) {
	// return;
	// }
	// else {
	// Collection<PathCapable> pathc = new LinkedList<PathCapable>();
	// for (TreePath path : paths) {
	// Object[] os = path.getPath();
	// for (Object o : os) {
	// if (o instanceof PathCapable) {
	// pathc.add((PathCapable) o);
	// }
	// }
	// }
	// for (LinkedObject indentifiedObject : getParentObjects(pathc)) {
	// if (!indentifiedObject.getName().equalsIgnoreCase(linkedObjectLabel)) {
	// oboTermsTable.getModel().addParentsTermsOfSelectedLinkedObject(indentifiedObject);
	// }
	// }
	// }
	// }
	// }

	/**
	 * Feed all terms known to OBOEdit into the {@link OBOTermsTable}
	 * 
	 * @param idsUndergoingChange
	 */
	private void updateParentAsAnyExistingLinkedObject()
	{
		logger.trace("UPDATE updateExistingLinkedObjects() for :" + selectedCandidateTerm);
		List<LinkedObject> linkedObjects = new ArrayList<LinkedObject>();
		for (IdentifiedObject identifiedObject : sessionManager.getCurrentLinkDatabase().getObjects()) {
			if (identifiedObject instanceof LinkedObject) {
				LinkedObject linkedObject = (LinkedObject) identifiedObject;
				linkedObjects.add(linkedObject);
			}
		}
		oboTermsTable.setTerms(linkedObjects);
	}

	/**
	 * Feed specified terms known to OBOEdit into the {@link OBOTermsTable}
	 * 
	 * @param idsUndergoingChange
	 */
	private void updateParentAsAnyExistingLinkedObject(Collection<String> idsUndergoingChange)
	{
		logger.trace(String.format("UPDATE updateExistingLinkedObjects() for: %s + (%d)", selectedCandidateTerm,
		    idsUndergoingChange.size()));
		LinkDatabase currentLinkDatabase = sessionManager.getCurrentLinkDatabase();
		List<LinkedObject> linkedObjects = new ArrayList<LinkedObject>();
		for (String id : idsUndergoingChange) {
			IdentifiedObject objects = currentLinkDatabase.getObject(id);
			if (objects instanceof LinkedObject)
				linkedObjects.add((LinkedObject) objects);
		}
		if (linkedObjects.size() > 0) {
			oboTermsTable.getModel().updateTerms(linkedObjects);
		}
	}

	private void updateTermsTableUsingOntologyLookup(TermsTable pTermsTable)
	{
		Rectangle visibleRect = pTermsTable.getVisibleRect();
		int firstVisibleRow = pTermsTable.rowAtPoint(new Point(0, visibleRect.y));
		int lastVisibleRow = pTermsTable.rowAtPoint(new Point(0, visibleRect.y + visibleRect.height - 1));
		logger.trace(String.format("UPDATE updateTermsTableUsingOntologyLookup; FEED %s %s", firstVisibleRow,
		    lastVisibleRow));
		if (firstVisibleRow >= 0 && lastVisibleRow >= 0 && (termsTable.getCurrentFirstVisibleRow() != firstVisibleRow || termsTable
		    .getCurrentLastVisibleRow() != lastVisibleRow)) {
			ontologyLookupQueue.clear();
			for (int i = firstVisibleRow; i <= lastVisibleRow; i++) {
				CandidateTerm candidateTerm = pTermsTable.getModel().getTermAt(i);
				ontologyLookupQueue.offer(candidateTerm);
			}
		}
		termsTable.setCurrentFirstVisibleRow(firstVisibleRow);
		termsTable.setCurrentLastVisibleRow(lastVisibleRow);
	}
	
	private void updateTermsTableWithExistingTerms(boolean showExistingTerms) 
	{
		termsTable.onlyShowExistingTerms(showExistingTerms);

		// Scroll and select
		JTableHelper.scrollToTopAndSelectFirst(termsTable);

		// Reset the cursor in normal mode
		setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));

		// remove existing lookup queries
		ontologyLookupQueue.clear();
		ontologyLookupChildrenQueue.clear();
		
		// reinitialize visible table rows 
		termsTable.setCurrentFirstVisibleRow(-1);
		termsTable.setCurrentLastVisibleRow(-1);
		
		// restart ontology terms lookup
		updateTermsTableUsingOntologyLookup(termsTable);

	}

	/**
	 * Checks for the selected {@link LinkedObject} and updates the GUI
	 */
	private void updateSelectedLinkedObjectAndParents()
	{
		logger.trace("UPDATE SELF updateSelectedLinkedObject() for :" + selectedCandidateTerm);
		Collection<PathCapable> paths = SelectionManager.getGlobalSelection().getAllSelectedObjects();
		oboTermsTable.getModel().clearSelectedLinkedObjects();
		oboTermsTable.getModel().clearParentsOfSelectedLinkedObject();
		Set<LinkedObject> parents = new HashSet<LinkedObject>();
		for (PathCapable pathCapable : paths) {
			if (pathCapable instanceof LinkedObject) {
				LinkedObject linkedObject = (LinkedObject) pathCapable;
				selectedLinkedObject = (LinkedObject) pathCapable;
				inputPubMedQueryField.setText(linkedObject.getName());
				String selectedLinkedObjectLabel = linkedObject.getName();
				selectedLinkedObjectField.setText(selectedLinkedObjectLabel);
				oboTermsTable.getModel().addSelectedLinkedObject(linkedObject);
				for (Link link : sessionManager.getCurrentLinkDatabase().getParents(linkedObject)) {
					parents.add(link.getParent());
				}
			}
		}
		// add parents of selected
		LinkedObject[] parentArray = new LinkedObject[parents.size()];
		parentArray = parents.toArray(parentArray);
		oboTermsTable.getModel().addParentsTermsOfSelectedLinkedObject(parentArray);
	}
	
	/**
	 * Adds the <code>definition</code>, which was selected in the 
	 * <code>DefinitionsPopup</code> to the <code>editDefArea</code>.
	 * 
	 * @param definition the definition to be attached to the <code>editDefArea</code> text.
	 * 
	 * @author Goetz Fabian
	 */
	public void updateEditDefArea(String definition)
	{
		StringBuffer defString = new StringBuffer();

		String defAreaText = editDefArea.getText();

		if (defAreaText.length() == 0) {
			defString.append("\n");
			defString.append(definition.trim());
		}
		else {
			defString.append(defAreaText.trim());
			defString.append("\n");
			defString.append(definition.trim());
		}
		editDefArea.setText(defString.toString());
	}

	/*
	 * INTERACTION WITH OBO EDIT ONTOLOGY MODEL
	 */

	private String getTypeID()
	{
		String id = JOptionPane.showInputDialog("Please input an id");
		if (id == null || id.length() == 0) {
			System.err.println("Cannot create a new type " + "without an id");
			return null;
		}
		else {
			if (SessionManager.getManager().getSession().getObject(id) != null) {
				System.err.println("ID " + id + " already in use!");
				return null;
			}
			else if (!IDUtil.isLegalID(id)) {
				System.err.println("ID " + id + " contains illegal characters");
				return null;
			}
		}
		return id;
	}

	/**
	 * Takes the LinkedObject, locates it in the Ontology Tree and adds selected term as child
	 * 
	 * @param includeBranch
	 * @param includeChildren
	 * @param selectedObject LinkedObject to locate in ontology tree
	 */
	private void addToOntologyAsChildOfLinkedObject(Set<String> parentIds, boolean includeChildren,
	    boolean includeBranch)
	{
		TermMacroHistoryItem changeItem = new TermMacroHistoryItem();
		if (parentIds == null || parentIds.size() == 0) {
			JOptionPane.showMessageDialog(null,
			    "Please select a term to add children or add root edit/Add Root/Add Root");
			return;
		}
		else {
			Iterator<String> selectedParentIdIterator = parentIds.iterator();
			String selectedCandidateTermID = null;
			List<IdentifiedObject> linkedObjectsIfExistLikeSelectedTerm = getLinkedObjectsIfExist(selectedCandidateTerm
			    .getLabel());
			if (selectedParentIdIterator.hasNext()) {
				String parentId = selectedParentIdIterator.next();
				LinkedObject parentLinkedObject = (LinkedObject) SessionManager.getManager().getCurrentLinkDatabase()
				    .getObject(parentId);
				if (linkedObjectsIfExistLikeSelectedTerm == null) {
					if (TermUtil.isProperty(parentLinkedObject))
						selectedCandidateTermID = getTypeID();
					else
						selectedCandidateTermID = GUIUtil.fetchID(parentLinkedObject);
					if (selectedCandidateTermID == null || selectedCandidateTermID.trim().length() == 0) {
						System.err.println("Could not generate ID! " + "Action cancelled.");
					}
					TermMacroHistoryItem item = createTermInOBOEdit(selectedCandidateTermID, selectedCandidateTerm,
					    parentLinkedObject);
					changeItem.addItem(item);
					TermMacroHistoryItem addTerm = addTermToOBOEdit(selectedCandidateTermID, parentId, OBOProperty.IS_A
					    .getID());
					changeItem.addItem(addTerm);

				}
				else if (linkedObjectsIfExistLikeSelectedTerm.size() == 1) {
					TermMacroHistoryItem addTerm = addTermToOBOEdit(
					    linkedObjectsIfExistLikeSelectedTerm.get(0).getID(), parentId, OBOProperty.IS_A.getID());
					changeItem.addItem(addTerm);
					selectedCandidateTermID = linkedObjectsIfExistLikeSelectedTerm.get(0).getID();
				}
				else {
					throw new RuntimeException(String.format("There exist already more than one term with name '%s'",
					    selectedCandidateTerm.getLabel()));
				}
			}
			while (selectedParentIdIterator.hasNext()) {
				TermMacroHistoryItem addTerm = addTermToOBOEdit(selectedCandidateTermID, selectedParentIdIterator
				    .next(), OBOProperty.IS_A.getID());
				changeItem.addItem(addTerm);
			}
			SessionManager.getManager().apply(changeItem, false);

			/*
			 * TODO care for multiple for the same label and create a joint term with all these ids currently seperate
			 * terms with differentIds and different labels are added
			 */
			// include known children of the added term to the ontology
			if (includeChildren && !includeBranch) {
				addAllChildrenToOBOEditForTermWithID(selectedCandidateTermID, selectedCandidateTerm);
			}
			else if (includeBranch) {
				throw new RuntimeException("feature is not implemented");
				// ArrayList<String> list = new ArrayList<String>();
				// for (OBOLookupTerm term : selectedCandidateTerm.getExistingOntologyTerms()) {
				// list.add(term.getOboID());
				// }
				// addAllDescendantsToOBOEdit(list);
			}
		}
	}

	/**
	 * Recursively get all the descendants
	 * 
	 * @param children
	 */
	private void addAllDescendantsToOBOEdit(List<String> children)
	{
		try {
			HashMap<String, String> idToName = new HashMap<String, String>();

			for (String id : children) {
				OBOLookupResult lookupResult = ontoLookupProxy.getChildren(id, 1);
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
						System.err.println(String.format("%s -%s-> %s", parent, relationString, child));
					}
				}
				addAllDescendantsToOBOEdit(children2);
			}

		}
		catch (RemoteException exception) {
			// TODO Auto-generated catch block
			throw new RuntimeException(exception);
		}

	}

	/**
	 * TODO describe me!
	 * 
	 * @param selectedCandidateTermID
	 * @return
	 */
	private void addAllChildrenToOBOEditForTermWithID(String selectedCandidateTermID, CandidateTerm candidateTerm)
	{
		TermMacroHistoryItem changeItem = new TermMacroHistoryItem();
		// setup the map of id to name mappings
		HashMap<String, String> idToName = new HashMap<String, String>();
		for (OBOLookupTerm lookupTerm : candidateTerm.getExistingChildTerms()) {
			idToName.put(lookupTerm.getOboID(), lookupTerm.getLabel());
		}
		// create all and add all known children
		for (final OBOLookupRelation relation : candidateTerm.getExistingChildRelations()) {
			final String childLabel = idToName.get(relation.getOboChildTermID());
			if (childLabel != null) {
				final List<IdentifiedObject> linkedObjectsIfExist = getLinkedObjectsIfExist(childLabel);
				final LinkedObject parentLinkedObject = (LinkedObject) SessionManager.getManager()
				    .getCurrentLinkDatabase().getObject(selectedCandidateTermID);
				if (linkedObjectsIfExist == null) {
					final String[] lexicalRepresentation = { childLabel };
					final CandidateTerm childCandidateTerm = new CandidateTerm(childLabel, new String[0],
					    lexicalRepresentation, Double.NaN, CandidateTerm.TYPE_OBO_TERM);
					final TermMacroHistoryItem createTermItem = createTermInOBOEdit(relation.getOboChildTermID(),
					    childCandidateTerm, parentLinkedObject);
					changeItem.addItem(createTermItem);
					final TermMacroHistoryItem addTermItem = addTermToOBOEdit(relation.getOboChildTermID(),
					    parentLinkedObject.getID(), relation.getOboRelationShipType());
					changeItem.addItem(addTermItem);
					// accumulate return value
				}
				else {
					for (final IdentifiedObject identifiedObject : linkedObjectsIfExist) {
						final Collection<Link> children = SessionManager.getManager().getCurrentLinkDatabase()
						    .getChildren(parentLinkedObject);
						if (!children.contains(identifiedObject)) {
							final TermMacroHistoryItem addTermItem = addTermToOBOEdit(identifiedObject.getID(),
							    parentLinkedObject.getID(), relation.getOboRelationShipType());
							changeItem.addItem(addTermItem);
							// accumulate return value
						}
					}
				}
			}
		}
		SessionManager.getManager().apply(changeItem, false);
	}

	/**
	 * Add a create new child to the OBO Ontology
	 * 
	 * @param id, the id of the newly added term
	 * @param candidateTerm, the term to add
	 * @param parentLinkedObject, the parent {@link LinkedObject} for the newly added term
	 * @return
	 */
	private TermMacroHistoryItem createTermInOBOEdit(String id, CandidateTerm candidateTerm,
	    LinkedObject parentLinkedObject)
	{
		TermMacroHistoryItem item = new TermMacroHistoryItem("Add and create child to " + parentLinkedObject);
		String label = candidateTerm.getLabel();
		item.addItem(new CreateObjectHistoryItem(id, parentLinkedObject.getType().getID()));
		item.addItem(new NameChangeHistoryItem(label, id, id));

		Namespace ns = parentLinkedObject.getNamespace();
		if (ns == null)
			ns = SessionManager.getManager().getSession().getDefaultNamespace();
		if (ns != null) {
			item.addItem(new NamespaceHistoryItem(null, ns, id));
		}

		if (candidateTerm.getUserDefinedDefinition() != null) {
			item.addItem(new DefinitionChangeHistoryItem(null, candidateTerm.getUserDefinedDefinition(), id));
			for (CandidateDefinition candidateDef : candidateTerm.getGeneratedDefinitions()) {
	            if (candidateDef.isTicked()) {
					for (String url :candidateDef.getUrl()) {
		    			item.addItem(new AddDbxrefHistoryItem(id,new DbxrefImpl("URL",url,DbxrefImpl.DEFINITION), true, null));
		            }
	            }
            }
		}

		// add synonyms
		for (String abbreviation : candidateTerm.getAbbreviations()) {
			AddSynonymHistoryItem addSynonymHistoryItem = new AddSynonymHistoryItem(id, abbreviation);
			item.addItem(addSynonymHistoryItem);
		}
		for (String lex : candidateTerm.getLexicalRepresentations()) {
			if (!lex.equals(label)) {
				item.addItem(new AddSynonymHistoryItem(id, lex));
			}
		}
		return item;
	}

	/**
	 * Returns the {@link LinkedObject}s known in OBOEdit with same label
	 * 
	 * @param label
	 * @return
	 */
	private List<IdentifiedObject> getLinkedObjectsIfExist(String label)
	{
		List<IdentifiedObject> list = null;
		Collection<IdentifiedObject> objects = SessionManager.getManager().getCurrentLinkDatabase().getObjects();
		for (IdentifiedObject identifiedObject : objects) {
			if (identifiedObject.getName().equalsIgnoreCase(label)) {
				if (list == null) {
					list = new ArrayList<IdentifiedObject>();
				}
				list.add(identifiedObject);
			}
		}
		return list;
	}

	/**
	 * Add a new child to the OBO Ontology TODO find out, if it is necessary to take care of cycle detection
	 * 
	 * @param id, the id of the newly added term
	 * @param parentLinkedObject, the parent {@link LinkedObject} for the newly added term
	 * @return
	 */
	private TermMacroHistoryItem addTermToOBOEdit(String id, String parentId, String relationType)
	{
		TermMacroHistoryItem item = new TermMacroHistoryItem("Add new child to " + parentId);
		item.addItem(new CreateLinkHistoryItem(id, relationType, parentId));
		// item.setTarget(parentId); // TODO unclear if needed
		// item.setResult(id); // TODO unclear if needed
		return item;
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
		filterDefTextField.setText(null);
		editDefArea.setText(null);
		synonymTermsTable.getModel().removeAll();
		editNameTextField.setText(null);
		filterPotentialParentsTextField.setText(null);
	}

	/*
	 * HELPERS
	 */
	private String putINQoutes(String term)
	{
		StringBuffer qBuffer = new StringBuffer();
		qBuffer.append("\"");
		qBuffer.append(term);
		qBuffer.append("\"");
		return qBuffer.toString();
	}

	/**
	 * Returns true if string contains a potential substring
	 * 
	 * @param string
	 * @param potentialSubString
	 * @return
	 */
	private boolean contains(String string, String potentialSubString)
	{
		return string.toLowerCase().contains(potentialSubString.toLowerCase());
	}
	
	/**
     * Replace invalid character  (prepare text to send with Axis)
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

	/**
	 * Lays out the GUI
	 */
	private void buildGUI()
	{
		// Set the layout of main window
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		// 
		JPanel allStuffPanel = new JPanel();
		allStuffPanel.setLayout(new BoxLayout(allStuffPanel, BoxLayout.Y_AXIS));

		//
		// 1-Term Generation panel
		//
		JPanel termGenerationPanel = new JPanel(new BorderLayout(7, 7));

		// Set the Border with Bold font face
		TitledBorder titledBorderTermGenerationPanel = new TitledBorder("1. Term Generation");
		titledBorderTermGenerationPanel.setTitleFont(new Font(titledBorderTermGenerationPanel.getTitleFont()
		    .getFontName(), Font.BOLD, 16));
		titledBorderTermGenerationPanel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
		termGenerationPanel.setBorder(titledBorderTermGenerationPanel);

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

		// add compnents
		JLabel inputPubMedLabel = new JLabel("Query PubMed: ");
		Dimension spacer = new Dimension(4, 4);
		primaryInputBoardPanelForPubMed.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPubMed.add(inputPubMedLabel);
		primaryInputBoardPanelForPubMed.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPubMed.add(inputPubMedQueryField);
		primaryInputBoardPanelForPubMed.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPubMed.add(generateTermsFromPubMedButton);
		primaryInputBoardPanelForPubMed.add(Box.createRigidArea(spacer));

		JPanel primaryInputBoardPanelForText = new JPanel();
		primaryInputBoardPanelForText.setLayout(new BoxLayout(primaryInputBoardPanelForText, BoxLayout.X_AXIS));
		// Set the Border with Bold font face
		// TitledBorder titledBorderPrimaryInputBoardPanelForText = new TitledBorder("Generated From Text");
		// titledBorderPrimaryInputBoardPanelForText.setTitleFont(new
		// Font(titledBorderTermGenerationPanel.getTitleFont().getFontName(), Font.PLAIN, 10));
		// titledBorderPrimaryInputBoardPanelForText.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));

		JLabel inputTextLabel = new JLabel("Paste Text: ");
		primaryInputBoardPanelForText.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForText.add(inputTextLabel);
		primaryInputBoardPanelForText.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForText.add(inputTextScrollPane, inputTextAreaConstraints);
		primaryInputBoardPanelForText.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForText.add(generateTermsFromTextButton);
		primaryInputBoardPanelForPubMed.add(Box.createRigidArea(spacer));

		
		// primary input panel for Folder of pdf files
		JPanel primaryInputBoardPanelForPdfFolder = new JPanel();
		primaryInputBoardPanelForPdfFolder.setLayout(new BoxLayout(primaryInputBoardPanelForPdfFolder,BoxLayout.X_AXIS));
		
		JLabel inputFolderLabel = new JLabel("Folder/File: ");
		primaryInputBoardPanelForPdfFolder.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPdfFolder.add(inputFolderLabel);
		primaryInputBoardPanelForPdfFolder.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPdfFolder.add(inputFolderLocationField);
		primaryInputBoardPanelForPdfFolder.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPdfFolder.add(folderChooseButton);
		primaryInputBoardPanelForPdfFolder.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPdfFolder.add(generateTermsFormFolderButton);
		primaryInputBoardPanelForPdfFolder.add(Box.createRigidArea(spacer));
		
		
		inputTabPanel.addTab("PubMed", primaryInputBoardPanelForPubMed);
		inputTabPanel.addTab("Text", primaryInputBoardPanelForText);
		inputTabPanel.addTab("PDF", primaryInputBoardPanelForPdfFolder);
		inputPanel.add(inputTabPanel);

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
		inputPanel.add(openHelpPageButton);
		inputPanel.add(openSplashScreenButton);

		if (inputPubMedQueryField.getText().trim().length() == 0) {
			generateTermsFromPubMedButton.setEnabled(false);
		}
		else {
			generateTermsFromPubMedButton.setEnabled(true);
		}

		if (inputTextArea.getText().trim().length() == 0) {
			generateTermsFromTextButton.setEnabled(false);
		}
		else {
			generateTermsFromTextButton.setEnabled(true);
		}
		
		if(inputFolderLocationField.getText().trim().length() == 0) {
			generateTermsFormFolderButton.setEnabled(false);
		}
		else {
			generateTermsFormFolderButton.setEnabled(true);
		}

		// Filter Term Panel to be added to InputPanel containing regular
		// expressions filtration
		JPanel filterTermPanel = new JPanel();
		filterTermPanel.setLayout(new BoxLayout(filterTermPanel, BoxLayout.Y_AXIS));
		filterTermPanel.setAlignmentX(Component.TOP_ALIGNMENT);

		JPanel filterTermUpperPanel = new JPanel();
		filterTermUpperPanel.setLayout(new BoxLayout(filterTermUpperPanel, BoxLayout.X_AXIS));
		filterTermUpperPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		JLabel searchTermLabel = new JLabel("Search:");
		filterTermUpperPanel.add(searchTermLabel);
		searchTermsTextField = new JTextField();
		searchTermsTextField.setSize(200, 25);
		searchTermsTextField.setMaximumSize(new Dimension(200, 25));
		searchTermsTextField.setPreferredSize(new Dimension(150, 25));
		filterTermUpperPanel.add(searchTermsTextField);

		JLabel filterLabel = new JLabel(" Filter:");
		filterTermUpperPanel.add(filterLabel);
		filterTermsTextField = new JTextField();
		filterTermsTextField.setSize(200, 25);
		filterTermsTextField.setMaximumSize(new Dimension(200, 25));
		filterTermsTextField.setPreferredSize(new Dimension(150, 25));
		filterTermUpperPanel.add(filterTermsTextField);
		onlyShowExistingTerms.setText("Show existing terms only.");
		filterTermUpperPanel.add(Box.createRigidArea(spacer));
		filterTermUpperPanel.add(onlyShowExistingTerms);
		JLabel filterExampleLabel = new JLabel(
	    "<html><font color=\"blue\"><b>Search or Filter Example</b>: Show those starting or ending with <b>cell</b> by typing <b>\\Acell</b> or <b>cell$</b></font></html>");
		filterTermPanel.add(filterTermUpperPanel);
		filterTermPanel.add(Box.createRigidArea(spacer));
		filterTermPanel.add(filterExampleLabel);

		// Add the subPanels to the Term Generation Panel
		termGenerationPanel.add(inputPanel, BorderLayout.NORTH);

		termsTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		termsTable.setPreferredScrollableViewportSize(new Dimension(200, 200));
		scrollPaneForTermsTable = new JScrollPane(termsTable);
		termGenerationPanel.add(scrollPaneForTermsTable, BorderLayout.CENTER);
		termGenerationPanel.add(filterTermPanel, BorderLayout.SOUTH);

		//
		// 2 Definition Generation panel
		//
		JPanel definitonGenerationPanel = new JPanel(new BorderLayout(7, 7));
		// set the Border with Bold font face
		TitledBorder titledBorderDefPanel = new TitledBorder("2. Definition Generation");
		titledBorderDefPanel.setTitleFont(new Font(titledBorderDefPanel.getTitleFont().getFontName(), Font.BOLD, 16));
		titledBorderDefPanel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
		definitonGenerationPanel.setBorder(titledBorderDefPanel);

		
		JPanel manualDefGenerationPanel = new JPanel();
		manualDefGenerationPanel.setLayout(new BoxLayout(manualDefGenerationPanel,BoxLayout.X_AXIS));
//		manualDefGenerationPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
//		manualDefGenerationPanel.setAlignmentY(Component.BOTTOM_ALIGNMENT);
		
		JLabel manualDefGenLabel = new JLabel("Manual Definition: ");
//		manualDefGenLabel.setVisible(true);

		manualDefinitionField = new JTextField();
//		manualDefinitionField.setLayout(new BoxLayout(manualDefinitionField,BoxLayout.X_AXIS));
		manualDefinitionField.setSize(200, 25);
		manualDefinitionField.setMaximumSize(new Dimension(200, 25));
		manualDefinitionField.setPreferredSize(new Dimension(200, 25));

        generateManualDefinitionButton.setEnabled(false);
        
		manualDefGenerationPanel.add(manualDefGenLabel);
		manualDefGenerationPanel.add(manualDefinitionField);
		manualDefGenerationPanel.add(generateManualDefinitionButton);
        manualDefGenerationPanel.add(manualDefInfo);
		

		//
		// 2-Definition Generation sub panels
		//
		// Filter definitions
		JPanel filterDefPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		JLabel filterDefLabel = new JLabel("Filter Definitions:");
		filterDefPanel.add(filterDefLabel);

		filterDefTextField = new JTextField();
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
		saveAbbrButton.setEnabled(false); // TODO make abbreviations editable, and enable adding
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

		definitonGenerationPanel.add(manualDefGenerationPanel, BorderLayout.NORTH);
		definitonGenerationPanel.add(new JScrollPane(definitionTable), BorderLayout.CENTER);
		definitonGenerationPanel.add(southDefPanel, BorderLayout.SOUTH);

		//
		// 3-Add child to ontology Panel and 3 sub panels
		//
		TitledBorder titledBorderAddToOntologyPanel = new TitledBorder("3. Add to Ontology");
		titledBorderAddToOntologyPanel.setTitleFont(new Font(titledBorderAddToOntologyPanel.getTitleFont()
		    .getFontName(), Font.BOLD, 16));
		titledBorderAddToOntologyPanel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));

		JPanel addToOntologyPanel = new JPanel();
		addToOntologyPanel.setLayout(new BoxLayout(addToOntologyPanel, BoxLayout.Y_AXIS));

		addToOntologyPanel.setBorder(titledBorderAddToOntologyPanel);

		JPanel candidateToAddPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		JLabel label = new JLabel("Add Term:");
		Font boldFont = new Font(label.getFont().getName(), Font.BOLD, label.getFont().getSize());
		label.setFont(boldFont);
		candidateToAddPanel.add(label);
		candidateToAddPanel.add(Box.createRigidArea(new Dimension(5, 0)));
		candidateToAddPanel.add(editNameTextField);
		checkboxIncludeChildren = new JCheckBox();
		candidateToAddPanel.add(checkboxIncludeChildren);
		JLabel label2 = new JLabel("include children");
		candidateToAddPanel.add(label2);
		checkboxIncludeBranch = new JCheckBox();
		// candidateToAddPanel.add(checkboxIncludeBranch);
		// JLabel label3 = new JLabel("include sub-branch");
		// candidateToAddPanel.add(label3);
		JPanel parentHeaderPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		JLabel parentLabel = new JLabel("Potential parent terms (existing in OBO-Edit):");
		parentLabel.setFont(boldFont);
		parentHeaderPanel.add(parentLabel);
		checkboxShowTickedParents = new JCheckBox();
		checkboxShowTickedParents.setEnabled(true);
		parentHeaderPanel.add(checkboxShowTickedParents);
		JLabel label4 = new JLabel("show ticked parent terms only");
		parentHeaderPanel.add(label4);

		JPanel potentialParentPanel = new JPanel();
		potentialParentPanel.setLayout(new BoxLayout(potentialParentPanel, BoxLayout.X_AXIS));
		this.oboTermsTable = new OBOTermsTable();
		this.oboTermsTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		this.oboTermsTable.setMinimumPreferedeScrollableViewportHeight(100);
		this.oboTermsTable.setMaximumPreferedeScrollableViewportHeight(300);
		// this.oboTermsTable.setPreferredScrollableViewportSize(new Dimension(600, 100));
		JScrollPane scrollPaneForPotentialParents = new JScrollPane(this.oboTermsTable);
		// JLabel label4 = new JLabel("Parent term:");
		// label4.setFont(boldFont);
		// potentialParentPanel.add(label4);
		potentialParentPanel.add(Box.createRigidArea(new Dimension(7, 0)));
		potentialParentPanel.add(scrollPaneForPotentialParents);
		addToOntologyButton = new JButton("<html><center>Add term<br>to<br>Ontology</center></html>");
		addToOntologyButton.setMaximumSize(new Dimension(110, 330));
		potentialParentPanel.add(Box.createRigidArea(new Dimension(7, 0)));
		addToOntologyButton.setFont(boldFont);
		potentialParentPanel.add(this.addToOntologyButton);
		potentialParentPanel.add(Box.createRigidArea(new Dimension(7, 0)));

		// Filter Term Panel to be added to InputPanel containing regular
		// expressions filtration
		JPanel filterPotentialParentTermsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		filterPotentialParentTermsPanel.add(new JLabel(" Filter:"));
		filterPotentialParentsTextField = new JTextField();
		filterPotentialParentsTextField.setSize(200, 25);
		filterPotentialParentsTextField.setMaximumSize(new Dimension(200, 25));
		filterPotentialParentsTextField.setPreferredSize(new Dimension(150, 25));
		filterPotentialParentTermsPanel.add(filterPotentialParentsTextField);
		filterPotentialParentTermsPanel
		    .add(new JLabel(
		        "<html><font color=\"blue\">&nbsp; <b>Example</b>: starts or ends with <b>cell</b> type <b>\\Acell</b> or <b>cell$</b></font></html>"));

		addToOntologyPanel.add(candidateToAddPanel, BorderLayout.NORTH);
		addToOntologyPanel.add(parentHeaderPanel, BorderLayout.NORTH);
		addToOntologyPanel.add(potentialParentPanel, BorderLayout.CENTER);
		addToOntologyPanel.add(filterPotentialParentTermsPanel, BorderLayout.SOUTH);

		// ALL PANELS
		// Add All panels to allStuffPanel and then add JscrollPane to it
		allStuffPanel.add(Box.createRigidArea(new Dimension(0, 5)));
		allStuffPanel.add(termGenerationPanel);
		allStuffPanel.add(Box.createRigidArea(new Dimension(0, 15)));
		allStuffPanel.add(definitonGenerationPanel);
		allStuffPanel.add(Box.createRigidArea(new Dimension(0, 15)));
		allStuffPanel.add(addToOntologyPanel);
		allStuffPanel.add(Box.createRigidArea(new Dimension(0, 5)));
		allStuffPanel.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 5));

		// TODO scrolling to slow, increase step size
		scrollPane = new JScrollPane(allStuffPanel);
		scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		scrollPane.setBorder(BorderFactory.createEmptyBorder(0, 5, 0, 0));
		scrollPane.getVerticalScrollBar().setUnitIncrement(10);
		add(scrollPane);
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
					// logger.trace(candidateTerm.getGeneratedLabel());
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
								for (int i = termsTable.getCurrentFirstVisibleRow(); i <= termsTable
								    .getCurrentLastVisibleRow(); i++) {
									// TODO destroys table witdh (BUG) / synchronize with resize and set terms
									termsTable.getModel().fireTableCellUpdated(i, 1);
								}
							}
						}
					}
				}
				catch (Throwable exception) {
					// proceed on error
					throw new RuntimeException(exception);
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
				logger.trace("TAKEN: " + candidateTerm.getGeneratedLabel());

				// Set the cursor in wait mode
				setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

				try {
					if (candidateTerm.getExistingChildTerms() == null) {
						final Set<String> lexicalRepresentations = candidateTerm.getLexicalRepresentations();
						String[] labels = new String[lexicalRepresentations.size()];
						labels = lexicalRepresentations.toArray(labels);
						// update existing OBO terms
						if (candidateTerm.getExistingOntologyTerms() == null) {
							logger.trace("OBO Lookup: ");
							final OBOLookupTerm[] existingOntologyTerms = ontoLookupProxy.lookupExactConcept(null,
							    labels);
							logger.trace(candidateTerm.getGeneratedLabel());
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
//									logger.trace("OBO ChildLookup: " + term.getOboID() + " " + term.getLabel());
									OBOLookupResult result = ontoLookupProxy.getChildren(term.getOboID(), 1);
//									logger.debug("OBO ChildLookup: " + term.getOboID() + " " + term.getLabel() + " DONE");
									if (result != null) {
										if (result.getTerms() != null) {
											for (OBOLookupTerm childTerm : result.getTerms()) {
												allChildrenMap.put(childTerm.getOboID(), childTerm);
											}
										}
										if (result.getRelations() != null) {
											for (OBOLookupRelation relation : result.getRelations()) {
												allRelations.add(relation);
												OBOLookupTerm childTerm = allChildrenMap.get(relation
												    .getOboChildTermID());
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
					// proceed on error
					exception.printStackTrace();
				}
				finally {
					// Reset the cursor in normal mode
					if (ontologyLookupChildrenQueue.isEmpty()) {
						setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
					}
				}
			}
		}
	}

	/**
	 * Inner Class to invoke {@link GoPubMedTermGenerationStub} in a separate worker thread
	 */
	private class TermGenerationServiceWorker extends SwingWorker<TextConceptRepresentation[], Void>
	{
		private String inputData = new String();
		private final TermsTable table;
		private final String source;

		/**
		 * Constructs a {@link TermGenerationServiceWorker}
		 * 
		 * @param inputData, the query or text uses for the generation
		 * @param destinationTable, the table where to place the terms
		 * @param textSourceName, the source type (e.g. TEXT,PUBMED)
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
					throw new RuntimeException(exception);
				}
			}

		}



		@Override
		public TextConceptRepresentation[] doInBackground()
		{
			// Reset the cursor in waite mode
			setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

			this.setProgress(0);
			TextConceptRepresentation[] concepts = new TextConceptRepresentation[1000];
			try {
				if (source.equals(SOURCE_PUBMED)) {
					GenerateConceptsFromPubMedQuery query = new GoPubMedTermGenerationStub.GenerateConceptsFromPubMedQuery();
					query.setMaxNumberOfTerms(1000);
					query.setQueryString(inputData);
					query.setApplicationCode(PLUGIN_VERSIONED_NAME);
					GenerateConceptsFromPubMedQueryResponse response = termGenerationServiceStub
					    .generateConceptsFromPubMedQuery(query);
					concepts = response.get_return();
				}
				if (source.equals(SOURCE_TEXT)) {
					String[] lines = inputData.split("\n");
					GenerateConceptsFromText query = new GoPubMedTermGenerationStub.GenerateConceptsFromText();
					query.setMaxNumberOfTerms(1000);
					query.setTexts(lines);
					query.setApplicationCode(PLUGIN_VERSIONED_NAME);
					GenerateConceptsFromTextResponse response = termGenerationServiceStub
					    .generateConceptsFromText(query);
					concepts = response.get_return();
				}
				if(source.equals(SOURCE_FOLDER)) {
					File source = new File(inputData);
					String data = "";
					
					PdfToTextExtraction pdfParser = new PdfToTextExtraction();
					if(source.isDirectory()) {
						File[] fileArray = source.listFiles();
						
						for(int i=0 ; i<fileArray.length ; i++) {
							if(fileArray[i].isFile()) {
								data += pdfParser.fileExtraction(fileArray[i]);
							}
						}
					}
					else if(source.isFile()){
						data += pdfParser.fileExtraction(source);
					}
						
							
					String[] pdfText = data.split("\n");
					GenerateConceptsFromText query = new GoPubMedTermGenerationStub.GenerateConceptsFromText();
					query.setMaxNumberOfTerms(1000);
					query.setTexts(pdfText);
					query.setApplicationCode(PLUGIN_VERSIONED_NAME);
					GenerateConceptsFromTextResponse response = termGenerationServiceStub.generateConceptsFromText(query);
					concepts = response.get_return();
				}
			}
			catch (RemoteException exception) {
				throw new RuntimeException(exception);
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
			}
			catch (java.util.concurrent.ExecutionException exception) {
				String why = null;
				Throwable cause = exception.getCause();
				if (cause != null) {
					why = cause.getMessage();
				}
				else {
					why = exception.getMessage();
				}
				logger.error("Error retrieving definitions: " + why);
			}
			if (concepts == null) {
				this.setProgress(100);
				JOptionPane
				    .showMessageDialog(null, String.format("No candidate terms found for query '%s'", inputData));
				// Reset the cursor in normal mode
				setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
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
							candidateTerm = new CandidateTerm(concept.getLabel(), concept.getKnownAbbreviation(),
							    concept.getLexicalRepresentation(), concept.getScore(), CandidateTerm.TYPE_GENERATED);
						}
						termsFromService.add(candidateTerm);
					}
				}
				this.table.setTerms(termsFromService);

				for (CandidateTerm generatedTerm : termsFromService) {
					if (table.getModel().isInClipboard(generatedTerm)) {
						// add merged existing from clipboard with generated
						table.getModel().addTermToClipboard(generatedTerm);
					}
				}
				// Scroll and select
				JTableHelper.scrollToTopAndSelectFirst(this.table);

				// Reset the cursor in normal mode
				setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));

				// finish progessbar
				this.setProgress(100);

				// trigger update ontology lookup
				updateTermsTableUsingOntologyLookup(this.table);
			}
		}

		/**
		 * Update an given candidate term with the information
		 * 
		 * @param concept
		 * @param candidateTerm
		 */
		private CandidateTerm updateCandidateTermWithConcept(TextConceptRepresentation concept,
		    CandidateTerm candidateTerm)
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
	 * Worker to invoke the {@link GoPubMedDefinitionGeneratorStub} in a separate thread
	 */
	private class DefinitionGenerationServiceWorker extends SwingWorker<DefinitionContainer[], Void>
	{
		private String qTerm = new String();
		private final String[] parents;
		private final DefinitionsTable table;

		/**
		 * Constructs a {@link DefinitionGenerationServiceWorker}
		 * 
		 * @param term, the term to be defined
		 * @param relatedTerms, known terms associated with the term (e.g. known parent terms)
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
					throw new RuntimeException(exception);
				}
			}
		}

		@Override
		public DefinitionContainer[] doInBackground()
		{
			// Reset the cursor in waite mode
			setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

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

				exception.printStackTrace();
			}
			this.setProgress(50);
			return defs;

		}

		@Override
		public void done()
		{
			List<CandidateDefinition> defList = new ArrayList<CandidateDefinition>();
			DefinitionContainer[] defs = null;
            // add manual generated definitions
            if(table.getModel().getDefinitions()!=null) {
                List<CandidateDefinition> manDefinitions = table.getModel().getDefinitions();
                for(CandidateDefinition definition : manDefinitions) {
                    defList.add(definition);
                }
            }
			try {
				defs = get();
			}
			catch (InterruptedException ignore) {
			}
			catch (ExecutionException exception) {
				String why = null;
				Throwable cause = exception.getCause();
				if (cause != null) {
					why = cause.getMessage();
				}
				else {
					why = exception.getMessage();
				}
				logger.error("Error retrieving definitions: " + why);
			}
			if (defs != null) {
				int index = 0;
				
				for (final DefinitionContainer def : defs) {
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
										// Try to find identical alternative definition.
										if (definition.getDefinition().equals(def.getDefinition())) {
											duplicateAlternativeDefinition = true;
											
											definition.addURL(def.getUrl());
											definition.addCachedURL(def.getCachedURL());
										}
									}
								}
								
								// If no identical alternative definition is found,
								// add a new alternative definition.
								if (! duplicateAlternativeDefinition) {
									final CandidateDefinition candidateDefinition = new CandidateDefinition(index, def
											.getDefinition(), def.getFormattedDefinition(), def.getUrl(), def.getCachedURL(), def
											.getParentTermCount(), false);

									if (def.getDefinition().length() > candDef.getDefinition().length()) {
										// swap candidateDefinition and alternative Definition
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
										
										candidateDefinition.addListener(new UpdateListener()
										{
											public void update()
											{
												updateParentAsTermFromDefinition();
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
							final CandidateDefinition candidateDefinition = new CandidateDefinition(index, def
							    .getDefinition(), def.getFormattedDefinition(), def.getUrl(), def.getCachedURL(), def
							    .getParentTermCount(), false);
							index++;
							candidateDefinition.addListener(new UpdateListener()
							{
								public void update()
								{
									updateParentAsTermFromDefinition();
								}
							});
							defList.add(candidateDefinition);
						}
					}
					else {
						logger.trace("A retrieved definition was null");
					}
				}
				CandidateTerm termToDefine = selectedCandidateTerm;
				if (termToDefine != null) {
					this.table.getColumnModel().getColumn(1).setHeaderValue(
					    "Definitions for \"" + termToDefine.getGeneratedLabel() + "\"");
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
				JOptionPane.showMessageDialog(null, "No definitions found for term: \"" + selectedCandidateTerm
				    .getGeneratedLabel() + "\"");
			}

			// start extension of definitions by parsing from web pages (only check the first 3 incomplete
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
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));

			this.table.repaint();
		}
	}
}
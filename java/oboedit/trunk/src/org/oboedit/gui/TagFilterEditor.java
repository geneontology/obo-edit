package org.oboedit.gui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import org.obo.filters.TagFilter;
import org.apache.log4j.Logger;



/**
 * 
 * @author Jennifer I Deegan, Nicolas Rodriguez, Tim Deegan
 * @date June 2009
 * 
 *       Creates the user interface in the 'Save as' dialog box that allows
 *       users to specify which OBO tags will be saved out into each file.
 *       Returns a collection of OBOConstants tags that will then be used to
 *       modify the collection that dictates the order in which tags will be
 *       written to the files.
 * 
 * 
 */
public class TagFilterEditor extends JPanel implements ActionListener {

	// initialize logger
	protected final static Logger logger = Logger
	.getLogger(TagFilterEditor.class);

	//protected JPanel mainPanel = new JPanel();

	/**
	 * Unselects all of the JCheckboxes. The id JCheckbox is always selected as
	 * we do not want to allow people to save out files that do not have term
	 * ids.
	 */
	protected void unSelectAllTagsToBeWritten(TagFilter tagFilter) {

		tagFilter.setIdTagToBeWritten(true);
		tagFilter.setNameTagToBeWritten(false);
		tagFilter.setIs_anonymousTagToBeWritten(false);
		tagFilter.setNamespaceTagToBeWritten(false);
		tagFilter.setAlt_idTagToBeWritten(false);
		tagFilter.setDefTagToBeWritten(false);
		tagFilter.setCommentTagToBeWritten(false);
		tagFilter.setSubsetTagToBeWritten(false);
		tagFilter.setSynonymTagToBeWritten(false);
		// exact_synonym
		// narrow_synonym
		// broad_synonym
		tagFilter.setXrefTagToBeWritten(false);
		tagFilter.setIs_obsoleteTagToBeWritten(false);
		tagFilter.setReplaced_byTagToBeWritten(false);
		tagFilter.setConsiderTagToBeWritten(false);
		tagFilter.setInstanceOfTagToBeWritten(false);
		tagFilter.setPropertyValueTagToBeWritten(false);
		tagFilter.setDomainTagToBeWritten(false);
		tagFilter.setRangeTagToBeWritten(false);
		tagFilter.setAlwaysImpliesInverseTagToBeWritten(false);
		tagFilter.setIsCyclicTagToBeWritten(false);
		tagFilter.setIsReflexiveTagToBeWritten(false);
		tagFilter.setIsSymmetricTagToBeWritten(false);
		tagFilter.setIsTransitiveTagToBeWritten(false);
		tagFilter.setTransitiveOverTagToBeWritten(false);
		tagFilter.setHoldsOverChainTagToBeWritten(false);
		tagFilter.setLinkTagToBeWritten(false);
		tagFilter.setValueLinkTagToBeWritten(false);
		tagFilter.setUnrecognizedTagToBeWritten(false);
		tagFilter.setCreatedByTagToBeWritten(false);
		tagFilter.setCreationDateTagToBeWritten(false);
		tagFilter.setModifiedByTagToBeWritten(false);
		tagFilter.setModificationDateTagToBeWritten(false);
		tagFilter.setIsMetadataTagToBeWritten(false);
		tagFilter.setSelectAllButtonSelectsAll(true);
		tagFilter.setSelectAllButtonString("Select all");
	}

	/**
	 * Shows what to do with tagsToWrite if a new filter has been created
	 * (detected by finding the variable tagsToWrite to be null).
	 * 
	 * @param tagsToWrite
	 * @return
	 */
	public void setTagsToWriteInGUI(TagFilter tagFilterFromDisc) {

		if (tagFilterFromDisc == null) {
			TagFilter tagFilter = new TagFilter();
			/* Null means start from scratch */
			layoutGUI(tagFilter);
			return;

		} else {
//			logger.debug("TagFilterEditor: setTagsToWrite: tagsToWrite = "
//					+ tagFilterFromDisc.getTagsToWrite());
			idCheckBox.setSelected(tagFilterFromDisc.isIdTagToBeWritten());
			nameCheckBox.setSelected(tagFilterFromDisc.isNameTagToBeWritten());
			is_anonymousCheckBox.setSelected(tagFilterFromDisc.isIs_anonymousTagToBeWritten());
			namespaceCheckBox.setSelected(tagFilterFromDisc.isNamespaceTagToBeWritten());
			alt_idCheckBox.setSelected(tagFilterFromDisc.isAlt_idTagToBeWritten());
			defCheckBox.setSelected(tagFilterFromDisc.isDefTagToBeWritten());
			commentCheckBox.setSelected(tagFilterFromDisc.isCommentTagToBeWritten());
			subsetCheckBox.setSelected(tagFilterFromDisc.isSubsetTagToBeWritten());
			synonymCheckBox.setSelected(tagFilterFromDisc.isSynonymTagToBeWritten());
			xrefCheckBox.setSelected(tagFilterFromDisc.isXrefTagToBeWritten());
			is_obsoleteCheckBox.setSelected(tagFilterFromDisc.isIs_obsoleteTagToBeWritten());
			replaced_byCheckBox.setSelected(tagFilterFromDisc.isReplaced_byTagToBeWritten());
			considerCheckBox.setSelected(tagFilterFromDisc.isConsiderTagToBeWritten());
			instance_ofCheckBox.setSelected(tagFilterFromDisc.isInstanceOfTagToBeWritten());
			property_valueCheckBox.setSelected(tagFilterFromDisc.isPropertyValueTagToBeWritten());
			domainCheckBox.setSelected(tagFilterFromDisc.isDomainTagToBeWritten());
			rangeCheckBox.setSelected(tagFilterFromDisc.isRangeTagToBeWritten());
			always_implies_inverseCheckBox.setSelected(tagFilterFromDisc.isAlwaysImpliesInverseTagToBeWritten());
			is_cyclicCheckBox.setSelected(tagFilterFromDisc.isIsReflexiveTagToBeWritten());
			is_reflexiveCheckBox.setSelected(tagFilterFromDisc.isIsReflexiveTagToBeWritten());
			is_symmetricCheckBox.setSelected(tagFilterFromDisc.isIsSymmetricTagToBeWritten());
			is_transitiveCheckBox.setSelected(tagFilterFromDisc.isIsTransitiveTagToBeWritten());
			transitive_overCheckBox.setSelected(tagFilterFromDisc.isTransitiveOverTagToBeWritten());
			holds_over_chainCheckBox.setSelected(tagFilterFromDisc.isHoldsOverChainTagToBeWritten());
			linkCheckBox.setSelected(tagFilterFromDisc.isLinkTagToBeWritten());
			value_linkCheckBox.setSelected(tagFilterFromDisc.isValueLinkTagToBeWritten());
			unrecognizedCheckBox.setSelected(tagFilterFromDisc.isUnrecognizedTagToBeWritten());
			created_byCheckBox.setSelected(tagFilterFromDisc.isCreatedByTagToBeWritten());
			creation_dateCheckBox.setSelected(tagFilterFromDisc.isCreationDateTagToBeWritten());
			modified_byCheckBox.setSelected(tagFilterFromDisc.isModifiedByTagToBeWritten());
			modification_dateCheckBox.setSelected(tagFilterFromDisc.isModificationDateTagToBeWritten());
			is_metadataCheckBox.setSelected(tagFilterFromDisc.isIsMetadataTagToBeWritten());
			
		}
		return;

	}

	/**
	 * Selects all of the tags to be written out in response to pressing the
	 * "select all" button.
	 * 
	 * @return
	 */
	protected TagFilter selectAllTagsToBeWritten(TagFilter tagFilter) {

		tagFilter.setIdTagToBeWritten(true);
		tagFilter.setNameTagToBeWritten(true);
		tagFilter.setIs_anonymousTagToBeWritten(true);
		tagFilter.setNamespaceTagToBeWritten(true);
		tagFilter.setAlt_idTagToBeWritten(true);
		tagFilter.setDefTagToBeWritten(true);
		tagFilter.setCommentTagToBeWritten(true);
		tagFilter.setSubsetTagToBeWritten(true);
		tagFilter.setSynonymTagToBeWritten(true);
		// exact_synonym
		// narrow_synonym
		// broad_synonym
		tagFilter.setXrefTagToBeWritten(true);
		tagFilter.setIs_obsoleteTagToBeWritten(true);
		tagFilter.setReplaced_byTagToBeWritten(true);
		tagFilter.setInstanceOfTagToBeWritten(true);
		tagFilter.setPropertyValueTagToBeWritten(true);
		tagFilter.setDomainTagToBeWritten(true);
		tagFilter.setRangeTagToBeWritten(true);
		tagFilter.setAlwaysImpliesInverseTagToBeWritten(true);
		tagFilter.setIsCyclicTagToBeWritten(true);
		tagFilter.setIsReflexiveTagToBeWritten(true);
		tagFilter.setIsSymmetricTagToBeWritten(true);
		tagFilter.setIsTransitiveTagToBeWritten(true);
		tagFilter.setTransitiveOverTagToBeWritten(true);
		tagFilter.setHoldsOverChainTagToBeWritten(true);
		tagFilter.setLinkTagToBeWritten(true);
		tagFilter.setValueLinkTagToBeWritten(true);
		tagFilter.setUnrecognizedTagToBeWritten(true);
		tagFilter.setCreatedByTagToBeWritten(true);
		tagFilter.setCreationDateTagToBeWritten(true);
		tagFilter.setModifiedByTagToBeWritten(true);
		tagFilter.setModificationDateTagToBeWritten(true);
		tagFilter.setIsMetadataTagToBeWritten(true);
		tagFilter.setSelectAllButtonSelectsAll(false);
		tagFilter.setSelectAllButtonString("Unselect all");

		return tagFilter;
	}

	/**
	 * <p>
	 * Boolean value that shows what effect will result from clicking the button
	 * that alternates between saying 'select all' and 'unselect all'.
	 * </p>
	 * <p>
	 * If selectAllButtonSelectsAll is false then clicking the button will
	 * unselect all of the checkboxes.
	 * </p>
	 * <p>
	 * If selectAllButtonSelectsAll is true then clicking the button will select
	 * all of the checkboxes.
	 * </p>
	 */
	boolean selectAllButtonSelectsAll = false;

	JCheckBox idCheckBox = new JCheckBox("id");
	JCheckBox nameCheckBox = new JCheckBox("name");
	JCheckBox is_anonymousCheckBox = new JCheckBox("is_anonymous");
	JCheckBox namespaceCheckBox = new JCheckBox("namespace");
	JCheckBox alt_idCheckBox = new JCheckBox("alt_id");
	JCheckBox defCheckBox = new JCheckBox("definition");
	JCheckBox commentCheckBox = new JCheckBox("comment");
	JCheckBox subsetCheckBox = new JCheckBox("subset");
	JCheckBox synonymCheckBox = new JCheckBox("synonym");
	// JCheckBox exact_synonymCheckBox = new JCheckBox("exact_synonym");
	// JCheckBox narrow_synonymCheckBox = new JCheckBox("narrow_synonym");
	// JCheckBox broad_synonymCheckBox = new JCheckBox("broad_synonym");
	JCheckBox xrefCheckBox = new JCheckBox("xref");
	JCheckBox is_obsoleteCheckBox = new JCheckBox("is_obsolete");
	JCheckBox replaced_byCheckBox = new JCheckBox("replaced_by");
	JCheckBox considerCheckBox = new JCheckBox("consider");
	JCheckBox instance_ofCheckBox = new JCheckBox("instance_of");
	JCheckBox property_valueCheckBox = new JCheckBox("property_value");
	JCheckBox domainCheckBox = new JCheckBox("domain");
	JCheckBox rangeCheckBox = new JCheckBox("range");
	JCheckBox always_implies_inverseCheckBox = new JCheckBox("always_implies_inverse");
	JCheckBox is_cyclicCheckBox = new JCheckBox("is_cyclic");
	JCheckBox is_reflexiveCheckBox = new JCheckBox("is_reflexive");
	JCheckBox is_symmetricCheckBox = new JCheckBox("is_symmetric");
	JCheckBox is_transitiveCheckBox = new JCheckBox("is_transitive");
	JCheckBox transitive_overCheckBox = new JCheckBox("transitive_over");
	JCheckBox holds_over_chainCheckBox = new JCheckBox("holds_over_chain");
	JCheckBox linkCheckBox = new JCheckBox("link");
	JCheckBox value_linkCheckBox = new JCheckBox("value_link");
	JCheckBox unrecognizedCheckBox = new JCheckBox("unrecognized");
	JCheckBox created_byCheckBox = new JCheckBox("created_by");
	JCheckBox creation_dateCheckBox = new JCheckBox("creation_date");
	JCheckBox modified_byCheckBox = new JCheckBox("modified_by");
	JCheckBox modification_dateCheckBox = new JCheckBox("modification_date");
	JCheckBox is_metadataCheckBox = new JCheckBox("is_metadata");
	JButton selectAllButton = new JButton("Select all");

	/**
	 * Triggers the setup of the GUI via layoutGUI().
	 */
	public TagFilterEditor() {
		TagFilter localTagFilter = new TagFilter();
		selectAllButton.addActionListener(this);
		layoutGUI(localTagFilter);
	}

	
	/**
	 * Checks value of boolean selectAllButtonSelectsAll and then either selects
	 * or unselects all checkboxes accordingly. Then resets the boolean so that
	 * the next call will have the opposite effect.
	 */
	public void actionPerformed(ActionEvent e) {

		TagFilter localTagFilter = getTagsToWriteFromGUI();

		if (selectAllButtonSelectsAll == true) {
			selectAllTagsToBeWritten(localTagFilter);
			layoutGUI(localTagFilter);
			selectAllButtonSelectsAll = false;
		} else {
			unSelectAllTagsToBeWritten(localTagFilter);
			layoutGUI(localTagFilter);
			selectAllButtonSelectsAll = true;
		}
		
	}
	
	/**
	 * Sets up the GUI for the Tag Filter Editor.
	 * 
	 * @param tagFilterEditor
	 * 
	 */
	protected void layoutGUI(TagFilter tagFilter) {
		
	
		setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();

		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 0.5;
		c.weighty = 0.5;
		c.gridx = 0;
		c.gridy = 0;
		add(idCheckBox, c);
		idCheckBox.setEnabled(false);
		idCheckBox.setSelected(tagFilter.isIdTagToBeWritten());

		c.gridy = 1;
		add(nameCheckBox, c);
		nameCheckBox.setSelected(tagFilter.isNameTagToBeWritten());

		c.gridy = 2;
		add(is_anonymousCheckBox, c);
		is_anonymousCheckBox.setSelected(tagFilter
				.isIs_anonymousTagToBeWritten());

		c.gridy = 3;
		add(alt_idCheckBox, c);
		alt_idCheckBox.setSelected(tagFilter.isAlt_idTagToBeWritten());

		c.gridy = 4;
		add(linkCheckBox, c);
		linkCheckBox.setToolTipText("Tags that start 'relationship:' or 'is_a:'.");
		linkCheckBox.setSelected(tagFilter.isLinkTagToBeWritten());

		c.gridx = 1;
		c.gridy = 0;
		add(defCheckBox, c);
		defCheckBox.setSelected(tagFilter.isDefTagToBeWritten());

		c.gridy = 1;
		add(commentCheckBox, c);
		commentCheckBox.setSelected(tagFilter.isCommentTagToBeWritten());

		c.gridy = 2;
		add(subsetCheckBox, c);
		subsetCheckBox.setSelected(tagFilter.isSubsetTagToBeWritten());

		c.gridy = 3;
		add(synonymCheckBox, c);
		synonymCheckBox.setSelected(tagFilter.isSynonymTagToBeWritten());

		c.gridy = 4;
		add(value_linkCheckBox, c);
		value_linkCheckBox.setSelected(tagFilter.isValueLinkTagToBeWritten());

		c.gridx = 2;
		c.gridy = 0;
		add(is_obsoleteCheckBox, c);
		is_obsoleteCheckBox
		.setSelected(tagFilter.isIs_obsoleteTagToBeWritten());

		c.gridy = 1;
		add(xrefCheckBox, c);
		xrefCheckBox.setSelected(tagFilter.isXrefTagToBeWritten());

		c.gridy = 2;
		add(replaced_byCheckBox, c);
		replaced_byCheckBox
		.setSelected(tagFilter.isReplaced_byTagToBeWritten());

		c.gridy = 3;
		add(considerCheckBox, c);
		considerCheckBox.setSelected(tagFilter.isCommentTagToBeWritten());

		c.gridy = 4;
		add(unrecognizedCheckBox, c);
		unrecognizedCheckBox.setSelected(tagFilter
				.isUnrecognizedTagToBeWritten());

		c.gridx = 3;
		c.gridy = 0;
		add(namespaceCheckBox, c);
		namespaceCheckBox.setSelected(tagFilter.isNamespaceTagToBeWritten());

		c.gridy = 1;
		add(instance_ofCheckBox, c);
		instance_ofCheckBox.setSelected(tagFilter.isInstanceOfTagToBeWritten());

		c.gridy = 2;
		add(property_valueCheckBox, c);
		property_valueCheckBox.setSelected(tagFilter
				.isPropertyValueTagToBeWritten());

		c.gridy = 3;
		add(domainCheckBox, c);
		domainCheckBox.setSelected(tagFilter.isDomainTagToBeWritten());

		c.gridy = 4;
		add(created_byCheckBox, c);
		created_byCheckBox.setSelected(tagFilter.isCreatedByTagToBeWritten());

		c.gridx = 4;
		c.gridy = 0;
		add(rangeCheckBox, c);
		rangeCheckBox.setSelected(tagFilter.isRangeTagToBeWritten());

		c.gridy = 1;
		add(always_implies_inverseCheckBox, c);
		always_implies_inverseCheckBox.setSelected(tagFilter
				.isAlwaysImpliesInverseTagToBeWritten());

		c.gridy = 2;
		add(is_cyclicCheckBox, c);
		is_cyclicCheckBox.setSelected(tagFilter.isIsCyclicTagToBeWritten());

		c.gridy = 3;
		add(is_reflexiveCheckBox, c);
		is_reflexiveCheckBox.setSelected(tagFilter
				.isIsReflexiveTagToBeWritten());

		c.gridy = 4;
		add(creation_dateCheckBox, c);
		creation_dateCheckBox.setSelected(tagFilter
				.isCreationDateTagToBeWritten());

		c.gridx = 5;
		c.gridy = 0;
		add(is_symmetricCheckBox, c);
		is_symmetricCheckBox.setSelected(tagFilter
				.isIsSymmetricTagToBeWritten());

		c.gridy = 1;
		add(is_transitiveCheckBox, c);
		is_transitiveCheckBox.setSelected(tagFilter
				.isIsTransitiveTagToBeWritten());

		c.gridy = 2;
		add(transitive_overCheckBox, c);
		transitive_overCheckBox.setSelected(tagFilter
				.isTransitiveOverTagToBeWritten());

		c.gridy = 3;
		add(holds_over_chainCheckBox, c);
		holds_over_chainCheckBox.setSelected(tagFilter
				.isHoldsOverChainTagToBeWritten());

		c.gridy = 4;
		add(modified_byCheckBox, c);
		modified_byCheckBox.setSelected(tagFilter.isModifiedByTagToBeWritten());

		c.gridx = 8;
		c.gridy = 0;
		add(modification_dateCheckBox, c);
		modification_dateCheckBox.setSelected(tagFilter
				.isModificationDateTagToBeWritten());

		c.gridy = 1;
		add(is_metadataCheckBox, c);
		is_metadataCheckBox.setSelected(tagFilter.isIsMetadataTagToBeWritten());

		c.gridy = 2;
		add(selectAllButton, c);
		selectAllButton.setText(tagFilter.getSelectAllButtonString());

		validate();
		repaint();
	}

	// The parts of the code that are commented out below are like this because
	// the tags in question are documented in the OBO 1.2 spec,
	// but the OBOConstant tag type is not instantiated in OBOConstants.java.
	/**
	 * <p>
	 * Gets the set of tags to be printed to the file by looking at which
	 * checkboxes are checked.
	 * </p>
	 * <p>
	 * 
	 * @return
	 */
	public TagFilter getTagsToWriteFromGUI() {

		TagFilter localTagFilter = new TagFilter(false);

		if (idCheckBox.isSelected()) {
			localTagFilter.setIdTagToBeWritten(true);
		}
		if (nameCheckBox.isSelected()) {
			localTagFilter.setNameTagToBeWritten(true);
		}
		if (is_anonymousCheckBox.isSelected()) {
			localTagFilter.setIs_anonymousTagToBeWritten(true);
		}
		if (namespaceCheckBox.isSelected()) {
			localTagFilter.setNamespaceTagToBeWritten(true);
		}
		if (alt_idCheckBox.isSelected()) {
			localTagFilter.setAlt_idTagToBeWritten(true);
		}
		if (defCheckBox.isSelected()) {
			localTagFilter.setDefTagToBeWritten(true);
		}
		if (commentCheckBox.isSelected()) {
			localTagFilter.setCommentTagToBeWritten(true);
		}
		if (subsetCheckBox.isSelected()) {
			localTagFilter.setSubsetTagToBeWritten(true);
		}
		if (synonymCheckBox.isSelected()) {
			localTagFilter.setSynonymTagToBeWritten(true);
		}
		if (xrefCheckBox.isSelected()) {
			localTagFilter.setXrefTagToBeWritten(true);
		}
		if (is_obsoleteCheckBox.isSelected()) {
			localTagFilter.setIs_obsoleteTagToBeWritten(true);
		}
		if (replaced_byCheckBox.isSelected()) {
			localTagFilter.setReplaced_byTagToBeWritten(true);
		}
		if (considerCheckBox.isSelected()) {
			localTagFilter.setConsiderTagToBeWritten(true);
		}
		if (instance_ofCheckBox.isSelected()) {
			localTagFilter.setInstanceOfTagToBeWritten(true);
		}	
		if (property_valueCheckBox.isSelected()) {
			localTagFilter.setPropertyValueTagToBeWritten(true);
		}	
		if (domainCheckBox.isSelected()) {
			localTagFilter.setDomainTagToBeWritten(true);
		}	
		if (rangeCheckBox.isSelected()) {
			localTagFilter.setRangeTagToBeWritten(true);
		}	
		if (always_implies_inverseCheckBox.isSelected()) {
			localTagFilter.setAlwaysImpliesInverseTagToBeWritten(true);
		}
		if (is_cyclicCheckBox.isSelected()) {
			localTagFilter.setIsCyclicTagToBeWritten(true);
		}	
		if (is_reflexiveCheckBox.isSelected()) {
			localTagFilter.setIsReflexiveTagToBeWritten(true);
		}	
		if (is_symmetricCheckBox.isSelected()) {
			localTagFilter.setIsSymmetricTagToBeWritten(true);
		}	
		if (is_transitiveCheckBox.isSelected()) {
			localTagFilter.setIsTransitiveTagToBeWritten(true);
		}	
		if (transitive_overCheckBox.isSelected()) {
			localTagFilter.setTransitiveOverTagToBeWritten(true);
		}	
		if (holds_over_chainCheckBox.isSelected()) {
			localTagFilter.setHoldsOverChainTagToBeWritten(true);
		}	
		if (linkCheckBox.isSelected()) {
			localTagFilter.setLinkTagToBeWritten(true);
		}	
		if (value_linkCheckBox.isSelected()) {
			localTagFilter.setValueLinkTagToBeWritten(true);
		}	
		if (unrecognizedCheckBox.isSelected()) {
			localTagFilter.setUnrecognizedTagToBeWritten(true);
		}	
		if (created_byCheckBox.isSelected()) {
			localTagFilter.setCreatedByTagToBeWritten(true);
		}	
		if (creation_dateCheckBox.isSelected()) {
			localTagFilter.setCreationDateTagToBeWritten(true);
		}	
		if (modified_byCheckBox.isSelected()) {
			localTagFilter.setModifiedByTagToBeWritten(true);
		}	
		if (modification_dateCheckBox.isSelected()) {
			localTagFilter.setModificationDateTagToBeWritten(true);
		}	
		if (is_metadataCheckBox.isSelected()) {
			localTagFilter.setIsMetadataTagToBeWritten(true);
		}	
		return localTagFilter;

	}

}

package org.obo.filters;

import java.io.Serializable;
import java.util.HashSet;

import org.obo.dataadapter.OBOConstants;

/**
 * 
 * An object capturing information about which tags will be written to the OBO file
 * using the 'save as' dialogue. 
 * 
 * @author Jennifer I Deegan
 *
 */
public class TagFilter implements Serializable {

	/**
	 * Creates a new TagFilter with all tags marked to be written to the file. This is the default state 
	 * for file writing.
	 */
	public TagFilter() {
		this.setDoTagFilter(true);
		this.setIdTagToBeWritten(true);
		this.setNameTagToBeWritten(true);
		this.setIs_anonymousTagToBeWritten(true);
		this.setNamespaceTagToBeWritten(true);
		this.setAlt_idTagToBeWritten(true);
		this.setDefTagToBeWritten(true);
		this.setCommentTagToBeWritten(true);
		this.setSubsetTagToBeWritten(true);
		this.setSynonymTagToBeWritten(true);
		//		exact_synonym
		//		narrow_synonym
		//		broad_synonym
		this.setXrefTagToBeWritten(true);
		this.setIs_obsoleteTagToBeWritten(true);
		this.setReplaced_byTagToBeWritten(true);
		this.setConsiderTagToBeWritten(true);
		this.setInstanceOfTagToBeWritten(true);
		this.setPropertyValueTagToBeWritten(true);
		this.setDomainTagToBeWritten(true);
		this.setRangeTagToBeWritten(true);
		this.setAlwaysImpliesInverseTagToBeWritten(true);
		this.setIsCyclicTagToBeWritten(true);
		this.setIsReflexiveTagToBeWritten(true);
		this.setIsReflexiveTagToBeWritten(true);
		this.setIsSymmetricTagToBeWritten(true);
		this.setIsTransitiveTagToBeWritten(true);
		this.setTransitiveOverTagToBeWritten(true);
		this.setHoldsOverChainTagToBeWritten(true);
		this.setLinkTagToBeWritten(true);
		this.setValueLinkTagToBeWritten(true);
		this.setUnrecognizedTagToBeWritten(true);
		this.setCreatedByTagToBeWritten(true);
		this.setCreationDateTagToBeWritten(true);
		this.setModifiedByTagToBeWritten(true);
		this.setModificationDateTagToBeWritten(true);
		this.setIsMetadataTagToBeWritten(true);
		
		
	}

	/**
	 * Creates a new TagFilter object. <br>
	 * If 'true' is passed then all tags will be written.<br>
	 * If 'false' is passed then only the id tag will be written. 
	 * 
	 * @param allSelectedTagFilter is a boolean to indicate whether the 
	 * full set of tags should be written to file.
	 */

	public TagFilter(boolean allSelectedTagFilter) {
		this.setDoTagFilter(allSelectedTagFilter);
		this.setIdTagToBeWritten(true);
		this.setNameTagToBeWritten(allSelectedTagFilter);
		this.setIs_anonymousTagToBeWritten(allSelectedTagFilter);
		this.setNamespaceTagToBeWritten(allSelectedTagFilter);
		this.setAlt_idTagToBeWritten(allSelectedTagFilter);
		this.setDefTagToBeWritten(allSelectedTagFilter);
		this.setCommentTagToBeWritten(allSelectedTagFilter);
		this.setSubsetTagToBeWritten(allSelectedTagFilter);
		this.setSynonymTagToBeWritten(allSelectedTagFilter);
		//		exact_synonym
		//		narrow_synonym
		//		broad_synonym
		this.setXrefTagToBeWritten(allSelectedTagFilter);
		this.setIs_obsoleteTagToBeWritten(allSelectedTagFilter);
		this.setReplaced_byTagToBeWritten(allSelectedTagFilter);
		this.setConsiderTagToBeWritten(allSelectedTagFilter);
		this.setInstanceOfTagToBeWritten(allSelectedTagFilter);
		this.setPropertyValueTagToBeWritten(allSelectedTagFilter);
		this.setDomainTagToBeWritten(allSelectedTagFilter);
		this.setRangeTagToBeWritten(allSelectedTagFilter);
		this.setAlwaysImpliesInverseTagToBeWritten(allSelectedTagFilter);
		this.setIsCyclicTagToBeWritten(allSelectedTagFilter);
		this.setIsReflexiveTagToBeWritten(allSelectedTagFilter);
		this.setIsReflexiveTagToBeWritten(allSelectedTagFilter);
		this.setIsSymmetricTagToBeWritten(allSelectedTagFilter);
		this.setIsTransitiveTagToBeWritten(allSelectedTagFilter);
		this.setTransitiveOverTagToBeWritten(allSelectedTagFilter);
		this.setHoldsOverChainTagToBeWritten(allSelectedTagFilter);
		this.setLinkTagToBeWritten(allSelectedTagFilter);
		this.setValueLinkTagToBeWritten(allSelectedTagFilter);
		this.setUnrecognizedTagToBeWritten(allSelectedTagFilter);
		this.setCreatedByTagToBeWritten(allSelectedTagFilter);
		this.setCreationDateTagToBeWritten(allSelectedTagFilter);
		this.setModifiedByTagToBeWritten(allSelectedTagFilter);
		this.setModificationDateTagToBeWritten(allSelectedTagFilter);
		this.setIsMetadataTagToBeWritten(allSelectedTagFilter);
	
	}



	private static final long serialVersionUID = 1L;

	HashSet<OBOConstants.TagMapping> tagsToWrite = new HashSet<OBOConstants.TagMapping>();

	boolean doTagFilter;

	boolean idTagToBeWritten;

	boolean nameTagToBeWritten;

	boolean is_anonymousTagToBeWritten;

	boolean namespaceTagToBeWritten;

	boolean alt_idTagToBeWritten;

	boolean defTagToBeWritten;

	boolean commentTagToBeWritten;

	boolean subsetTagToBeWritten;

	boolean synonymTagToBeWritten;

	boolean xrefTagToBeWritten;

	boolean is_obsoleteTagToBeWritten;

	boolean replaced_byTagToBeWritten;

	boolean considerTagToBeWritten;

	boolean instanceOfTagToBeWritten;

	boolean propertyValueTagToBeWritten;

	boolean domainTagToBeWritten;

	boolean rangeTagToBeWritten;

	boolean alwaysImpliesInverseTagToBeWritten;

	boolean isIsCyclicTagToBeWritten;

	boolean isIsReflexiveTagToBeWritten;

	boolean isIsSymmetricTagToBeWritten;

	boolean isIsTransitiveTagToBeWritten;

	boolean isTransitiveOverTagToBeWritten;

	boolean isHoldsOverChainTagToBeWritten;

	boolean isLinkTagToBeWritten;

	boolean IsValueLinkTagToBeWritten;

	boolean isUnrecognizedTagToBeWritten;

	boolean isCreatedByTagToBeWritten;

	boolean isCreationDateTagToBeWritten;

	boolean isModifiedByTagToBeWritten;

	boolean isModificationDateTagToBeWritten;

	boolean isIsMetadataTagToBeWritten;	

	boolean selectAllTagsToBeWritten;

	boolean selectAllButtonSelectsAll = false;

	String selectAllButtonString = "Unselect All";

	public boolean getSelectAllButtonSelectsAll() {
		return selectAllButtonSelectsAll;
	}

	public String getSelectAllButtonString() {
		return selectAllButtonString;
	}

	public HashSet<OBOConstants.TagMapping> getTagsToWrite() {
		HashSet<OBOConstants.TagMapping> localHashSet = new HashSet<OBOConstants.TagMapping>();

		if (isIdTagToBeWritten()) {
			localHashSet.add(OBOConstants.ID_TAG);
		}
		if (isNameTagToBeWritten()) {
			localHashSet.add(OBOConstants.NAME_TAG);
		}
		if (isIs_anonymousTagToBeWritten()) {
			localHashSet.add(OBOConstants.IS_ANONYMOUS_TAG);
		}
		if (isNamespaceTagToBeWritten()) {
			localHashSet.add(OBOConstants.NAMESPACE_TAG);
		}
		if (isAlt_idTagToBeWritten()) {
			localHashSet.add(OBOConstants.ALT_ID_TAG);
		}
		if (isDefTagToBeWritten()) {
			localHashSet.add(OBOConstants.DEF_TAG);
		}
		if (isCommentTagToBeWritten()) {
			localHashSet.add(OBOConstants.COMMENT_TAG);
		}
		if (isSubsetTagToBeWritten()) {
			localHashSet.add(OBOConstants.SUBSET_TAG);	
		}
		if (isSynonymTagToBeWritten()) {
			localHashSet.add(OBOConstants.RELATED_SYNONYM_TAG);	
		}
		if (isXrefTagToBeWritten()) {
			localHashSet.add(OBOConstants.XREF_TAG);	
		}
		if (isIs_obsoleteTagToBeWritten()) {
			localHashSet.add(OBOConstants.IS_OBSOLETE_TAG);	
		}
		if (isReplaced_byTagToBeWritten()) {
			localHashSet.add(OBOConstants.REPLACED_BY_TAG);	
		}
		if (isConsiderTagToBeWritten()) {
			localHashSet.add(OBOConstants.CONSIDER_TAG);	
		}
		if (isInstanceOfTagToBeWritten()) {
			localHashSet.add(OBOConstants.INSTANCE_OF_TAG);	
		}
		if (isPropertyValueTagToBeWritten()) {
			localHashSet.add(OBOConstants.PROPERTY_VALUE_TAG);	
		}
		if (isDomainTagToBeWritten()) {
			localHashSet.add(OBOConstants.DOMAIN_TAG);	
		}
		if (isRangeTagToBeWritten()) {
			localHashSet.add(OBOConstants.RANGE_TAG);	
		}
		if (isAlwaysImpliesInverseTagToBeWritten()) {
			localHashSet.add(OBOConstants.ALWAYS_IMPLIES_INVERSE_TAG);	
		}
		if (isIsCyclicTagToBeWritten()) {
			localHashSet.add(OBOConstants.IS_CYCLIC_TAG);	
		}
		if (isIsReflexiveTagToBeWritten()) {
			localHashSet.add(OBOConstants.IS_REFLEXIVE_TAG);	
		}
		if (isIsSymmetricTagToBeWritten()) {
			localHashSet.add(OBOConstants.IS_SYMMETRIC_TAG);	
		}
		if (isIsTransitiveTagToBeWritten()) {
			localHashSet.add(OBOConstants.IS_TRANSITIVE_TAG);	
		}
		if (isTransitiveOverTagToBeWritten()) {
			localHashSet.add(OBOConstants.TRANSITIVE_OVER_TAG);	
		}
		if (isHoldsOverChainTagToBeWritten()) {
			localHashSet.add(OBOConstants.HOLDS_OVER_CHAIN_TAG);	
		}
		if (isLinkTagToBeWritten()) {
			localHashSet.add(OBOConstants.LINK_TAG);	
		}
		if (isValueLinkTagToBeWritten()) {
			localHashSet.add(OBOConstants.VALUE_LINK_TAG);	
		}
		if (isUnrecognizedTagToBeWritten()) {
			localHashSet.add(OBOConstants.UNRECOGNIZED_TAG);	
		}
		if (isCreatedByTagToBeWritten()) {
			localHashSet.add(OBOConstants.CREATED_BY_TAG);	
		}
		if (isCreationDateTagToBeWritten()) {
			localHashSet.add(OBOConstants.CREATION_DATE_TAG);	
		}
		if (isModifiedByTagToBeWritten()) {
			localHashSet.add(OBOConstants.MODIFIED_BY_TAG);	
		}
		if (isModificationDateTagToBeWritten()) {
			localHashSet.add(OBOConstants.MODIFICATION_DATE_TAG);	
		}
		if (isIsMetadataTagToBeWritten()) {
			localHashSet.add(OBOConstants.IS_METADATA_TAG);	
		}
		return localHashSet;
	}

	//Getters

	public boolean isAlt_idTagToBeWritten() {
		return alt_idTagToBeWritten;
	}

	public boolean isNamespaceTagToBeWritten() {
		return namespaceTagToBeWritten;
	}

	public boolean isCommentTagToBeWritten() {
		return commentTagToBeWritten;
	}

	public boolean isConsiderTagToBeWritten() {
		return considerTagToBeWritten;
	}

	public boolean isDefTagToBeWritten() {
		return defTagToBeWritten;
	}

	public boolean isDoTagFilter() {
		return doTagFilter;
	}

	public boolean isIdTagToBeWritten() {
		return idTagToBeWritten;
	}

	public boolean isIs_anonymousTagToBeWritten() {
		return is_anonymousTagToBeWritten;
	}

	public boolean isIs_obsoleteTagToBeWritten() {
		return is_obsoleteTagToBeWritten;
	}

	public boolean isNameTagToBeWritten() {
		return nameTagToBeWritten;
	}

	public boolean isReplaced_byTagToBeWritten() {
		return replaced_byTagToBeWritten;
	}

	public boolean isSelectAllTagsToBeWritten() {
		return selectAllTagsToBeWritten;
	}

	public boolean isSubsetTagToBeWritten() {
		return subsetTagToBeWritten;
	}

	public boolean isSynonymTagToBeWritten() {
		return synonymTagToBeWritten;
	}

	public boolean isXrefTagToBeWritten() {
		return xrefTagToBeWritten;
	}

	public boolean isInstanceOfTagToBeWritten() {
		return instanceOfTagToBeWritten;
	}

	public boolean isPropertyValueTagToBeWritten() {
		return propertyValueTagToBeWritten;
	}

	public boolean isDomainTagToBeWritten() {
		return domainTagToBeWritten;
	}

	public boolean isRangeTagToBeWritten() {
		return rangeTagToBeWritten;
	}

	public boolean isAlwaysImpliesInverseTagToBeWritten() {
		return alwaysImpliesInverseTagToBeWritten;
	}

	public boolean isIsCyclicTagToBeWritten() {
		return isIsCyclicTagToBeWritten;
	}

	public boolean isIsReflexiveTagToBeWritten() {
		return isIsReflexiveTagToBeWritten;
	}

	public boolean isIsSymmetricTagToBeWritten() {
		return isIsSymmetricTagToBeWritten;
	}

	public boolean isIsTransitiveTagToBeWritten() {
		return isIsTransitiveTagToBeWritten;
	}

	public boolean isTransitiveOverTagToBeWritten() {
		return isTransitiveOverTagToBeWritten;
	}

	public boolean isHoldsOverChainTagToBeWritten() {
		return isHoldsOverChainTagToBeWritten;
	}

	public boolean isLinkTagToBeWritten() {
		return isLinkTagToBeWritten;
	}

	public boolean isValueLinkTagToBeWritten() {
		return IsValueLinkTagToBeWritten;
	}

	public boolean isUnrecognizedTagToBeWritten() {
		return isUnrecognizedTagToBeWritten;
	}

	public boolean isCreatedByTagToBeWritten() {
		return isCreatedByTagToBeWritten;
	}

	public boolean isCreationDateTagToBeWritten() {
		return isCreationDateTagToBeWritten;
	}

	public boolean isModifiedByTagToBeWritten() {
		return isModifiedByTagToBeWritten;
	}

	public boolean isModificationDateTagToBeWritten() {
		return isModificationDateTagToBeWritten;
	}

	public boolean isIsMetadataTagToBeWritten() {
		return isIsMetadataTagToBeWritten;
	}


	//Setters

	public void setAlt_idTagToBeWritten(boolean alt_idTagToBeWritten) {
		this.alt_idTagToBeWritten = alt_idTagToBeWritten;
	}
	public void setCommentTagToBeWritten(boolean commentTagToBeWritten) {
		this.commentTagToBeWritten = commentTagToBeWritten;
	}
	public void setConsiderTagToBeWritten(boolean considerTagToBeWritten) {
		this.considerTagToBeWritten = considerTagToBeWritten;
	}
	public void setDefTagToBeWritten(boolean defTagToBeWritten) {
		this.defTagToBeWritten = defTagToBeWritten;
	}
	public void setDoTagFilter(boolean doTagFilter) {
		this.doTagFilter = doTagFilter;
	}
	public void setIdTagToBeWritten(boolean idTagSelected) {
		this.idTagToBeWritten = idTagSelected;
	}
	public void setIs_anonymousTagToBeWritten(boolean is_anonymousTagToBeWritten) {
		this.is_anonymousTagToBeWritten = is_anonymousTagToBeWritten;
	}
	public void setNamespaceTagToBeWritten(boolean namespaceTagToBeWritten) {
		this.namespaceTagToBeWritten = namespaceTagToBeWritten;
	}
	public void setIs_obsoleteTagToBeWritten(boolean is_obsoleteTagToBeWritten) {
		this.is_obsoleteTagToBeWritten = is_obsoleteTagToBeWritten;
	}
	public void setNameTagToBeWritten(boolean nameTagToBeWritten) {
		this.nameTagToBeWritten = nameTagToBeWritten;
	}
	public void setReplaced_byTagToBeWritten(boolean replaced_byTagToBeWritten) {
		this.replaced_byTagToBeWritten = replaced_byTagToBeWritten;
	}
	public void setSelectAllButtonSelectsAll(boolean selectAllButtonSelectsAll) {
		this.selectAllButtonSelectsAll = selectAllButtonSelectsAll;
	}
	public void setSelectAllButtonString(String selectAllButtonString) {
		this.selectAllButtonString = selectAllButtonString;
	}

	public void setSelectAllTagsToBeWritten(boolean selectAllTagsToBeWritten) {
		this.selectAllTagsToBeWritten = selectAllTagsToBeWritten;
	}

	public void setSubsetTagToBeWritten(boolean subsetTagToBeWritten) {
		this.subsetTagToBeWritten = subsetTagToBeWritten;
	}

	public void setSynonymTagToBeWritten(boolean synonymTagToBeWritten) {
		this.synonymTagToBeWritten = synonymTagToBeWritten;
	}

	public void setTagsToWrite(HashSet<OBOConstants.TagMapping> tagsToWrite) {
		this.tagsToWrite = tagsToWrite;
	}

	public void setXrefTagToBeWritten(boolean xrefTagToBeWritten) {
		this.xrefTagToBeWritten = xrefTagToBeWritten;
	}

	public void setInstanceOfTagToBeWritten(boolean instanceOfTagToBeWritten) {
		this.instanceOfTagToBeWritten = instanceOfTagToBeWritten;
	}

	public void setPropertyValueTagToBeWritten(boolean propertyValueTagToBeWritten) {
		this.propertyValueTagToBeWritten = propertyValueTagToBeWritten;
	}

	public void setDomainTagToBeWritten(boolean domainTagToBeWritten) {
		this.domainTagToBeWritten = domainTagToBeWritten;
	}

	public void setRangeTagToBeWritten(boolean rangeTagToBeWritten) {
		this.rangeTagToBeWritten = rangeTagToBeWritten;
	}

	public void setAlwaysImpliesInverseTagToBeWritten(
			boolean alwaysImpliesInverseTagToBeWritten) {
		this.alwaysImpliesInverseTagToBeWritten = alwaysImpliesInverseTagToBeWritten;
	}

	public void setIsCyclicTagToBeWritten(boolean isIsCyclicTagToBeWritten) {
		this.isIsCyclicTagToBeWritten = isIsCyclicTagToBeWritten;
	}

	public void setIsReflexiveTagToBeWritten(boolean isIsReflexiveTagToBeWritten) {
		this.isIsReflexiveTagToBeWritten = isIsReflexiveTagToBeWritten;
	}

	public void setIsSymmetricTagToBeWritten(boolean isIsSymmetricTagToBeWritten) {
		this.isIsSymmetricTagToBeWritten = isIsSymmetricTagToBeWritten;
	}

	public void setIsTransitiveTagToBeWritten(boolean isIsTransitiveTagToBeWritten) {
		this.isIsTransitiveTagToBeWritten = isIsTransitiveTagToBeWritten;
	}

	public void setTransitiveOverTagToBeWritten(
			boolean isTransitiveOverTagToBeWritten) {
		this.isTransitiveOverTagToBeWritten = isTransitiveOverTagToBeWritten;
	}

	public void setHoldsOverChainTagToBeWritten(boolean isHoldsOverChainTagToBeWritten) {
		this.isHoldsOverChainTagToBeWritten = isHoldsOverChainTagToBeWritten;
	}

	public void setLinkTagToBeWritten(boolean isLinkTagToBeWritten) {
		this.isLinkTagToBeWritten = isLinkTagToBeWritten;
	}

	public void setValueLinkTagToBeWritten(boolean isValueLinkTagToBeWritten) {
		IsValueLinkTagToBeWritten = isValueLinkTagToBeWritten;
	}

	public void setUnrecognizedTagToBeWritten(boolean isUnrecognizedTagToBeWritten) {
		this.isUnrecognizedTagToBeWritten = isUnrecognizedTagToBeWritten;
	}

	public void setCreatedByTagToBeWritten(boolean isCreatedByTagToBeWritten) {
		this.isCreatedByTagToBeWritten = isCreatedByTagToBeWritten;
	}

	public void setCreationDateTagToBeWritten(boolean isCreationDateTagToBeWritten) {
		this.isCreationDateTagToBeWritten = isCreationDateTagToBeWritten;
	}

	public void setModifiedByTagToBeWritten(boolean isModifiedByTagToBeWritten) {
		this.isModifiedByTagToBeWritten = isModifiedByTagToBeWritten;
	}

	public void setModificationDateTagToBeWritten(
			boolean isModificationDateTagToBeWritten) {
		this.isModificationDateTagToBeWritten = isModificationDateTagToBeWritten;
	}

	public void setIsMetadataTagToBeWritten(boolean isIsMetadataTagToBeWritten) {
		this.isIsMetadataTagToBeWritten = isIsMetadataTagToBeWritten;
	}


	@Override
	public String toString() {

		StringBuffer stringToWrite = new StringBuffer();
		//stringToWrite.append("tagFilter object location is " + super.toString() + "\n");
		stringToWrite.append("id to be written = " + isIdTagToBeWritten() + "\n");
		stringToWrite.append("name to be written = " + isNameTagToBeWritten() + "\n");
		stringToWrite.append("definition to be written = " + isDefTagToBeWritten() + "\n");
		stringToWrite.append("comment to be written = " + isCommentTagToBeWritten() + "\n");
		stringToWrite.append("synonym to be written = " + isSynonymTagToBeWritten() + "\n");
		stringToWrite.append("(Not all tags listed.)");

		return stringToWrite.toString();
	}

}

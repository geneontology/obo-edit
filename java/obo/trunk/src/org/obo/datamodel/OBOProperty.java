package org.obo.datamodel;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import org.obo.datamodel.impl.OBOPropertyImpl;

/**
 * An OBOProperty is an OBOObject that is used to type {@link Relationship}s. An OBOProperty is also known as a "relation" or "relationship type".
 * The term "property" is derived from the OWL vocabulary, which includes DataProperties and ObjectProperties. In OBO Format, OBOProperties
 * are represented using [Typedef] stanzas (the term "Typedef" is somewhat confusing, but we are stuck with it. In various places in this API, the
 * OBOProperty is accessed using a type() method).
 * 
 * Examples of OBOProperties are: part_of, is_a, develops_from, regulates
 * 
 * See http://wiki.geneontology.org/index.php/Glossary
 *
 */
public interface OBOProperty extends OBOObject {

	public static final OBOProperty IS_A = new OBOPropertyImpl("OBO_REL:is_a",
			null) {
		/**
				 * 
				 */
				private static final long serialVersionUID = 1L;

		{
			this.name = "is_a";
		}

		@Override
		public boolean isTransitive() {
			return true;
		}

		@Override
		public boolean isBuiltIn() {
			return true;
		}
	};
	
	// TODO: do not hardcode relations
	public static final OBOProperty HAS_PART = new OBOPropertyImpl("OBO_REL:has_part",
			null) {
		/**
				 * 
				 */
				private static final long serialVersionUID = 1L;

		{
			this.name = "has_part";
		}

		@Override
		public boolean isTransitive() {
			return false;
		}

		@Override
		public boolean isBuiltIn() {
			return false;
		}
	};
	
	// TODO: do not hardcode relations
	public static final OBOProperty PART_OF = new OBOPropertyImpl("OBO_REL:part_of",
			null) {
		/**
				 * 
				 */
				private static final long serialVersionUID = 1L;

		{
			this.name = "part_of";
		}

		@Override
		public boolean isTransitive() {
			return false;
		}

		@Override
		public boolean isBuiltIn() {
			return false;
		}
	};
	

	public static final OBOProperty DISJOINT_FROM = new OBOPropertyImpl(
			"disjoint_from", null) {

		private static final long serialVersionUID = 3485031633547592064L;

		{
			this.name = "disjoint from";
		}

		@Override
		public boolean isSymmetric() {
			return true;
		}

		@Override
		public boolean isBuiltIn() {
			return true;
		}
		
		public boolean isNonInheritable() {
			return true;
		}
	};

	public static final OBOProperty UNION_OF = new OBOPropertyImpl("union_of",
			null) {
		/**
				 * 
				 */
				private static final long serialVersionUID = -8912464284570355892L;

		{
			this.name = "union of";
		}

		@Override
		public boolean isSymmetric() {
			return false;
		}

		@Override
		public boolean isBuiltIn() {
			return true;
		}
		
		public boolean isNonInheritable() {
			return true;
		}
	};

	public static final OBOProperty INVERSE_OF = new OBOPropertyImpl(
			"inverse_of", null) {
		/**
				 * 
				 */
				private static final long serialVersionUID = 3635477793098376143L;

		{
			this.name = "inverse of";
		}

		@Override
		public boolean isSymmetric() {
			return true;
		}

		@Override
		public boolean isBuiltIn() {
			return true;
		}
		
		public boolean isNonInheritable() {
			return true;
		}
	};
	
	public static final OBOProperty HAS_ZERO_CARDINALITY_OVER = new OBOPropertyImpl(
			"has_zero_cardinality_over", null) {
	
		{
			this.name = "has_zero_cardinality_over";
		}

		@Override
		public boolean isBuiltIn() {
			return true;
		}
		
		public boolean isNonInheritable() {
			return true;
		}
	};

	
	public static final OBOProperty TRANSITIVE_OVER = new OBOPropertyImpl(
			"transitive_over", null) {
		/**
				 * 
				 */
				private static final long serialVersionUID = 3635477793098376143L;

		{
			this.name = "transitive_over";
		}

		@Override
		public boolean isSymmetric() {
			return false;
		}

		@Override
		public boolean isBuiltIn() {
			return true;
		}
		
		public boolean isNonInheritable() {
			return true;
		}
	};
	
	public static final OBOProperty DISJOINT_OVER = new OBOPropertyImpl(
			"disjoint_over", null) {
		/**
				 * 
				 */
				private static final long serialVersionUID = 3635477793098376143L;

		{
			this.name = "transitive_over";
		}

		@Override
		public boolean isSymmetric() {
			return false;
		}

		@Override
		public boolean isBuiltIn() {
			return true;
		}
		
		public boolean isNonInheritable() {
			return true;
		}
	};
	
	public static final OBOProperty HOLDS_OVER_CHAIN = new OBOPropertyImpl(
			"holds_over_chain", null) {
		/**
				 * 
				 */
				private static final long serialVersionUID = 3635477793098376143L;

		{
			this.name = "holds_over_chain";
		}

		@Override
		public boolean isSymmetric() {
			return false;
		}

		@Override
		public boolean isBuiltIn() {
			return true;
		}
		
		public boolean isNonInheritable() {
			return true;
		}
	};

	public static final OBOProperty REPLACES = new OBOPropertyImpl(
			"replaced_by", null) {
		/**
				 * 
				 */
				private static final long serialVersionUID = -8761324497338688877L;

		{
			this.name = "replaced by";
		}

		@Override
		public boolean isSymmetric() {
			return false;
		}

		@Override
		public boolean isBuiltIn() {
			return true;
		}
		
		public boolean isNonInheritable() {
			return true;
		}
		
		@Override
		public boolean isDummy() {
			return true;
		}
	};

	public static final OBOProperty CONSIDER = new OBOPropertyImpl("consider",
			null) {
		/**
				 * 
				 */
				private static final long serialVersionUID = -5337953501398692118L;

		{
			this.name = "consider";
		}

		@Override
		public boolean isSymmetric() {
			return false;
		}

		@Override
		public boolean isBuiltIn() {
			return true;
		}
		
		public boolean isNonInheritable() {
			return true;
		}
		
		@Override
		public boolean isDummy() {
			return true;
		}
	};


	public static final OBOProperty[] BUILTIN_TYPES = { IS_A, DISJOINT_FROM,
							    UNION_OF, INVERSE_OF, TRANSITIVE_OVER, HAS_ZERO_CARDINALITY_OVER};
    // 4/11/08: Don't make these builtin
    //REGULATES, NEGATIVELY_REGULATES, POSITIVELY_REGULATES};

	public void setCyclic(boolean cyclic);

	public void setSymmetric(boolean symmetric);

	public void setTransitive(boolean transitive);
	
	public void setReflexive(boolean reflexive);
	
	public void setAlwaysImpliesInverse(boolean inverse);

	public boolean isTransitive();

	public boolean isSymmetric();

	public boolean isReflexive();
	
	public boolean isAlwaysImpliesInverse();

	public boolean isCyclic();
	
	public boolean isDummy();
	
	public boolean isMetadataTag();

	public void setMetadataTag(boolean isMetadataTag);

	public NestedValue getCyclicExtension();

	public NestedValue getSymmetricExtension();

	public NestedValue getTransitiveExtension();

	public NestedValue getReflexiveExtension();

	public NestedValue getAlwaysImpliesInverseExtension();

	public void setCyclicExtension(NestedValue nv);

	public void setAlwaysImpliesInverseExtension(NestedValue nv);
	
	public void setReflexiveExtension(NestedValue nv);

	public void setSymmetricExtension(NestedValue nv);

	public void setTransitiveExtension(NestedValue nv);

	public NestedValue getDomainExtension();

	public NestedValue getRangeExtension();

	public void setRangeExtension(NestedValue domainExtension);

	public void setDomainExtension(NestedValue domainExtension);

	public void setRange(Type range);

	public void setDomain(IdentifiedObject domain);

	public Type getRange();

	public IdentifiedObject getDomain();

	public boolean isNonInheritable();
	
	// if true then every use of this relation implies a
	// universal quantification axiom AS WELL AS the default 
	// existential quantified one
	public boolean isUniversallyQuantified();
	public void setUniversallyQuantified(boolean isUniversallyQuantified);
	
	/**
	 * R transitive_over R2 & X R Y & Y R2 Z => X R Z
	 * @return
	 */
	public OBOProperty getTransitiveOver();
	public void setTransitiveOver(OBOProperty transitiveOver);
	
	/**
	 * R disjoint_over R2 & A R B => not exists X : X R2 A & X R2 B
	 * Example: spatially_disconnected_from disjoint_over part_of
	 * @return
	 */
	public OBOProperty getDisjointOver();
	public void setDisjointOver(OBOProperty disjointOver);
	
	/**
	 * for each chain:
	 * 
	 * R holds_over_chain R1, R2, ... Rn
	 * A1 R1 A2, A2 R2 A3, ... An Rn An+1 => A1 R An+1
	 * 
	 * each relation can be associated with multiple chains
	 * @return
	 */
	public Collection<List<OBOProperty>> getHoldsOverChains();
	public void setHoldsOverChains(Collection<List<OBOProperty>> holdsOverChains);
	public void addHoldsOverChain(List<OBOProperty> holdsOverChain);
	
}

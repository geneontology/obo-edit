package org.obo.reasoner;

import java.io.Serializable;

public enum ExplanationType {
	
	GIVEN, IMPLIED_BY_EXTERNAL_REASONER,
	TRANSITIVITY, GENUS, DIFFERENTIA, INTERSECTION, SYMMETRY, REFLEXIVITY, INVERSION,
	TRANSITIVE_OVER, HOLDS_OVER_CHAIN;
}

//
//	public static final ExplanationType GIVEN = new ExplanationType() {
//
//		public String getName() {
//			return "GIVEN";
//		}
//
//		public String getDesc() {
//			return "Given by ontology";
//		}
//
//		@Override
//		public String toString() {
//			return getName();
//		}
//	};
//
//	public static final ExplanationType REDUNDANT_GIVEN = new ExplanationType() {
//
//		public String getName() {
//			return "REDUNDANT_GIVEN";
//		}
//
//		public String getDesc() {
//			return "Redundant link";
//		}
//
//		@Override
//		public String toString() {
//			return getName();
//		}
//	};
//
//	public static final ExplanationType TRANSITIVITY = new ExplanationType() {
//
//		public String getName() {
//			return "TRANSITIVITY";
//		}
//
//		public String getDesc() {
//			return "Definition of transitivity";
//		}
//
//		@Override
//		public String toString() {
//			return getName();
//		}
//	};
//
//	public static final ExplanationType GENUS = new ExplanationType() {
//		public String getName() {
//			return "GENUS";
//		}
//
//		public String getDesc() {
//			return "Definition of intersection genus";
//		}
//
//		@Override
//		public String toString() {
//			return getName();
//		}
//	};
//
//	public static final ExplanationType DIFFERENTIA = new ExplanationType() {
//		/**
//		 * 
//		 */
//		private static final long serialVersionUID = -732173211396988910L;
//
//		public String getName() {
//			return "DIFFERENTIA";
//		}
//
//		public String getDesc() {
//			return "Definition of intersection differentia";
//		}
//
//		@Override
//		public String toString() {
//			return getName();
//		}
//	};
//
//	public static final ExplanationType COMPLETENESS = new ExplanationType() {
//		/**
//		 * 
//		 */
//		private static final long serialVersionUID = -4813249561272338371L;
//
//		public String getName() {
//			return "COMPLETENESS";
//		}
//
//		public String getDesc() {
//			return "Definition of completeness";
//		}
//
//		@Override
//		public String toString() {
//			return getName();
//		}
//	};
//
//	public static final ExplanationType SYMMETRY = new ExplanationType() {
//		/**
//		 * 
//		 */
//		private static final long serialVersionUID = 157651141410289124L;
//
//		public String getName() {
//			return "SYMMETRY";
//		}
//
//		public String getDesc() {
//			return "Definition of symmetry";
//		}
//
//		@Override
//		public String toString() {
//			return getName();
//		}
//	};
//
//	public static final ExplanationType INVERSION = new ExplanationType() {
//		/**
//		 * 
//		 */
//		private static final long serialVersionUID = -1052328104657563714L;
//
//		public String getName() {
//			return "INVERSION";
//		}
//
//		public String getDesc() {
//			return "Definition of inversion";
//		}
//
//		@Override
//		public String toString() {
//			return getName();
//		}
//	};
//
//	public String getName();
//
//	public String getDesc();
//}

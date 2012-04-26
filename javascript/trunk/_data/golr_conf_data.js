////////////
////
//// bbop.golr.conf_data
////
//// Purpose: Useful information about GOlr for AmiGO.
////
//// Requirements: amigo.js for bbop.amigo namespace.
////
//// NOTE: This file is generated dynamically at installation time.
////       Hard to work with unit tests--hope it's not too bad.
////       Want to keep this real simple.
////
//////////

// All of the server/instance-specific meta-data.
bbop.golr.conf_data = {
   "bbop_ann_ev_agg" : {
      "searchable_extension" : "_searchable",
      "_infile" : "/home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/ann_ev_agg-config.yaml",
      "default_fields_and_boosts" : "foo^1.0",
      "display_name" : "Evidence Aggregate",
      "description" : "A description of annotation evidence aggregate for GOlr and AmiGO.",
      "fields" : [
         {
            "transform" : [],
            "description" : "The category of the document in the Solr index.",
            "display_name" : "Category",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "document_category",
            "property_type" : "fixed",
            "property" : "annotation_evidence_aggregate"
         },
         {
            "transform" : [],
            "description" : "Bioentity id.",
            "display_name" : "Acc",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "id",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 1 + columns 2.",
            "display_name" : "Bioentity id",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "bioentity_id",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 3.",
            "display_name" : "Bioentity label",
            "searchable" : "true",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "bioentity_label",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 5.",
            "display_name" : "Annotation class",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "annotation_class",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 5 + ontology.",
            "display_name" : "Annotation class label",
            "searchable" : "true",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "annotation_class_label",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "???",
            "display_name" : "Evidence closure",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "multi",
            "type" : "string",
            "id" : "evidence_closure",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "???",
            "display_name" : "Evidence with",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "multi",
            "type" : "string",
            "id" : "evidence_with",
            "property_type" : "dynamic",
            "property" : "???"
         }
      ],
      "weight" : "10",
      "id" : "bbop_ann_ev_agg",
      "_strict" : 0,
      "_outfile" : "/home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/ann_ev_agg-config.yaml"
   },
   "bbop_ann" : {
      "searchable_extension" : "_searchable",
      "_infile" : "/home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/ann-config.yaml",
      "default_fields_and_boosts" : "foo^1.0",
      "display_name" : "Annotations",
      "description" : "A description of annotations for GOlr and AmiGO.",
      "fields" : [
         {
            "transform" : [],
            "description" : "The category of the document in the Solr index.",
            "display_name" : "Category",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "document_category",
            "property_type" : "fixed",
            "property" : "annotation"
         },
         {
            "transform" : [],
            "description" : "A unique (and internal) combination of bioentity and ontology class.",
            "display_name" : "Acc",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "id",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 15: assigned by.",
            "display_name" : "Source",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "source",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 14: date of assignment.",
            "display_name" : "Date",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "date",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 13: taxon.",
            "display_name" : "Taxon",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "taxon",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Derived from C13 + ncbi_taxonomy.obo.",
            "display_name" : "Taxon label",
            "searchable" : "true",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "taxon_label",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Closure of ids/accs over isa and partof.",
            "display_name" : "isa/partof closure",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "multi",
            "type" : "string",
            "id" : "isa_partof_closure",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Closure of labels over isa and partof.",
            "display_name" : "isa/partof label closure",
            "searchable" : "true",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "multi",
            "type" : "string",
            "id" : "isa_partof_label_closure",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 1 + columns 2.",
            "display_name" : "Bioentity id",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "bioentity_id",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 3: bioentity label.",
            "display_name" : "Bioentity label",
            "searchable" : "true",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "bioentity_label",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 5.",
            "display_name" : "Annotation class",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "annotation_class",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 5 + ontology.",
            "display_name" : "Annotation class label",
            "searchable" : "true",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "annotation_class_label",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "???",
            "display_name" : "Evidence type",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "evidence_type",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 8: with/from.",
            "display_name" : "With",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "multi",
            "type" : "string",
            "id" : "evidence_with",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "???",
            "display_name" : "Reference",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "reference",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "???",
            "display_name" : "Annotation extension class",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "multi",
            "type" : "string",
            "id" : "annotation_extension_class",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "???",
            "display_name" : "Annotation extension class label",
            "searchable" : "true",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "multi",
            "type" : "string",
            "id" : "annotation_extension_class_label",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "???",
            "display_name" : "Annotation extension class closure",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "multi",
            "type" : "string",
            "id" : "annotation_extension_class_closure",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "???",
            "display_name" : "Annotation extension class label closure",
            "searchable" : "true",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "multi",
            "type" : "string",
            "id" : "annotation_extension_class_label_closure",
            "property_type" : "dynamic",
            "property" : "???"
         }
      ],
      "weight" : "20",
      "id" : "bbop_ann",
      "_strict" : 0,
      "_outfile" : "/home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/ann-config.yaml"
   },
   "bbop_bio" : {
      "searchable_extension" : "_searchable",
      "_infile" : "/home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/bio-config.yaml",
      "default_fields_and_boosts" : "foo^1.0",
      "display_name" : "Bioentities",
      "description" : "A description of bioentities file for GOlr.",
      "fields" : [
         {
            "transform" : [],
            "description" : "The category of the document in the Solr index.",
            "display_name" : "Category",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "document_category",
            "property_type" : "fixed",
            "property" : "bioentity"
         },
         {
            "transform" : [],
            "description" : "Bioentity id.",
            "display_name" : "Acc",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "id",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 12: type class id.",
            "display_name" : "Type class id",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "type",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 13: taxon.",
            "display_name" : "Taxon",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "taxon",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Derived from C13 + ncbi_taxonomy.obo.",
            "display_name" : "Taxon label",
            "searchable" : "true",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "taxon_label",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Closure of ids/accs over isa and partof.",
            "display_name" : "isa/partof closure",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "multi",
            "type" : "string",
            "id" : "isa_partof_closure",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Closure of labels over isa and partof.",
            "display_name" : "isa/partof label closure",
            "searchable" : "true",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "multi",
            "type" : "string",
            "id" : "isa_partof_label_closure",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Symbol or name.",
            "display_name" : "Label",
            "searchable" : "true",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "label",
            "property_type" : "dynamic",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 1: Identifier database.",
            "display_name" : "Database",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "db",
            "property_type" : "dynamic",
            "property" : "???"
         }
      ],
      "weight" : "30",
      "id" : "bbop_bio",
      "_strict" : 0,
      "_outfile" : "/home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/bio-config.yaml"
   },
   "bbop_ont" : {
      "searchable_extension" : "_searchable",
      "_infile" : "/home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/ont-config.yaml",
      "default_fields_and_boosts" : "foo^1.0",
      "display_name" : "Ontology",
      "description" : "Test mapping of ontology class for GO.",
      "fields" : [
         {
            "transform" : [],
            "description" : "The category of the document in the Solr index.",
            "display_name" : "Category",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "document_category",
            "property_type" : "fixed",
            "property" : "ontology_class"
         },
         {
            "transform" : [],
            "description" : "Term acc/id.",
            "display_name" : "Acc",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "id",
            "property_type" : "dynamic",
            "property" : "getIdentifier"
         },
         {
            "transform" : [],
            "description" : "Common term name.",
            "display_name" : "Term",
            "searchable" : "true",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "label",
            "property_type" : "dynamic",
            "property" : "getLabel"
         },
         {
            "transform" : [],
            "description" : "Term definition.",
            "display_name" : "Definition",
            "searchable" : "true",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "description",
            "property_type" : "dynamic",
            "property" : "getDef"
         },
         {
            "transform" : [],
            "description" : "Term namespace.",
            "display_name" : "Source",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "source",
            "property_type" : "dynamic",
            "property" : "getNamespace"
         },
         {
            "transform" : [],
            "description" : "Is the term obsolete?",
            "display_name" : "Obsoletion",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "boolean",
            "id" : "is_obsolete",
            "property_type" : "dynamic",
            "property" : "getIsObsoleteBinaryString"
         },
         {
            "transform" : [],
            "description" : "Term comment.",
            "display_name" : "Comment",
            "searchable" : "true",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "single",
            "type" : "string",
            "id" : "comment",
            "property_type" : "dynamic",
            "property" : "getComment"
         },
         {
            "transform" : [],
            "description" : "Term synonym.",
            "display_name" : "Synonym",
            "searchable" : "true",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "multi",
            "type" : "string",
            "id" : "synonym",
            "property_type" : "dynamic",
            "property" : "getOBOSynonymStrings"
         },
         {
            "transform" : [],
            "description" : "Alternate term id.",
            "display_name" : "Alt ID",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "multi",
            "type" : "string",
            "id" : "alternate_id",
            "property_type" : "dynamic",
            "property" : "getAltIds"
         },
         {
            "transform" : [],
            "description" : "Term subset.",
            "display_name" : "Subset",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "multi",
            "type" : "string",
            "id" : "subset",
            "property_type" : "dynamic",
            "property" : "getSubsets"
         },
         {
            "transform" : [],
            "description" : "Definition cross-reference.",
            "display_name" : "Def XRef",
            "searchable" : "false",
            "required" : "false",
            "weight" : 0,
            "cardinality" : "multi",
            "type" : "string",
            "id" : "definition_xref",
            "property_type" : "dynamic",
            "property" : "getDefXref"
         }
      ],
      "weight" : "40",
      "id" : "bbop_ont",
      "_strict" : 0,
      "_outfile" : "/home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/ont-config.yaml"
   }
};
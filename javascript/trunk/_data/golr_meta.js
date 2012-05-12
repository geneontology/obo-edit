/* 
 * Package: golr_meta.js
 * Namespace: bbop.golr.golr_meta
 * 
 * Useful information about GOlr for JS. See package golr.js
 * for API to interact with this data file.
 *
 * NOTE: Requirements: core.js for namespace controls.
 *
 * NOTE: This file is generated dynamically at installation time.
 * Hard to work with unit tests--hope it's not too bad.
 * Want to keep this real simple.
 *
 * NOTE: This file has a slightly different latout from the YAML
 * configurations files--in addition instead of the fields
 * being in lists (fields), they are in hashes keyed by the
 * field id (fields_hash).
 */

// All of the server/instance-specific meta-data.
bbop.core.require('bbop', 'core');
bbop.core.namespace('bbop', 'golr');
bbop.golr.golr_meta = {
   "bbop_ann_ev_agg" : {
      "searchable_extension" : "_searchable",
      "result_weights" : "",
      "filter_weights" : "",
      "_infile" : "/home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/ann_ev_agg-config.yaml",
      "display_name" : "Evidence Aggregate",
      "description" : "A description of annotation evidence aggregate for GOlr and AmiGO.",
      "boost_weights" : "annotation_class^2.0 annotation_class_label^1.0 bioentity_id^2.0 bioentity_label^1.0",
      "fields" : [
         {
            "transform" : [],
            "description" : "Bioentity id.",
            "display_name" : "Acc",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "id",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 1 + columns 2.",
            "display_name" : "Bioentity id",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "bioentity_id",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 3.",
            "display_name" : "Bioentity label",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "bioentity_label",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 5.",
            "display_name" : "Annotation class",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "annotation_class",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 5 + ontology.",
            "display_name" : "Annotation class label",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "annotation_class_label",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "???",
            "display_name" : "Evidence closure",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "evidence_closure",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "???",
            "display_name" : "Evidence with",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "evidence_with",
            "property" : "???"
         }
      ],
      "fields_hash" : {
         "annotation_class_label" : {
            "transform" : [],
            "description" : "Column 5 + ontology.",
            "display_name" : "Annotation class label",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "annotation_class_label",
            "property" : "???"
         },
         "bioentity_label" : {
            "transform" : [],
            "description" : "Column 3.",
            "display_name" : "Bioentity label",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "bioentity_label",
            "property" : "???"
         },
         "evidence_with" : {
            "transform" : [],
            "description" : "???",
            "display_name" : "Evidence with",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "evidence_with",
            "property" : "???"
         },
         "annotation_class" : {
            "transform" : [],
            "description" : "Column 5.",
            "display_name" : "Annotation class",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "annotation_class",
            "property" : "???"
         },
         "bioentity_id" : {
            "transform" : [],
            "description" : "Column 1 + columns 2.",
            "display_name" : "Bioentity id",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "bioentity_id",
            "property" : "???"
         },
         "id" : {
            "transform" : [],
            "description" : "Bioentity id.",
            "display_name" : "Acc",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "id",
            "property" : "???"
         },
         "evidence_closure" : {
            "transform" : [],
            "description" : "???",
            "display_name" : "Evidence closure",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "evidence_closure",
            "property" : "???"
         }
      },
      "document_category" : "annotation_evidence_aggregate",
      "weight" : "10",
      "_strict" : 0,
      "id" : "bbop_ann_ev_agg",
      "_outfile" : "/home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/ann_ev_agg-config.yaml"
   },
   "bbop_ann" : {
      "searchable_extension" : "_searchable",
      "result_weights" : "annotation_class_label^9.0 evidence_type^8.0 bioentity_label^7.0 type^6.0 source^4.0 taxon_label^3.0 annotation_extension_class_label^1.0",
      "filter_weights" : "source^7.0 evidence_type^6.0 taxon^5.0 isa_partof_label_closure^4.0 annotation_extension_class^3.0 type^2.0",
      "_infile" : "/home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/ann-config.yaml",
      "display_name" : "Annotations",
      "description" : "A description of annotations for GOlr and AmiGO.",
      "boost_weights" : "annotation_class^2.0 annotation_class_label^1.0 bioentity_id^2.0 bioentity_label^1.0 annotation_extension_class^2.0 annotation_extension_class_label^1.0",
      "fields" : [
         {
            "transform" : [],
            "description" : "A unique (and internal) combination of bioentity and ontology class.",
            "display_name" : "Acc",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "id",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 15: assigned by.",
            "display_name" : "Source",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "source",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 14: date of assignment.",
            "display_name" : "Date",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "date",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 13: taxon.",
            "display_name" : "Taxon",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "taxon",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Derived from C13 + ncbi_taxonomy.obo.",
            "display_name" : "Taxon label",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "taxon_label",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Closure of ids/accs over isa and partof.",
            "display_name" : "isa/partof closure",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "isa_partof_closure",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Closure of labels over isa and partof.",
            "display_name" : "isa/partof label closure",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "isa_partof_label_closure",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 1 + columns 2.",
            "display_name" : "Bioentity id",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "bioentity_id",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 3: bioentity label.",
            "display_name" : "Bioentity label",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "bioentity_label",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 5.",
            "display_name" : "Annotation class",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "annotation_class",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 5 + ontology.",
            "display_name" : "Annotation class label",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "annotation_class_label",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "???",
            "display_name" : "Evidence type",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "evidence_type",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 8: with/from.",
            "display_name" : "With",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "evidence_with",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "???",
            "display_name" : "Reference",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "reference",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "???",
            "display_name" : "Annotation extension class",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "annotation_extension_class",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "???",
            "display_name" : "Annotation extension class label",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "annotation_extension_class_label",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "???",
            "display_name" : "Annotation extension class closure",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "annotation_extension_class_closure",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "???",
            "display_name" : "Annotation extension class label closure",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "annotation_extension_class_label_closure",
            "property" : "???"
         }
      ],
      "fields_hash" : {
         "annotation_extension_class" : {
            "transform" : [],
            "description" : "???",
            "display_name" : "Annotation extension class",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "annotation_extension_class",
            "property" : "???"
         },
         "source" : {
            "transform" : [],
            "description" : "Column 15: assigned by.",
            "display_name" : "Source",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "source",
            "property" : "???"
         },
         "isa_partof_label_closure" : {
            "transform" : [],
            "description" : "Closure of labels over isa and partof.",
            "display_name" : "isa/partof label closure",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "isa_partof_label_closure",
            "property" : "???"
         },
         "bioentity_label" : {
            "transform" : [],
            "description" : "Column 3: bioentity label.",
            "display_name" : "Bioentity label",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "bioentity_label",
            "property" : "???"
         },
         "date" : {
            "transform" : [],
            "description" : "Column 14: date of assignment.",
            "display_name" : "Date",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "date",
            "property" : "???"
         },
         "bioentity_id" : {
            "transform" : [],
            "description" : "Column 1 + columns 2.",
            "display_name" : "Bioentity id",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "bioentity_id",
            "property" : "???"
         },
         "reference" : {
            "transform" : [],
            "description" : "???",
            "display_name" : "Reference",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "reference",
            "property" : "???"
         },
         "evidence_type" : {
            "transform" : [],
            "description" : "???",
            "display_name" : "Evidence type",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "evidence_type",
            "property" : "???"
         },
         "id" : {
            "transform" : [],
            "description" : "A unique (and internal) combination of bioentity and ontology class.",
            "display_name" : "Acc",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "id",
            "property" : "???"
         },
         "annotation_extension_class_label_closure" : {
            "transform" : [],
            "description" : "???",
            "display_name" : "Annotation extension class label closure",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "annotation_extension_class_label_closure",
            "property" : "???"
         },
         "annotation_extension_class_label" : {
            "transform" : [],
            "description" : "???",
            "display_name" : "Annotation extension class label",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "annotation_extension_class_label",
            "property" : "???"
         },
         "annotation_class" : {
            "transform" : [],
            "description" : "Column 5.",
            "display_name" : "Annotation class",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "annotation_class",
            "property" : "???"
         },
         "taxon" : {
            "transform" : [],
            "description" : "Column 13: taxon.",
            "display_name" : "Taxon",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "taxon",
            "property" : "???"
         },
         "isa_partof_closure" : {
            "transform" : [],
            "description" : "Closure of ids/accs over isa and partof.",
            "display_name" : "isa/partof closure",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "isa_partof_closure",
            "property" : "???"
         },
         "taxon_label" : {
            "transform" : [],
            "description" : "Derived from C13 + ncbi_taxonomy.obo.",
            "display_name" : "Taxon label",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "taxon_label",
            "property" : "???"
         },
         "annotation_class_label" : {
            "transform" : [],
            "description" : "Column 5 + ontology.",
            "display_name" : "Annotation class label",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "annotation_class_label",
            "property" : "???"
         },
         "evidence_with" : {
            "transform" : [],
            "description" : "Column 8: with/from.",
            "display_name" : "With",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "evidence_with",
            "property" : "???"
         },
         "annotation_extension_class_closure" : {
            "transform" : [],
            "description" : "???",
            "display_name" : "Annotation extension class closure",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "annotation_extension_class_closure",
            "property" : "???"
         }
      },
      "document_category" : "annotation",
      "weight" : "20",
      "_strict" : 0,
      "id" : "bbop_ann",
      "_outfile" : "/home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/ann-config.yaml"
   },
   "bbop_bio" : {
      "searchable_extension" : "_searchable",
      "result_weights" : "label^2.0 id^1.0",
      "filter_weights" : "",
      "_infile" : "/home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/bio-config.yaml",
      "display_name" : "Bioentities",
      "description" : "A description of bioentities file for GOlr.",
      "boost_weights" : "id^2.0 label^2.0",
      "fields" : [
         {
            "transform" : [],
            "description" : "Bioentity id.",
            "display_name" : "Acc",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "id",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 12: type class id.",
            "display_name" : "Type class id",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "type",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 13: taxon.",
            "display_name" : "Taxon",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "taxon",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Derived from C13 + ncbi_taxonomy.obo.",
            "display_name" : "Taxon label",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "taxon_label",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Closure of ids/accs over isa and partof.",
            "display_name" : "isa/partof closure",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "isa_partof_closure",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Closure of labels over isa and partof.",
            "display_name" : "isa/partof label closure",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "isa_partof_label_closure",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Symbol or name.",
            "display_name" : "Label",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "label",
            "property" : "???"
         },
         {
            "transform" : [],
            "description" : "Column 1: Identifier database.",
            "display_name" : "Database",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "db",
            "property" : "???"
         }
      ],
      "fields_hash" : {
         "isa_partof_label_closure" : {
            "transform" : [],
            "description" : "Closure of labels over isa and partof.",
            "display_name" : "isa/partof label closure",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "isa_partof_label_closure",
            "property" : "???"
         },
         "db" : {
            "transform" : [],
            "description" : "Column 1: Identifier database.",
            "display_name" : "Database",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "db",
            "property" : "???"
         },
         "taxon" : {
            "transform" : [],
            "description" : "Column 13: taxon.",
            "display_name" : "Taxon",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "taxon",
            "property" : "???"
         },
         "isa_partof_closure" : {
            "transform" : [],
            "description" : "Closure of ids/accs over isa and partof.",
            "display_name" : "isa/partof closure",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "isa_partof_closure",
            "property" : "???"
         },
         "taxon_label" : {
            "transform" : [],
            "description" : "Derived from C13 + ncbi_taxonomy.obo.",
            "display_name" : "Taxon label",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "taxon_label",
            "property" : "???"
         },
         "id" : {
            "transform" : [],
            "description" : "Bioentity id.",
            "display_name" : "Acc",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "id",
            "property" : "???"
         },
         "type" : {
            "transform" : [],
            "description" : "Column 12: type class id.",
            "display_name" : "Type class id",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "type",
            "property" : "???"
         },
         "label" : {
            "transform" : [],
            "description" : "Symbol or name.",
            "display_name" : "Label",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "label",
            "property" : "???"
         }
      },
      "document_category" : "bioentity",
      "weight" : "30",
      "_strict" : 0,
      "id" : "bbop_bio",
      "_outfile" : "/home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/bio-config.yaml"
   },
   "bbop_ont" : {
      "searchable_extension" : "_searchable",
      "result_weights" : "label^10.0 id^8.0 description^6.0 source^4.0 synonym^3.0 alternate_id^2.0 comment^1.0",
      "filter_weights" : "",
      "_infile" : "/home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/ont-config.yaml",
      "display_name" : "Ontology",
      "description" : "Test mapping of ontology class for GO.",
      "boost_weights" : "id^2.0 label^2.0 description^1.0 comment^0.5 synonym^1.0 alternate_id^1.0",
      "fields" : [
         {
            "transform" : [],
            "description" : "Term acc/id.",
            "display_name" : "Acc",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "id",
            "property" : "getIdentifier"
         },
         {
            "transform" : [],
            "description" : "Common term name.",
            "display_name" : "Term",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "label",
            "property" : "getLabel"
         },
         {
            "transform" : [],
            "description" : "Term definition.",
            "display_name" : "Definition",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "description",
            "property" : "getDef"
         },
         {
            "transform" : [],
            "description" : "Term namespace.",
            "display_name" : "Source",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "source",
            "property" : "getNamespace"
         },
         {
            "transform" : [],
            "description" : "Is the term obsolete?",
            "display_name" : "Obsoletion",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "boolean",
            "id" : "is_obsolete",
            "property" : "getIsObsoleteBinaryString"
         },
         {
            "transform" : [],
            "description" : "Term comment.",
            "display_name" : "Comment",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "comment",
            "property" : "getComment"
         },
         {
            "transform" : [],
            "description" : "Term synonym.",
            "display_name" : "Synonym",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "synonym",
            "property" : "getOBOSynonymStrings"
         },
         {
            "transform" : [],
            "description" : "Alternate term id.",
            "display_name" : "Alt ID",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "alternate_id",
            "property" : "getAltIds"
         },
         {
            "transform" : [],
            "description" : "Term subset.",
            "display_name" : "Subset",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "subset",
            "property" : "getSubsets"
         },
         {
            "transform" : [],
            "description" : "Definition cross-reference.",
            "display_name" : "Def XRef",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "definition_xref",
            "property" : "getDefXref"
         }
      ],
      "fields_hash" : {
         "source" : {
            "transform" : [],
            "description" : "Term namespace.",
            "display_name" : "Source",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "source",
            "property" : "getNamespace"
         },
         "definition_xref" : {
            "transform" : [],
            "description" : "Definition cross-reference.",
            "display_name" : "Def XRef",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "definition_xref",
            "property" : "getDefXref"
         },
         "alternate_id" : {
            "transform" : [],
            "description" : "Alternate term id.",
            "display_name" : "Alt ID",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "alternate_id",
            "property" : "getAltIds"
         },
         "description" : {
            "transform" : [],
            "description" : "Term definition.",
            "display_name" : "Definition",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "description",
            "property" : "getDef"
         },
         "comment" : {
            "transform" : [],
            "description" : "Term comment.",
            "display_name" : "Comment",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "comment",
            "property" : "getComment"
         },
         "synonym" : {
            "transform" : [],
            "description" : "Term synonym.",
            "display_name" : "Synonym",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "synonym",
            "property" : "getOBOSynonymStrings"
         },
         "subset" : {
            "transform" : [],
            "description" : "Term subset.",
            "display_name" : "Subset",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "multi",
            "type" : "string",
            "id" : "subset",
            "property" : "getSubsets"
         },
         "id" : {
            "transform" : [],
            "description" : "Term acc/id.",
            "display_name" : "Acc",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "id",
            "property" : "getIdentifier"
         },
         "label" : {
            "transform" : [],
            "description" : "Common term name.",
            "display_name" : "Term",
            "searchable" : "true",
            "required" : "false",
            "cardinality" : "single",
            "type" : "string",
            "id" : "label",
            "property" : "getLabel"
         },
         "is_obsolete" : {
            "transform" : [],
            "description" : "Is the term obsolete?",
            "display_name" : "Obsoletion",
            "searchable" : "false",
            "required" : "false",
            "cardinality" : "single",
            "type" : "boolean",
            "id" : "is_obsolete",
            "property" : "getIsObsoleteBinaryString"
         }
      },
      "document_category" : "ontology_class",
      "weight" : "40",
      "_strict" : 0,
      "id" : "bbop_ont",
      "_outfile" : "/home/sjcarbon/local/src/svn/owltools/OWLTools-Solr/src/main/resources/ont-config.yaml"
   }
};

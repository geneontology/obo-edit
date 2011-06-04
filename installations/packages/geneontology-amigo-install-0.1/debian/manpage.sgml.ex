<!doctype refentry PUBLIC "-//OASIS//DTD DocBook V4.1//EN" [

<!-- Process this file with docbook-to-man to generate an nroff manual
     page: `docbook-to-man manpage.sgml > manpage.1'.  You may view
     the manual page with: `docbook-to-man manpage.sgml | nroff -man |
     less'.  A typical entry in a Makefile or Makefile.am is:

manpage.1: manpage.sgml
	docbook-to-man $< > $@


	The docbook-to-man binary is found in the docbook-to-man package.
	Please remember that if you create the nroff version in one of the
	debian/rules file targets (such as build), you will need to include
	docbook-to-man in your Build-Depends control field.

  -->

  <!-- Fill in your name for FIRSTNAME and SURNAME. -->
  <!ENTITY dhfirstname "<firstname>FIRSTNAME</firstname>">
  <!ENTITY dhsurname   "<surname>SURNAME</surname>">
  <!-- Please adjust the date whenever revising the manpage. -->
  <!ENTITY dhdate      "<date>May 26, 2011</date>">
  <!-- SECTION should be 1-8, maybe w/ subsection other parameters are
       allowed: see man(7), man(1). -->
  <!ENTITY dhsection   "<manvolnum>SECTION</manvolnum>">
  <!ENTITY dhemail     "<email>sjcarbon@lbl.gov</email>">
  <!ENTITY dhusername  "Seth Carbon">
  <!ENTITY dhucpackage "<refentrytitle>GENEONTOLOGY-AMIGO-INSTALL</refentrytitle>">
  <!ENTITY dhpackage   "geneontology-amigo-install">

  <!ENTITY debian      "<productname>Debian</productname>">
  <!ENTITY gnu         "<acronym>GNU</acronym>">
  <!ENTITY gpl         "&gnu; <acronym>GPL</acronym>">
]>

<refentry>
  <refentryinfo>
    <address>
      &dhemail;
    </address>
    <author>
      &dhfirstname;
      &dhsurname;
    </author>
    <copyright>
      <year>2003</year>
      <holder>&dhusername;</holder>
    </copyright>
    &dhdate;
  </refentryinfo>
  <refmeta>
    &dhucpackage;

    &dhsection;
  </refmeta>
  <refnamediv>
    <refname>&dhpackage;</refname>

    <refpurpose>Meta-package to help install AmiGO</refpurpose>
  </refnamediv>
  <refsynopsisdiv>
    <cmdsynopsis>
      <command>&dhpackage;</command>

      <!-- <arg><option>-e <replaceable>this</replaceable></option></arg> -->

      <!-- <arg><option>--example <replaceable>that</replaceable></option></arg> -->
    </cmdsynopsis>
  </refsynopsisdiv>
  <refsect1>
    <title>DESCRIPTION</title>

    <para>This manual page documents briefly
      <command>&dhpackage;</command>.</para>

    <para>This manual page was written for the &debian; distribution
      because the original program does not have a manual page.
      Instead, it has documentation in the &gnu;
      <application>Info</application> format; see below.</para>

    <para><command>&dhpackage;</command> is a meta-package that is intended to aid in the installation of the Gene Ontology Consortium's AmiGO software from source. Eventually, the entire AmiGO installation process will be automated to a package install.</para>

  </refsect1>
  <refsect1>
    <title>AUTHOR</title>

    <para>This manual page was written by &dhusername; &dhemail; for
      the &debian; system (and may be used by others).  Permission is
      granted to copy, distribute and/or modify this document under
      the terms of the &gnu; General Public License, Version 2 any
      later version published by the Free Software Foundation.
    </para>
    <para>
      On Debian systems, the complete text of the GNU General Public
      License can be found in /usr/share/common-licenses/GPL.
    </para>

  </refsect1>
</refentry>

<!-- Keep this comment at the end of the file
Local variables:
mode: sgml
sgml-omittag:t
sgml-shorttag:t
sgml-minimize-attributes:nil
sgml-always-quote-attributes:t
sgml-indent-step:2
sgml-indent-data:t
sgml-parent-document:nil
sgml-default-dtd-file:nil
sgml-exposed-tags:nil
sgml-local-catalogs:nil
sgml-local-ecat-files:nil
End:
-->

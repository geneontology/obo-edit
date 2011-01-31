<?xml version = "1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="html"/>

    <xsl:key name="terms" match="term" use="id"/>

    <xsl:template match="/">
	<html>
		<head>
		<xsl:element name="link">
				<xsl:attribute name="href">obo_html.css</xsl:attribute>
				<xsl:attribute name="rel">stylesheet</xsl:attribute>
				<xsl:attribute name="type">text/css</xsl:attribute>
		</xsl:element>
		</head>
		<body>
			<xsl:apply-templates select="obo/term"/>
		</body>
	</html>

    </xsl:template>

    <xsl:template match="term">
	<ul>
		<li>
			<xsl:element name="a">
				<xsl:attribute name="id">
					<xsl:value-of select="id"/>
				</xsl:attribute>
				<xsl:text>[Term]</xsl:text>
			</xsl:element>
		</li>
		<li>
			<span class="label">id: </span><xsl:value-of select="id"/>
		</li>
		<li>
			<span class="label">name: </span><xsl:value-of select="name"/>
		</li>
		<li>
			<span class="label">ontology: </span><xsl:value-of select="name"/>
		</li>
		<xsl:apply-templates select="is_a"/>
		<xsl:apply-templates select="relationship"/>
		<xsl:apply-templates select="synonym"/>
	</ul>
    </xsl:template>

    <xsl:template match="is_a">
	<li>
		<span class="label"><xsl:text>is_a: </xsl:text></span>
		<span class="term_label">
			<xsl:value-of select="key('terms', .)/name"/>
		</span>
		<xsl:text> </xsl:text>
		<xsl:element name="a">
			<xsl:attribute name="href">
				<xsl:text>#</xsl:text>
				<xsl:value-of select="."/>
			</xsl:attribute>
			<xsl:value-of select="."/>
		</xsl:element>

	</li>
    </xsl:template>

    <xsl:template match="relationship">
	<li>
		<span class="label"><xsl:text>relationship: </xsl:text></span>
		<xsl:value-of select="type"/>
		<xsl:text> </xsl:text>
		<span class="term_label">
			<xsl:value-of select="key('terms', to)/name"/>
		</span>
		<xsl:text> </xsl:text>
		<xsl:element name="a">
			<xsl:attribute name="href"><xsl:text>#</xsl:text><xsl:value-of select="to"/></xsl:attribute>
			<xsl:value-of select="to"/>
		</xsl:element>
	</li>
    </xsl:template>

    <xsl:template match="synonym">
	<li>
		<span class="label">synonym: </span>
		<xsl:value-of select="synonym_text"/>
	</li>
    </xsl:template>


   <xsl:template match="text()|@*">
    </xsl:template>

</xsl:stylesheet>




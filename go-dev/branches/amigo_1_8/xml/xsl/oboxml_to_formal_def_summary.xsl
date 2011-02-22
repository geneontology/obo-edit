<?xml version = "1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="html"/>

    <xsl:key name="k-term" match="//term" use="id"/>
    <xsl:key name="k-term-by-genus" match="//term" use="intersection_of[type='is_a']/to"/>
    <xsl:key name="k-term-by-superclass" match="//term" use="is_a"/>

    <xsl:key 
      name='k-genus'
      match="intersection_of[not(type)]"
      use='to'/>

    <xsl:key 
      name='k-differentia-type'
      match="intersection_of[type!='is_a']"
      use='type'/>

    <xsl:key 
      name='k-differentia-to'
      match="intersection_of[type!='is_a']"
      use='to'/>

    <xsl:key 
      name='k-reltype'
      match="//typedef"
      use='id'/>

    <xsl:variable 
      name="u-genus"
      select='//intersection_of[generate-id(.)=generate-id(key("k-genus",to)[1])]'/>

    <xsl:template match="/obo">
      <html>
        <head>
          <xsl:element name="link">
            <xsl:attribute name="href">../doc/obo_html.css</xsl:attribute>
            <xsl:attribute name="rel">stylesheet</xsl:attribute>
            <xsl:attribute name="type">text/css</xsl:attribute>
          </xsl:element>
        </head>
        <body>
          <h1>Formal Definitions and Inferences</h1>
          <!-- intro blurb -->
          <div class="summary">
            Only terms which have a formal definition are shown. Terms
            are grouped by <b>Genus</b>. Terms are uniquely identified
          by their <b>Genus/Differentiae</b> formal
          definition. <i>Anonymous</i> terms have been created in
          order to complete some definitions. <b>{inferred}</b>
          relationships were generated automatically, using the
          definition; <b>{contingent}</b> relationships were
          curator-created, and would have been inferred if not
          present. Inferred relationships that have already been
          vetted and discarded are shown greyed-out.
          </div>

          <!-- show genus tree -->
          <xsl:if test="0=1">
          <ul>
            <xsl:for-each select="$u-genus">
            <xsl:sort data-type="text" select="key('k-term',to)/name" order="ascending"/>
              <li>
                <xsl:value-of select="key('k-term',to)/name"/>
                <xsl:apply-templates mode="id_href" select="to"/>
              </li>
            </xsl:for-each>
          </ul>
          </xsl:if>
          
          <!-- show genus details for all genera -->
          <xsl:for-each select="$u-genus">
            <xsl:apply-templates mode="genus-details" select="to"/>
          </xsl:for-each>
        </body>
      </html>
    </xsl:template>

    <xsl:template mode="tree-view" match="term">
      <li>
        <xsl:value-of select="name"/>
        <xsl:apply-templates mode="id_href" select="id"/>
        <xsl:for-each select="//term[is_a=current()/to]">
          <xsl:apply-templates mode="tree-view" select="key('k-term',is_a)"/>
        </xsl:for-each>
      </li>
    </xsl:template>

    <xsl:template mode="genus-details" match="*">
      <br></br>
      <div class="genus">
        <xsl:attribute name="id">
          <xsl:value-of select="."/>
        </xsl:attribute>
        <h2>
          <xsl:text>Genus: </xsl:text>
          <i>
            <xsl:value-of select="key('k-term',.)/name"/>
          </i>
          <xsl:text> [</xsl:text>
          <xsl:value-of select="."/>
          <xsl:text>] in </xsl:text>
          <i>
            <xsl:value-of select="key('k-term',.)/namespace"/>
          </i>
        </h2>
        <div class="genus_summary">
          <xsl:apply-templates select="key('k-term',.)/is_a"/>
          <xsl:apply-templates select="key('k-term',.)/relationship"/>
          <xsl:variable name="genus-id" select="."/>
          <!-- use munchean transform to select distinct types -->
          <xsl:variable 
            name="u-reltype"
            select="key('k-term-by-genus',.)/intersection_of[generate-id(.)=generate-id(key('k-differentia-type',type)[1])]"/>
          <xsl:if test="count($u-reltype)>0">
            <h3>Constraints over types:</h3>
            <ul>
              <xsl:for-each select="$u-reltype">
                <li>
                  <i>
                    <xsl:value-of select="type"/>
                  </i>
                </li>
              </xsl:for-each>
            </ul>
          </xsl:if>
        </div>
        <table>
          <tr>
            <th align="left">
              <xsl:text>ID</xsl:text>
            </th>
            <th align="left">
              <xsl:text>Name</xsl:text>
            </th>
            <th align="left">
              <xsl:text>Differentiae</xsl:text>
            </th>
            <th align="left">
              <xsl:text>Relationships</xsl:text>
            </th>
          </tr>
          <xsl:apply-templates select="key('k-term-by-genus',.)"/>
        </table>
      </div>
    </xsl:template>

    <xsl:template match="term">
      <tr>
        <td class="id">
          <a>
            <xsl:attribute name="name">
              <xsl:value-of select="id"/>              
            </xsl:attribute>
          </a>
          <xsl:value-of select="id"/>
        </td>
        <td class="name">
          <xsl:value-of select="name"/>
        </td>
        <td class="intersection_of">
          <xsl:apply-templates select="intersection_of[type!='is_a']"/>
        </td>
        <td class="relationship">
          <xsl:apply-templates select="is_a"/>
          <xsl:apply-templates select="relationship"/>
        </td>
      </tr>
    </xsl:template>

    <xsl:template match="intersection_of">
      <div class="differentiae">
        <xsl:apply-templates select="type"/>
        <xsl:text> </xsl:text>
        <xsl:value-of select="key('k-term',to)/name"/>
        <xsl:apply-templates mode="id_href" select="to"/>
      </div>
    </xsl:template>

    <xsl:template match="relationship">
      <div class="relationship">
        <xsl:apply-templates select="type"/>
        <xsl:value-of select="key('k-term',to)/name"/>
        <xsl:apply-templates mode="id_href" select="to"/>
      </div>
    </xsl:template>

    <xsl:template match="is_a">
      <div>
        <xsl:attribute name="class">
          <xsl:choose>
            <xsl:when test="@problematic_inferred='true'">
              <xsl:text>problematic_relationship</xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>relationship</xsl:text>              
            </xsl:otherwise>
          </xsl:choose>
        </xsl:attribute>
        <i><xsl:text>is_a:</xsl:text></i>
        <xsl:value-of select="key('k-term',.)/name"/>
        <xsl:apply-templates mode="id_href" select="."/>
        <xsl:if test="@contingent='true'">
          <b><xsl:text>{contingent}</xsl:text></b>
        </xsl:if>
        <xsl:if test="@novel_inferred='true' or @problematic_inferred='true'">
          <b><xsl:text>{inferred}</xsl:text></b>
        </xsl:if>
        <xsl:if test="@notes">
          <font size="-2"><xsl:value-of select="@notes"/></font>
        </xsl:if>
      </div>
    </xsl:template>

    <xsl:template match="type">
      <i>
        <xsl:value-of select="."/>
      </i>
      <xsl:text>:</xsl:text>
    </xsl:template>

    <xsl:template mode="id_href" match="text()">
      <xsl:if test="count(key('k-term',.)/intersection_of) > 0">
        <a>
          <xsl:attribute name="href">
            <xsl:text>#</xsl:text>
            <xsl:value-of select="."/>
          </xsl:attribute>
          <xsl:text>[T]</xsl:text>
        </a>
      </xsl:if>
      <xsl:if test="count(key('k-term-by-genus',.)) > 0">
        <a>
          <xsl:attribute name="href">
            <xsl:text>#</xsl:text>
            <xsl:value-of select="."/>
          </xsl:attribute>
          <xsl:text>[G]</xsl:text>
        </a>
      </xsl:if>
      <sub>
        <xsl:text>[</xsl:text>
        <xsl:value-of select="."/>
        <xsl:text>]</xsl:text>
      </sub>
    </xsl:template>

   <xsl:template match="text()|@*">
    </xsl:template>

</xsl:stylesheet>




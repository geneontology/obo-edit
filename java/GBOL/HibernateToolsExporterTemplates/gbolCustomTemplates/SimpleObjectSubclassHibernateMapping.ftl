<?xml version="1.0"?>

<!DOCTYPE hibernate-mapping PUBLIC "-//Hibernate/Hibernate Mapping DTD 3.0//EN" "http://hibernate.sourceforge.net/hibernate-mapping-3.0.dtd">

<hibernate-mapping>
    <subclass name="org.gmod.gbol.simpleObject.${pojo.getDeclarationName()}" extends="org.gmod.gbol.simpleObject.generated.${pojo.getDeclarationName()}_g" discriminator-value="not null">
    </subclass>
</hibernate-mapping>
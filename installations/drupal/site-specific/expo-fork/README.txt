Drush has been unreliable, switching back to manual...
The 12 Steps for a Drupal upgrade:

1) Get and unpack new drupal:
   wget <new drupal>
   tar -zxvf <new drupal>

2) Log in as root (will need later):
   ...

3) Set site to offline:
   http://fork.lbl.gov/admin/config/development/maintenance 

4) Disable contributed modules (may take several passes):
   Pathauto, Token

5) Move old drupal directory:
   mv htdocs drupal-7.YY

6) Move new drupal directory:
  tar -zxvf drupal-7.XX.tar.gz 
  mv drupal-7.XX htdocs

7) Move "sites":
   mv htdocs/sites htdocs/sites.orig
   mv drupal-7.YY/sites htdocs

8) Move misc.:
   mv htdocs/.htaccess htdocs/.htaccess.orig
   mv drupal-7.YY/.htaccess htdocs

9) Go directly to update.php:
   http://fork.lbl.gov/update.php

11) Enable contributed modules (same as above):
   http://fork.lbl.gov/admin/modules

12) Set site to online:
   http://fork.lbl.gov/admin/config/development/maintenance

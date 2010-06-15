
The 12 Steps for a Drupal upgrade:

0) Get and unpack new drupal:
   wget <new drupal>
   tar -zxvf <new drupal>

1) Backup db:
   /local/dumps/save_db.pl -v -s localhost -d drupal_news4go -t /local/dumps

2) Backup files (permissions may not be preserved):
    /local/dumps/clone_drupal.pl -v -d /local/www/htdocs/news4go -t /local/dumps

3) Log in as root (will need later):
   ...

4) Set site to offline:
   http://go.berkeleybop.org/news4go/admin/settings/site-maintenance

5) Disable contributed modules (may take several passes):
   Calendar, Date API,  Date Timezone
   Advanced Help, BUEditor, IMCE, Twitter, Twitter Actions
   CAPTCHA, reCAPTCHA
   Google Analytics
   Views, Views UI

6) Move old drupal directory:
   mv news4go news4go.old

7) Move new drupal directory:
   mv drupal-6.XX news4go

8) Move "sites":
   mv news4go/sites news4go/sites.orig
   mv news4go.old/sites news4go

9) Move misc.:
   mv news4go.old/.htaccess news4go

10) Go directly to update.php:
   http://go.berkeleybop.org/news4go/update.php

11) Enable contributed modules (same as above):
   http://go.berkeleybop.org/news4go/admin/build/modules

12) Set site to online:
   http://go.berkeleybop.org/news4go/admin/settings/site-maintenance

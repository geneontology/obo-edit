package GO::Template::Template;

use Template;

sub process_template {
    my ($self, $session, $template, $args) = @_;

    my $template_paths = $session->get_param('template_paths');

    my $tt = Template->new({
	PLUGIN_BASE => 'GO::Template::Plugin',
	INCLUDE_PATH=>$template_paths,
	EVAL_PERL=>1,
	PRE_CHOMP=>1,
	POST_CHOMP=>1,
	TRIM=>1
	});

    $tt->process($template, $args);

};

1;

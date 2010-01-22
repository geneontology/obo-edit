#!/usr/bin/ruby1.8 -w
####
#### NOTE: Right now, needs to run from "go-dev/amigo/testing".
#### Run tests: ./webunit.rb
####

require 'mechanize'
require 'uuidtools'


## Shared code for Page Agents.
## TODO: Add cookie management (i.e. pass to descendents) in a.cookies => [].
class PAgent

  attr_reader :agent, :core

  def initialize (init)

    @agent = WWW::Mechanize.new
    @agent.user_agent= "AmiGO Inspector 0.01 (from ruby WWW-Mechanize/0.9.3)"
    @agent.read_timeout= 600 # is that right? 10min?

    @okay = true

    ## Init @core.
    if init.class.eql?(WWW::Mechanize::Page)
      @core = init
    elsif init.class.eql?(WWW::Mechanize::File)
      @core = init
    elsif init.class.eql?(String)
      begin
        @core = @agent.get(init)
      rescue WWW::Mechanize::ResponseCodeError
        puts "bad response on init"
        @core = nil
        @okay = false
      rescue SocketError
        puts "bad getaddrinfo on init"
        @core = nil
        @okay = false
      rescue Timeout::Error
        puts "timeout out on init"
        @core = nil
        @okay = false
      end
    else
      raise "unknown argument type"
    end
  end

  def okay?
    @okay
  end
  
  def code
    @core.code
  end
  
  def content
    @core.body
  end
  
  def uri
    if @agent.history.size != 0
      @agent.history[0].uri.to_s
    else
      ''
    end
  end
  
  def uuid
    UUIDTools::UUID.random_create.to_s
  end

  ## Take id input, catch output errors.
  ## DEPRECATED: moved into PageVerifier
  def dump (output_dir, token_label = "")

    ## Work if there is a core, otherwise
    if @core and @core.response

      ## Get the best suffix we can.
      suffix = '.nil'
      ct = @core.response['content-type']
      if ct
        suffix = '.' + ct.split('/')[-1]
      end

      ## Good times.
      gtime = Time.now.strftime("%Y%m%d%H%M%S")

      ## Complete file name.
      fname = nil
      if token_label.eql?('')
        fname = output_dir +'/'+ gtime +'_'+ uuid + suffix
      else
        slabel = File.basename(token_label, '.t')
        fname = output_dir +'/'+ gtime +'_'+ slabel + suffix
      end
      
      # puts "output_dir: #{output_dir}"
      # puts "gtime: #{gtime}"
      # puts "token_label: #{token_label}"
      # puts "uuid: #{uuid}"
      # puts "suffix: #{suffix}"
      
      ##
      begin
        File.open(fname, 'w') {|f| f.write(@core.body) }
        # puts "HERE"
      rescue
        fname = nil
        # puts "THERE"
      end

      fname
    else
      nil
    end

  end
  
end

  
## Page Agent specialized for dealing with graphics.
class PAgent::Graphic < PAgent
  ## Graphics are boring...
end


## Page Agent specialized for dealing with HTML pages and forms.
class PAgent::HTML < PAgent

  def links
    @core.links
  end

  ## Find all of the form identifiers that we can. They better be
  ## unique.
  def form_identifiers
    
    ret = @core.forms.map do |f|
      if f.name or f.action
        f.name or f.action
      else
        throw "couldn\'t get name or action"
      end
    end
    
    if ret.length != ret.uniq.length
      throw "amibiguously named forms--should fix in amigo"
    end
    ret
  end

  ## Find all of the field identifiers that we can. They better be
  ## unique.
  def field_identifiers (form_identifier)
    
    form = find_form(form_identifier)
    ret = form.fields.map do |f|
      if f.name
        f.name
      else
        throw "couldn\'t get name or action"
      end
    end
  end

  ##
  def select_values (form_identifier, select_name)
    form = find_form(form_identifier)
    select = form.field_with(:name => select_name)
    select.options.map { |o| o.value }
    #select.value = opt_template_array
  end

  ## Return a specific form from the agent. Will check form names
  ## and actions.
  def find_form (form_identifier)
    
    ##
    extract_uniq_form = lambda do |forms|
      if forms.length == 0
        nil
      elsif forms.length == 1
        forms[0]          
      else
        raise "could not construct off of argument"
      end
    end

    ## Poke around and try and find the correct one--search throu
    ret = extract_uniq_form.call(@core.forms_with(:name=>form_identifier))
    if ret.nil?
      ret = extract_uniq_form.call(@core.forms_with(:action=>form_identifier))
    end
    ret
  end

  ##
  def set_upload (form_identifier, name, file_str)

    form = find_form(form_identifier)

    ## Test for the existance of the file.
    if not File.readable? file_str
      raise "Cannot locate data file: \"#{file_str}\"!"
    end
    
    ## Upload.
    form.file_upload_with(:name => name).file_name = file_str
  end
  
  def set_multi_select (form_identifier, name, opt_template_array)
    form = find_form(form_identifier)
    select = form.field_with(:name => name)
    select.value = opt_template_array
  end
  
  def set_select (form_identifier, name, opt_template)
    form = find_form(form_identifier)
    select = form.field_with(:name => name)
    select.value = opt_template
  end
  
  def set_radio (form_identifier, name, opt)
    form = find_form(form_identifier)
    form.radiobuttons_with(:name => name).each do |r|
      if r.value.eql?(opt)
        r.check
      else
        r.uncheck
      end
    end
  end
  
  def set_field (form_identifier, name, value)
    form = find_form(form_identifier)
    field = form.field_with(:name => name)
    field.value = value
  end

  ## Turn the conf fields into form information. If it doesn't work,
  ## return nil. If it works, true.
  def form_from_conf (form_conf)
    
    ##
    form_id = form_conf.get('form')
    if form_id.nil?
      # raise "we need at least a form name to prepare it"
      nil
    else
      
      types = {
        'radio' => :set_radio,
        'field' => :set_field,
        'upload' => :set_upload,
        'select' => :set_select,
        'multi_select' => :set_multi_select
      }
      types.each_pair do |label, function|
        params_of_type = form_conf.get(label)
        if ! params_of_type.nil? 
          params_of_type.each_pair do |key,val|
            send(function, form_id, key, val)
          end
        end
        
      end
      
      true
    end
  end
  
  ## TODO: Turn the extra conf fields into meta information (comments,
  ## tests to run, etc.).
  def meta_from_conf (form_conf)
    
    ##
    form_id = form_conf.get('form')
    if form_id.nil?
      raise "we need at least a form name to prepare it"
    end
      
    types = {
      'radio' => :set_radio,
      'field' => :set_field,
      'upload' => :set_upload,
      'select' => :set_select,
      'multi_select' => :set_multi_select
      }
    types.each_pair do |label, function|
      params_of_type = form_conf.get(label)
      if ! params_of_type.nil? 
        params_of_type.each_pair do |key,val|
          send(function, form_id, key, val)
        end
      end
      
    end

    nil
  end
  
  def create_pagent (thing)

    ## Choose the return value.
    if ! thing.nil?
      if thing.class.eql?(WWW::Mechanize::Page)
        PAgent::HTML.new(thing)
      elsif thing.class.eql?(WWW::Mechanize::File)
        PAgent::Graphic.new(thing)
      else
        raise "thing is an unknown creature"
      end
    end
  end

  ##
  def submit (form_identifier)
    
    form = find_form(form_identifier)
    
    new_thing = nil
    begin
      new_thing = @agent.submit(form)
    rescue WWW::Mechanize::ResponseCodeError
      puts "bad response on submit"
    rescue SocketError
      puts "bad getaddrinfo on submit"
    rescue Timeout::Error
      puts "timeout out on submit"
    end
    
    create_pagent(new_thing)
  end

  ## 
  def click (classed_link)
    
    new_thing = nil
    begin
      new_thing = classed_link.click
    rescue WWW::Mechanize::ResponseCodeError
      puts "bad response on click"
    rescue SocketError
      puts "bad getaddrinfo on click"
    rescue Timeout::Error
      puts "timeout out on click"
    end
    
    create_pagent(new_thing)
  end
  
end


###
### Self-test.
###


if __FILE__ == $0

  require 'test/unit'
  #include AmiGO
  
  ## Test the job thingy to make sure that it's working.
  class PAgent_Test_Suite < Test::Unit::TestCase

    ## Run before every test.
    def setup
      @p = PAgent::HTML.new('http://google.com')
    end

    ## Run after every test.
    def teardown
    end

    ##
    def test_pagent_simple_page
      assert(@p.okay?, "got something from google")
      assert(@p.code.eql?("200"), "good page")
    end

    ##
    def test_pagent_simple_form

      @p.set_field("f", "q", "gene ontology amigo")
      new_pagent = @p.submit("f")

      assert(new_pagent.okay?, "got something from google")
      assert(new_pagent.code.eql?("200"), "good page")
    end

    ##
    def test_pagent_simple_links

      links = @p.links
      assert(links.size > 10, "got some links on the page")

      links.each do |l|
        new_pagent = @p.click(l)
        assert(new_pagent.okay?, "got something from google")
        assert(new_pagent.code.eql?("200"), "good page")
      end

    end

  end

end

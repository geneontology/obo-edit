#!/usr/bin/ruby -w
####
#### NOTE: Right now, needs to run from "go-dev/amigo".
#### Run tests: ./scripts/amigo.rb
####
#### TODO: logger mixin
####

module AmiGO

  require 'json/pure'
  require 'mechanize'
  require 'uuidtools'

  ##
  class JSONConf

    @conf_hash = {};

    def initialize (file_str)

      ## If extant, bring in the JSON config.
      if File.exists?(file_str) && File.readable?(file_str)
        json_str = IO.readlines(file_str).to_s
        if json_str
          @conf_hash = JSON.parse json_str
        end
        
      end

      ##
      if @conf_hash.empty?
        raise "empty configuration"
      end

      @conf_hash
    end


    ##
    def get (key)

      ## Simple ladder.
      if @conf_hash.has_key?(key.upcase)
        @conf_hash.fetch(key.upcase, nil)
      elsif @conf_hash.has_key?(key.downcase)
        @conf_hash.fetch(key.downcase, nil)
      else
        nil
      end

    end

  end


  ## Figure out where agents should go and what they should do there.
  ## TODO: Will probably end up being a combination of PageRunners and
  ## PageVerifiers.
  class Inspector

    attr_reader :base_uri, :data_dir, :dump_dir

    def initialize (start_url, data_home, dump_home)

      @base_uri = URI.parse(start_url)

      ## TODO: test to make sure that these are okay.
      @data_dir = data_home
      @dump_dir = dump_home
      
    end

    ##
    def same_host (thingy)
      
      ##
      uri = nil
      if thingy.class.eql?(URI) 
        ## Good--that's what we want.
        uri = thingy
      elsif thingy.class.eql?(String) 
        uri = URI.parse('http://localhost/cgi-bin/foo')
      else
        raise "I have no idea what this non-URI thing is"
      end

      @base_uri.host.eql?(uri.host)
    end

  end


  ## A class to run a pagent out of a file.
  ## Ends up looking a lot like JSONConf, doesn't it...
  class PageRunner

    attr_reader :resultant_page

    @json_conf = nil
    @tester = nil

    def initialize (file_str)
      @json_conf = AmiGO::JSONConf.new(file_str)
    end

    ##
    def over_page (url)

      @tester = AmiGO::PAgent::HTML.new(url)
      @tester.form_from_template(@template_hash)      

      form_name = @json_conf.get('form')
      @resultant_page = @tester.submit(form_name)

    end

  end


  ## Shared code for Page Agents.
  ## TODO: Add cookie management (i.e. pass to descendents) in a.cookies => [].
  class PAgent

    attr_reader :agent, :okay, :core

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
      elsif init.class.eql?(String) ||
          init.class.eql?(URI) ||
          init.class.eql?(URI::FTP) ||
          init.class.eql?(URI::HTTP) ||
          init.class.eql?(URI::HTTPS) then

        ## Stringify URIs
        if init.class.eql?(URI) ||
            init.class.eql?(URI::FTP) ||
            init.class.eql?(URI::HTTP) ||
            init.class.eql?(URI::HTTPS) then
          init = init.to_s
        end

        begin
          @core = @agent.get(init)
        rescue WWW::Mechanize::ResponseCodeError
          puts "bad response on init"
          @core = nil
          @okay = false
        rescue WWW::Mechanize::UnsupportedSchemeError
          puts "BUG: is this an FTP problem?"
          @core = nil
          @okay = false
        rescue SocketError
          puts "bad getaddrinfo on init"
          @core = nil
          @okay = false
        rescue Timeout::Error, Errno::ETIMEDOUT
          puts "timeout out on init"
          @core = nil
          @okay = false
        end
      else
        raise "unknown argument type"
      end
    end

    def code
      @core.code
    end

    def content
      @core.body
    end

    def uri
      @agent.history[0].uri.to_s
    end

    def uuid
      UUIDTools::UUID.random_create.to_s
    end

    def dump (dump_home)
      ct = @core.response['content-type']
      suffix = '.' + ct.split('/')[-1]
      fname = dump_home + uuid + suffix
      File.open(fname, 'w') {|f| f.write(@core.body) }
      fname
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

    ## BUG/TODO: We'll need to go

    def set_upload (form_identifier, name, file_str)
      form = find_form(form_identifier)
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

    ## TODO: 
    def form_from_template (form_hash)
      
      ##
      form_id = form_hash.fetch('form', nil)
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
        params_of_type = form_hash.fetch(label, nil)
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
          AmiGO::PAgent::HTML.new(thing)
        elsif thing.class.eql?(WWW::Mechanize::File)
          AmiGO::PAgent::Graphic.new(thing)
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

end

###
### Self-test the modules we have.
###

if __FILE__ == $0

  require 'test/unit'
  include AmiGO
  
  ## Test the job thingy to make sure that it's working.
  class AmiGO_JSONConf_Test_Suite < Test::Unit::TestCase

    ## Run before every test.
    def setup
      @aconf = AmiGO::JSONConf.new('./config.json')
    end

    ## Run after every test.
    def teardown
    end

    ##
    def test_configure

      assert(! @aconf.nil?, "got something")

      assert(@aconf.get('blahblahblah').nil?, "nothing for nothing")

      assert(! @aconf.get('go_dbhost').nil?, "found something for dbhost")
      assert(@aconf.get('go_dbhost').eql?("localhost"), "found dbhost")
      assert(! @aconf.get('GO_DBHOST').nil?, "found something for dbhost")
      assert(@aconf.get('GO_DBHOST').eql?("localhost"), "found dbhost")

    end

  end

end

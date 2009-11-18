#!/usr/bin/ruby -w
####
#### NOTE: Right now, needs to run from "go-dev/amigo/testing".
#### Run tests: ./aunit.rb
####

module AUnit

  require 'utils'
  require 'pagent'


  ## TODO: See tanuki
  ## Figure out where agents should go and what they should do there.
  ## TODO: Will probably end up being a combination of PageRunners and
  ## PageVerifiers with additional logic. It will take a position
  ## similar to tanuki's.
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
  class PageRunner

    # attr_queue :error
    attr_reader :resultant, :comment, :page, :url, :tests, :assertions

    @json_conf = nil
    @tester = nil

    def initialize (home_url, file_str)

      @json_conf = AmiGO::Conf.new(file_str)
      @comment = @json_conf.get('comment') || ''

      @page = @json_conf.get('page') || ''
      if @page.eql?('')
        raise "miss formatted conf file in page"      
      else
        @url = home_url + '/' + @page
      end

      ## Pull the boolean tests out of the conf.
      tmp = @json_conf.get('tests') || []
      @tests = tmp.map do |t|
        t.to_sym
      end

      ##
      tmp = @json_conf.get('assertions') || []
      @assertions = tmp.map do |a|
        if a.class == Hash or a.size >= 3
          # [a[0], a[1].to_sym, a[2]]
          # [a[0], a[1], a[2]]
          a
        else
          raise "miss formatted conf file in assertions"
        end
      end
      
      ## TODO: dump output to a specific directory
      # 
      
      ## TODO: continue on down...
      # 

      ##
      @tester = PAgent::HTML.new(@url)
      #puts "_@tester: " + @tester.to_s
      #puts "_@json_conf: " + @json_conf.to_s

      ## If there is a 
      begin
        @tester.form_from_conf(@json_conf)
        form_name = @json_conf.get('form')
        @resultant = @tester.submit(form_name)
      rescue
        @resultant = @tester        
      end

    end

  end


  ## Testing to see if pages (actually PAgents) have certain specific
  ## properties.
  class PageVerifier

    attr_queue :error, :warning
    attr_reader :comments, :comment_properties
    
    def initialize (pagent)
      # puts "runner class2: " + runner.class.to_s
      @pagent = pagent
      # @tests = runner.tests
      # @assertions = runner.assertions

      ##
      @comment_regexp = Regexp.new('\<\!\-\-.*\-\-\>')
      @comments = @pagent.content.scan(@comment_regexp)

      ##
      @comment_properties = nil
      ## Essentially, two values embedded in an HTML comment separated
      ## by an equals sign.
      value = '[\w\"\'\_\-\.]+'
      @quality_comment_regexp =
        Regexp.new('\<\!\-\-\s*(' + value + ')\s*\=\s*(' + value + ')\s*\-\-\>')
      @comment_properties = Hash.new 
      # puts "comments: " + comments.to_s
      @comments.each do |c|

        # puts "c: " + c

        results = c.match(@quality_comment_regexp)
        if not results.nil?

          matches = results[1..2]
          if matches.size == 2

            ## Remove trailing and leading quotes.
            matches = matches.map do |s|
              s = s.gsub(/^[\"\']/, '')
              s = s.gsub(/[\"\']$/, '')
            end

            ##
            key = matches[0]
            value = matches[1]
            # puts "c: #{key} : #{value}"
            @comment_properties[key] = value
          end
        end
      end

    end

    ###
    ### Utils for test writing.
    ###

    def content
      @pagent.content
    end

    ##
    def comment_property (name)
      # puts "in cp: #{name}"
      # puts "cp: cp1: #{@comment_properties}"
      # puts "cp: cp2: #{@comment_properties.class}"
      # $foo = name
      # $bar = @comment_properties
      res = @comment_properties.fetch(name, nil)
      # puts "cp: res: #{res}"
      res
    end
    
    ###
    ### Assertions about comment properties.
    ###

    ## Make an assertion out of strings
    # def assert (lval_key, boper, rval)
    def assert (arg_bundle)

      ## 
      sub = arg_bundle.fetch('sub', nil)
      rval = arg_bundle.fetch('arg', nil)
      op = arg_bundle.fetch('op', nil)
      type = arg_bundle.fetch('as', nil)

      ##
      lval = comment_property(sub)
      
      # puts "lval_key: #{lval_key}"
      # puts "lval: #{lval}"

      ##
      # decomp = boper.split('_')
      # type = decomp[0]
      # op = decomp[1].to_sym
      
      # puts "pre"
      # puts "type: #{type}"
      # puts "op: #{op}"

      ##
      case type
      when 'i'
        lval = lval.to_i
        rval = rval.to_i
      when 'f'
        lval = lval.to_f
        rval = rval.to_f
      else
        lval = lval.to_s
        rval = rval.to_s
      end
      
      # puts "post"
      # puts "lval: #{lval}"
      # puts "lval.class: #{lval.class}"
      # puts "op: #{op}"
      # puts "op.class: #{op.class}"
      # puts "rval: #{rval}"
      # puts "rval.class: #{rval.class}"

      ##
      meth = lval.method op
      meth.call rval
    end

    ###
    ### Boolean tests.
    ###

    ## TODO: Yeah, need to get more codes here eventually.
    def okay?
      if not @pagent.okay?
        error("returned with an error")
        false
      else
        true
      end
    end

    ## TODO: Yeah, need to get more codes here eventually.
    def code?
      if not @pagent.code.eql?("200")
        error("returned without a 200 code")
        false
      else
        true
      end
    end

    ## Check that all of the links are well...
    def links?

      ## Capture links, if possible...
      new_links = [];
      if not @pagent.okay?
        error("unable to get links due to bad page")
      elsif not code?
        error("not trying to get links due to bad page")
      else
        new_links = @pagent.links 
      end

      ## Collect new agents from links.
      ## Check that all pagents from links do not produce errors and
      ## have a decent code. Not such a fatal thing.
      all_links_pure_and_true = true
      new_links.each do |link|

        # puts "\tlink \"#{link}\""
        # puts "\t\tclass \"#{link.class}\""

        ## Did the click return a page? 
        pagent = @pagent.click(link)
        if pagent.nil?
          warning("link \"#{link}\" returned error")
          all_links_pure_and_true = false
        else

          ##
          mini_test = PageVerifier.new(pagent)
          if not mini_test.okay?
            warning("link \"#{pagent.uri}\" returned error")
            all_links_pure_and_true = false
          elsif not mini_test.code?
            warning("link \"#{pagent.uri}\" returned error")
            all_links_pure_and_true = false
          end
          
        end
        
      end
      
      all_links_pure_and_true
    end


    ##3

    # ###
    # ### Do everything we can tests.
    # ###
    
    # def do_all
      
    #   # num = comment_property("NUMBER_OF_RESULTS").to_i
    #   # if num > 100
    #   #   puts "\tX is okay!"
    #   # else
    #   #   puts "\tX is bad!"
    #   # end
      
    # end
    
  end

end


###
### Self-test the modules we have.
###


if __FILE__ == $0

  require 'test/unit'
  
  ## Test the job thingy to make sure that it's working.
 class AUnit_PageVerifier_Test_Suite < Test::Unit::TestCase

    ## Run before every test.
    def setup

      ## TODO: let's just assume that AmiGO is actually installed locally.
      # @aconf = AmiGO::Conf.new('./config.json')
      test_url = 'http://localhost/cgi-bin/amigo/page_test?mode=links_101'
      p = PAgent::HTML.new(test_url)
      @pv = AUnit::PageVerifier.new(p)
    end

    ## Run after every test.
    def teardown
    end

    ##
    def test_simple_links_101

      assert(@pv.okay?, "got something")
      assert(@pv.code?, "looks nice")
      assert(! @pv.links?, "there should be bad links")

      assert(@pv.warnings?, "there should be warnings")
      assert(! @pv.errors?, "there should not be errors")

      assert(@pv.warnings.size == 5, "there should be that many warnings")
      
    end

  end

end

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

    attr_reader :resultant_page

    @json_conf = nil
    @tester = nil

    def initialize (file_str)
      @json_conf = AmiGO::Conf.new(file_str)
    end

    ##
    def over_page (url)

      @tester = PAgent::HTML.new(url)
      #puts "_@tester: " + @tester.to_s
      #puts "_@json_conf: " + @json_conf.to_s
      @tester.form_from_conf(@json_conf)

      form_name = @json_conf.get('form')
      @resultant_page = @tester.submit(form_name)

    end

  end


  ## Testing to see if pages (actually PAgents) have certain specific
  ## properties.
  class PageVerifier

    attr_queue :error, :warning

    def initialize (pagent)
      @pagent = pagent
    end

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
    
    #     ## TODO: Here's a real meaty one...
    #     def debug_property (name, value)    
    #     end
    
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

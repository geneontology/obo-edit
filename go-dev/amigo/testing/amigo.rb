#!/usr/bin/ruby1.8 -w
####
#### NOTE: Right now, needs to run from "go-dev/amigo/testing".
#### Run tests: ./amigo.rb
####


module AmiGO

  ##
  require 'json/pure'

  ##
  class Conf

    @conf_hash = {};

    def initialize (thingy)

      ## Switch on incoming hash or file identifying a JSON file.
      if thingy.is_a?(Hash)

        @conf_hash = thingy

      elsif thingy.is_a?(String)

        ## If extant, bring in the JSON config.
        if File.exists?(thingy) && File.readable?(thingy)
          json_str = IO.readlines(thingy).to_s
          if json_str
            @conf_hash = JSON.parse json_str
          end
        
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

end

###
### Self-test the modules we have.
###

if __FILE__ == $0

  require 'test/unit'
  include AmiGO
  
  ## Test the job thingy to make sure that it's working.
  class AmiGO_Conf_Test_Suite < Test::Unit::TestCase

    ## Run before every test.
    def setup
      @aconf = AmiGO::Conf.new('./config.json')
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

#!/usr/bin/ruby1.8 -w
####
#### NOTE: Right now, needs to run from "go-dev/amigo/testing".
#### Run tests: ./amigo.rb
####
#### require 'testing/amigo.rb'
#### include AmiGO
#### a = AmiGO::Conf.new()
#### a.smart_get('version')
####

module AmiGO

  ##
  require 'json/pure'

  ##
  class Conf

    @conf_hash = {};

    def initialize (thingy=nil)

      ## If nothing came in, see if we can find the default AmiGO
      #config floating around here somewhere...
      if thingy.nil?
        ['./config.json','../config.json','../../config.json'].each do |f|
          if thingy.nil? && File.exists?(f)
            thingy = f
          end
        end
      end

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

      else

        raise "could find no valid configuration"

      end

      ##
      if @conf_hash.empty?
        raise "empty configuration"
      end

      @conf_hash
    end


    ##
    def get (key)

      retval = nil
      [key.upcase, key.downcase, 'AMIGO_' + key.upcase, 'GO_' + key.upcase,
       'amigo_' + key.downcase, 'go_' + key.downcase].each do |k|
        if @conf_hash.has_key?(k)
          retval = @conf_hash.fetch(k, nil)
          break
        end
      end

      retval
    end

    ##
    def smart_get (key)

      retval = get(key)
      if not retval.nil?
        if retval =~ /[0-9]*\.[0-9]+/ 
          retval = retval.to_f
        elsif retval =~ /[0-9]+/ 
          retval = retval.to_i
        end
      end

      retval
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
      @aconf = AmiGO::Conf.new()
    end

    ## Run after every test.
    def teardown
    end

    ##
    def test_configure

      assert(! @aconf.nil?, "got something")

      assert(@aconf.get('blahblahblah').nil?, "nothing for nothing")

      ## Let's pick something unlikely to change dramatically across
      ## amigo versions.
      res = 'templates/pages:templates/includes'
      ['AMIGO_TEMPLATE_PATHS', 'TEMPLATE_PATHS',
       'amigo_template_paths', 'template_paths'].each do |var|
        assert(! @aconf.get(var).nil?, "found something for variable")
        assert(@aconf.get(var).eql?(res), "found variable")
        assert(@aconf.smart_get(var).eql?(res), "found smartly variable")
      end

      ## Check smart_get.
      #lambda do
        thing = @aconf.smart_get('version')
        puts thing
        puts thing.class
        assert(! thing.nil?, "found something for variable")
        assert(thing.class.eql?(Float), "made it a float")
        assert(thing.between?(1.0, 10.0) , "in a good range")
      #end
      
    end
    
  end
  
end

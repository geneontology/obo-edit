#!/usr/bin/ruby -w
####
#### Run in this directory to recursively descend into AmiGO's JS
#### archive and run the *.tests.js unit testing files. 
####

require 'test/unit'

  
## Test the job thingy to make sure that it's working.
class JavaScript_Test_Suite < Test::Unit::TestCase

  ## Run before every test.
  def setup
    test_search = File.join("**", "*.tests.js")
    @test_files = Dir.glob(test_search)
  end

  ## Run after every test.
  def teardown
  end

  ##
  def test_everything

    puts @test_files
    @test_files.each do |filename|

      ## Push dir.
      current_path = Dir.pwd
      path, name = File.split(filename)
      Dir.chdir(path)

      ## Run test.
      okay_p = Kernel.system("smjs", "-f", name)
      assert(okay_p, "testing " + path + '/' + name)

      ## Pop dir.
      Dir.chdir(current_path)
    end
    
  end
  
end

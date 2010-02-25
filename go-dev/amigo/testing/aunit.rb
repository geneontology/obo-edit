#!/usr/bin/ruby1.8 -w
####
#### NOTE: Right now, needs to run from "go-dev/amigo/testing".
#### Run tests: ./aunit.rb
####
#### TODO: needs tests for PageRunner and such (will probably need to
#### mess with the constructor a bit).
####

module AUnit

  require 'utils'
  require 'pagent'

  ## 
  #class DataMissingError < Exception
  #end

  ## A class to run a pagent out of a file.
  class PageRunner
    
    attr_reader :resultant, :continue, :page, :tests, :assertions
    attr_reader :id, :comment, :reference
    
    @json_conf = nil
    @tester = nil
    
    ## We either have to bootstrap from a desc file (with a "page")
    ## and a url, or we have to work from a pagent and have a desc
    ## that doesn't need a "page" (since we are already working from a
    ## pagent).
    def initialize (page_source, desc_source)
      
      # home_url, file_str
      # puts "FOO: #{page_source.class} #{desc_source.class}"
      
      ## After this part, the following should be defined:
      ##   @json_conf : the configuration
      ##   @tester : a pagent to test
      if page_source.is_a?(String) and desc_source.is_a?(String)
        
        @json_conf = AmiGO::Conf.new(desc_source)
        page = @json_conf.get('page') || ''
        if page.eql?('')
          raise "miss formatted conf file in page"      
        end
        @tester = PAgent::HTML.new(page_source + '/' + page)
        
      elsif page_source.is_a?(PAgent) and desc_source.is_a?(AmiGO::Conf)
        
        @json_conf = desc_source
        @tester = page_source
        
      elsif page_source.is_a?(PAgent) and desc_source.is_a?(Hash)
        
        @json_conf = AmiGO::Conf.new(desc_source)
        @tester = page_source
        
      else
        raise "unusable argument types"
      end
      
      ## Pull key info out of the json.
      @id = @json_conf.get('id')
      if @id.nil? then raise "at least need a test id" end
      @comment = @json_conf.get('comment') || ''
      @reference = @json_conf.get('reference') || nil
      @continue = @json_conf.get('continue') || []
      
      ## Pull the boolean tests out of the conf.
      tmp = @json_conf.get('tests') || []
      @tests = tmp.map do |t|
        t.to_sym
      end
      
      ## Pull the assertions out of the conf.
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
      
      ## If there is a form defined and it exists in page, go do
      ## that. Otherwise, just use the current page as the resultant.
      if @tester.form_from_conf(@json_conf)
        form_name = @json_conf.get('form')
        @resultant = @tester.submit(form_name)
      else
        @resultant = @tester        
      end
      
    end

    ## Safely copy out AmiGO.Conf.
    def profile
      Marshal.restore Marshal.dump @json_conf
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
      @safe = true
      # @tests = runner.tests
      # @assertions = runner.assertions

      ## Hang on even if the pagent is rotten. We hant to be able to
      ## use the error and warning facilities still.
      if @pagent.nil?
        @safe = false
        @comment_regexp = nil
        @comments = nil
        @quality_comment_regexp = nil
        @comment_properties = nil
      else

        ## It looks like we're good.
        
        ##
        @comment_regexp = Regexp.new('\<\!\-\-.*\-\-\>')
        
        begin 
          @comments = @pagent.content.scan(@comment_regexp)
        rescue
          error("PAgent is still bad")        
          raise "PAgent is still bad"
        end
        
        ##
        @comment_properties = nil
        ## Essentially, two values embedded in an HTML comment separated
        ## by an equals sign.
        value = '[\w\"\'\_\-\.]+'
        @quality_comment_regexp =
          Regexp.new('\<\!\-\-\s*('+ value +')\s*\=\s*('+ value +')\s*\-\-\>')
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
    end

    def safe?
      @safe
    end
    
    ###
    ### Dumping, logging, record keeping routines.
    ###
    
    def ready_dir (dir)
      ## Make sure that there is a directory or something there...
      if File.exists? dir
        if not File.directory? dir or not File.writable? dir
          raise "There is something wrong with an extant #{dir}"
        end
        false
      else
        Dir.mkdir(dir)
        true
      end
    end

    ## Take id input, catch output errors.
    def dump (output_dir, token_label = "")
      
      ## Work if there is a core, otherwise
      base_name = nil
      full_name = nil
      if @pagent and @pagent.core and @pagent.core.response

        ready_dir(output_dir)
        
        ## Get the best suffix we can.
        suffix = '.nil'
        ct = @pagent.core.response['content-type']
        if ct
          suffix = '.' + ct.split('/')[-1]
          ## Check to see if there is a character on there too...and
          ## whack it off.
          if suffix =~ /charset/
            suffix = suffix.split(';')[0]
          end
        end
        
        ## Good times.
        gtime = Time.now.strftime("%Y%m%d%H%M%S")
        
        ## Complete file name.
        if token_label.eql?('')
          base_name = gtime +'_'+ uuid + suffix
          full_name = output_dir +'/'+ base_name
        else
          slabel = File.basename(token_label, '.t')
          base_name = gtime +'_'+ slabel + suffix
          full_name = output_dir +'/'+ base_name
        end
        
        ##
        begin
          File.open(full_name, 'w') {|f| f.write(@pagent.content)}
        rescue
          full_name = nil
          base_name = nil
        end
        
      end
      
      base_name
    end

    ###
    ### Utils for test writing.
    ###

    def content
      if @pagent and @pagent.content
        @pagent.content
      else
        nil
      end
    end

    ##
    def comment_property (name)
      if @pagent.nil?
        nil
      else
        @comment_properties.fetch(name, nil)
      end
    end
    
    ###
    ### Assertions about comment properties.
    ###

    ## Make an assertion out of strings
    # def assert (lval_key, boper, rval)
    def assert (arg_bundle)

      if @pagent.nil?
        error("PAgent is nil")
        false
      else

        ## 
        sub = arg_bundle.fetch('sub', nil)
        rval = arg_bundle.fetch('arg', nil)
        op = arg_bundle.fetch('op', nil)
        type = arg_bundle.fetch('as', nil)
        
        # puts "sub: #{sub}"
        # puts "sun.class: #{sub.class}"
        
        ##
        lval = comment_property(sub)
        
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
        
        ##
        meth = lval.method op
        res = meth.call rval
        
        # puts "lval: #{lval}"
        # puts "lval.class: #{lval.class}"
        # puts "rval: #{rval}"
        # puts "rval.class: #{rval.class}"
        # puts "op: #{op}"
        # puts "op.class: #{op.class}"
        # puts "type: #{type}"
        # puts "type.class: #{type.class}"
        # puts "meth: #{meth}"
        # puts "res.class: #{res.class}"
        
        if res
          true
        else
          error("Assertion failed")
          false
        end
      end
    end
    
    ###
    ### Boolean tests.
    ###
    
    ##
    def okay?
      if @pagent.nil?
        false
      elsif not @pagent.okay?
        error("returned with an error")
        false
      else
        true
      end
    end

    ## TODO: Yeah, need to get more codes here eventually.
    def code?
      if @pagent.nil?
        false
      elsif not @pagent.code.eql?("200")
        error("returned without a 200 code")
        false
      else
        true
      end
    end

    ## TODO: Yeah, need to get more codes here eventually.
    def ierror?
      if @pagent.nil?
        false
      elsif not @pagent.code.eql?("500")
        error("returned without a 500 code")
        false
      else
        true
      end
    end

    ## Check that all of the links are well...
    def links?

      new_links = [];
      all_links_pure_and_true = true

      ## Capture links, if possible...
      if @pagent.nil?
        all_links_pure_and_true = false
      elsif not @pagent.okay?
        error("unable to get links due to bad page")
      elsif not code?
        error("not trying to get links due to bad page")
      else
        new_links = @pagent.links 
      end
      
      ## Collect new agents from links.
      ## Check that all pagents from links do not produce errors and
      ## have a decent code. Not such a fatal thing.
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
    
  end


  ##
  class Inspector

    attr_reader :directory, :warnings, :errors
    attr_reader :profile_id, :profile_comment, :profile_reference
    attr_reader :generated_files

    ##
    def initialize (out_dir)

      # @base_uri = URI.parse(url)
      @directory = out_dir

      ## Ready hard output location.
      @log_file = @directory +'/'+ 'details.log'
      @log_struct_file = @directory +'/'+ 'struct.log'
    end  
  
  #   def same_host (thingy)
  #     ##
  #     uri = nil
  #     if thingy.class.eql?(URI) 
  #       ## Good--that's what we want.
  #       uri = thingy
  #     elsif thingy.class.eql?(String) 
  #       uri = URI.parse('http://localhost/cgi-bin/foo')
  #     else
  #       raise "I have no idea what this non-URI thing is"
  #     end
  #     @base_uri.host.eql?(uri.host)
  #   end

    ## Clear output directory of html and png files.
    def clear_output_dir (da_dir = @directory)
      Dir.foreach(da_dir) do |f|
        full_name = da_dir + '/' + f
        if File.exists?(full_name)
          if f =~ /^[0-9]{14}\_.*\.(png|gif|html|json)$/
            # puts "_" + full_name
            File.delete(full_name)
          end
        end
      end
    end
    
    ## Delete log files.
    def erase_logs
      [@log_file, @log_struct_file].each do |log_file|
        if File.exists?(log_file)
          File.delete(log_file)
          true
        else
          false
        end
      end
    end
    
    ##
    def ready_dir (dir = @directory)
      ## Make sure that there is a directory or something there...
      if File.exists? dir
        if not File.directory? dir or not File.writable? dir
          raise "There is something wrong with an extant #{dir}"
        end
        false
      else
        Dir.mkdir(dir)
        true
      end
    end

    ## Either :file or :console
    def log (line = "???", place = :console)

      if( place == :file )

        ## Add time to line.
        gtime = "[#{Time.now.strftime("%Y%m%d%H%M%S")}]"
        ready_dir(File.dirname(@log_file))
        line = "#{gtime} #{line}\n"

        ## Append line to file.
        f = File.open(@log_file, 'a')
        f.write(line)
        f.close
      else
        ## STDOUT
        puts line
      end

      line
    end

    ## 
    def write_struct in_hash

      ready_dir(File.dirname(@log_struct_file))
      
      f = File.open(@log_struct_file, 'a')
      f.write(JSON.dump in_hash)
      f.close  
      
      nil
    end

    ##
    def inspect (runner)
      
      log("Trying tests: #{runner.id}: #{runner.comment}")
      log("Trying tests: #{runner.id}: #{runner.comment}", :file)

      #
      @warnings = []
      @errors = []

      ## Things that touch on the runner.
      @profile_id = runner.id
      @profile_reference = runner.reference
      @profile_comment = runner.comment
      @generated_files = []
      
      ##
      vpage = PageVerifier.new(runner.resultant)
      
      ##
      if vpage.safe?

        ## Walk through tests.
        runner.tests.each do |t|
          log("   Trying test: #{t.to_s}")
          log("   Trying test: #{t.to_s}", :file)
          result = vpage.send t
          # puts "\t\t" + result.to_s
        end
        
        ## Walk through assertions.
        runner.assertions.each do |a|
          # puts "a: #{a}"
          # puts "a.class: #{a.class}"
          # puts "a[0]: #{a[0]}"
          # puts "a[1]: #{a[1]}"
          # puts "a[2]: #{a[2]}"
          lval_key = a.fetch('sub', nil)
          op = a.fetch('op', nil)
          rval = a.fetch('arg', nil)
          asstr = "#{lval_key.to_s} #{op.to_s} #{rval.to_s}"
          asstr_plus = "   Try assertion: #{asstr}"
          log(asstr_plus)
          log(asstr_plus, :file)
          ans = vpage.assert(a)
          log("      ==> #{ans.to_s}")
          log("      ==> #{ans.to_s}", :file)
        end
        
        ## Output: dump file and capture file name.
        @generated_files.push(vpage.dump(@directory, runner.id))
        
        ## Check to see if there are any continues; if so, run them and
        ## flatten the output.
        runner.continue.each do |next_desc|

          log("   Do a continue <==")
          log("   Do a continue <==", :file)

          ## Run the item described in the continue.
          next_pagent = runner.resultant
          next_runner = PageRunner.new(next_pagent, next_desc)

          # next_vpage = PageVerifier.new(next_runner.resultant)

          ## (Recursively) inspect the runner.
          next_inspector = Inspector.new(@directory)
          next_inspector.inspect(next_runner)
          
          ## Just flatten and pass up problems from below.
          next_inspector.warnings.each{ |s| @warnings.push(s) }
          next_inspector.errors.each{ |s| @errors.push(s) }
          next_inspector.generated_files.each{ |f| @generated_files.push(f) }
        
        end
        
      else
        ## There is no PV to add an error to, so do it manually.
        log("*  Failed to get a testable PageVerifier: \"#{$!}\"", :file)
        vpage.error("Failed to get a testable PageVerifier: \"#{$!}\"")
      end  
      
      ## Add the various warnings and errors to the log and hash.
      vpage.warnings.each do |w|
        log("#{runner.id}: warning: #{w}", :file)
        @warnings.push(w)
      end
      vpage.errors.each do |e|
        log("#{runner.id}: ERROR: #{e}", :file)
        @errors.push(e)
      end
      
      nil
    end

  end

end


###
### Self-test the modules we have.
###


if __FILE__ == $0

  require 'test/unit'
  require 'amigo'
  
  ## Test the job thingy to make sure that it's working.
  class AUnit_PageVerifier_Links_101_Test_Suite < Test::Unit::TestCase
    
    ## Run before every test.
    def setup
      ## TODO: let's just assume that AmiGO is actually installed locally.
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
  
  ## Test the job thingy to make sure that it's working.
  class AUnit_Simple_Test_Test_Suite < Test::Unit::TestCase
    
    ## Run before every test.
    def setup
    end
    
    ## Run after every test.
    def teardown
    end
    
    ##
    def test_comment_assertions
      
      test_url = 'http://localhost/cgi-bin/amigo/page_test?mode=simple_test'
      pa = PAgent::HTML.new(test_url)
      pv = AUnit::PageVerifier.new(pa)
      
      ##
      assert(pv.okay?, "page living")
      assert(pv.code?, "page has good code")
      assert(pv.comments.size == 2, "page has two comments")

      ## Work through different string properties: FOO = "BAR"
      assert(pv.assert({"sub"=>"FOO", "op"=>"==", "arg"=>"FOO", "as"=>"s"}),
             "\"FOO\" == \"FOO\"")
      assert(! pv.assert({"sub"=>"FOO", "op"=>"==", "arg"=>"BAR", "as"=>"s"}),
             "not \"FOO\" == \"BAR\"")
      assert(! pv.assert({"sub"=>"FOO", "op"=>">", "arg"=>"FOO", "as"=>"s"}),
             "not \"FOO\" > \"FOO\"")
      assert(pv.assert({"sub"=>"FOO", "op"=>">", "arg"=>"BAR", "as"=>"s"}),
             "\"FOO\" > \"BAR\"")

      ## Work through different int properties: ONE = 1
      assert(pv.assert({"sub"=>"ONE","op"=>"==","arg"=>1,"as"=>"i"}),
             "1 == 1 (int arg)")
      assert(pv.assert({"sub"=>"ONE","op"=>"==","arg"=>"1","as"=>"i"}),
             "1 == 1 (str arg)")
      assert(pv.assert({"sub"=>"ONE","op"=>"<","arg"=>2,"as"=>"i"}),
             "1 < 2 (int arg)")
      assert(pv.assert({"sub"=>"ONE","op"=>"<","arg"=>"2","as"=>"i"}),
             "1 < 2 (str arg)")
      assert(! pv.assert({"sub"=>"ONE","op"=>">","arg"=>2,"as"=>"i"}),
             "not 1 > 2 (int arg)")
      assert(! pv.assert({"sub"=>"ONE","op"=>">","arg"=>"2","as"=>"i"}),
             "not 1 > 2 (str arg)")
    end
    
    ## We're also doing some direct manipulation here.
    def test_comment_assertions_after_continue
      
      test_url = 'http://localhost/cgi-bin/amigo/page_test?mode=echo_form'

      ## 
      pa1 = PAgent::HTML.new(test_url)
      pv1 = AUnit::PageVerifier.new(pa1)
      assert(pv1.okay?, "page living")
      assert(pv1.code?, "page has good code")
      assert(pv1.comments.size == 0, "page has zero comments")

      ## 
      pa1.set_field("echo", "name", "foo")
      pa1.set_field("echo", "value", "bar")

      ## 
      pa2 = pa1.submit("echo")
      pv2 = AUnit::PageVerifier.new(pa2)
      assert(pv2.okay?, "page living")
      assert(pv2.code?, "page has good code")
      assert(pv2.comments.size == 2, "page has two comments")      

    end

    ##
    def test_comment_assertions_with_template
      
      test_url = 'http://localhost/cgi-bin/amigo/page_test?mode=echo_form'

      ## 
      pa = PAgent::HTML.new(test_url)
      form_hash = {"form" => "echo", "field" => {"name" =>"foo", "value"=>"1"} }
      pa.form_from_conf(AmiGO::Conf.new(form_hash))
      pa2 = pa.submit("echo")
      pv = AUnit::PageVerifier.new(pa2)

      assert(pv.okay?, "page living")
      assert(pv.code?, "page has good code")
      assert(pv.comments.size == 2, "page has two comments")

      assert(pv.assert({"sub"=>"name","op"=>"==","arg"=>"foo","as"=>"s"}),
             "name is \"foo\"")
      assert(pv.assert({"sub"=>"value","op"=>"==","arg"=>"1","as"=>"i"}),
             "value is 1")
    end

    ## TODO/BUG: Technically, we need to add a PAgent::JSON subclass...
    def test_timeout_catch      
      test_url =
        'http://localhost/cgi-bin/amigo/form_test?mode=timeout&seconds=300'
      puts "\nTESTING TIMEOUT--THIS WILL TAKE A WHILE..."
      pa = PAgent::HTML.new(test_url)
      # pv = AUnit::PageVerifier.new(pa)
      assert(! pa.okay?, "should fail on timeout")
    end

    ## TODO: needs tests for PageRunner and such (will probably need
    ## to mess with the constructor a bit).

  end
  
end

#!/usr/bin/ruby1.8 -w
####
#### Run tests: ./utils.rb
####


## Mixin for simple logging and error tracking.
#module AttributeArray

## Given the symbols, I want to have the following methods:
##  <label> "string" => "string" # adds string <label>-keyed array
##  <label>s => ["1", "2"] # returns <label>-keyed array
##  <label>s? => true or false # checks if <label>-keyed array is empty
class Module

  def attr_queue (*args)

    args.each do |symbol|

      ##
      iv_str = "@#{symbol}"

      ##
      push_symbol = symbol
      define_method(symbol) { |thing|

        # puts "in pusher..."

        ## 
        if not instance_variable_defined?(iv_str)
          iv = instance_variable_set(iv_str, Array.new)
          # puts "\tdefining #{iv_str}"
          # puts "\t\twhich is an #{iv.class} of size #{iv.size}"
        end
        iv = instance_variable_get(iv_str)

        # puts "\tnow have (1) #{iv}"
        # puts "\t\twhich is an #{iv.class} of size #{iv.size}"

        # puts "\twant to push: \"#{thing}\""
        iv.push(thing)
        #iv = instance_variable_set(iv_str, iv.push(thing))

        # puts "\tnow have (2) #{iv}"
        # puts "\t\twhich is an #{iv.class} of size #{iv.size}"

        iv
      }

      ## Return the value of the array.
      dump_symbol = "#{symbol.id2name}s".to_sym
      define_method(dump_symbol) {

        # puts "in dumper"

        ##
        if not instance_variable_defined?(iv_str)
          iv = instance_variable_set(iv_str, Array.new)
        else
          iv = instance_variable_get(iv_str)
        end
      }

      ##
      empty_symbol = "#{symbol.id2name}s?".to_sym
      define_method(empty_symbol) {

        # puts "in checker"

        if not instance_variable_defined?(iv_str)
          # puts "\toops, wasn\'t defined: false"
          iv = instance_variable_set(iv_str, Array.new)
          false
        else
          ##
          iv = instance_variable_get(iv_str)
          if iv.size == 0
            # puts "\tdefined, size 0: true"
            false
          else
            # puts "\tdefined, size > 0: false"
            true
          end
        end
      }

    end

  end

end


  ###
  ### Standalones.
  ###

#   ## TODO: should log? See AmiGO.
#   def kvetch (msg)
#     puts msg
#   end
  

###
### Self-test.
###


if __FILE__ == $0

  require 'test/unit'
 # include AttributeArray

  class Klass

    attr_queue :warning, :error

    def test number

      if number < 0
        error "less than 0: #{number}"
      elsif number == 0
        warning "greater than 0: #{number}"
      else
        # whatever
      end

      true
    end

    def have_errors?
      errors?
    end

    def have_warnings?
      warnings?
    end

    def the_errors
      errors
    end

    def the_warnings
      warnings
    end

  end

  
  ## Test the job thingy to make sure that it's working.
  class Mixin_Test_Suite < Test::Unit::TestCase

    ## Run before every test.
    def setup
      @klass = Klass.new
    end

    ## Run after every test.
    def teardown
    end

    ##
    def test_indep

      @klass.test(-1)

      assert( @klass.have_errors?, "should be one error")
      assert( ! @klass.have_warnings?, "should be no warnings")
    end

    ##
    def test_count

      @klass.test(-2)
      @klass.test(-1)
      @klass.test(0)
      @klass.test(1)

      assert( @klass.have_errors?, "should be errors")
      assert( @klass.have_warnings?, "should be warnings")
      assert( @klass.the_errors.size == 2, "should be two errors")
      assert( @klass.the_warnings.size == 1, "should be one warning")
      assert( @klass.the_errors[0].eql?("less than 0: -2"),
              "should be the right text")
    end

  end

end

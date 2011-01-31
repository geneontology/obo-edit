#!/usr/bin/ruby -w
####
#### Amibiguous operator toy. Licence still unknown, so if I can't
#### figure it out "trivially" reimplement it from McCarthy
#### (e.g. http://community.schemewiki.org/?amb) or one of the various
#### other canonical sources.
####
#### The lower wrapper will be mine.
####

class Amb

  class ExhaustedError < RuntimeError; end

  def initialize
    @fail = proc { fail ExhaustedError, "amb search exhausted" }
  end

  def choose(*choices)
    prev_fail = @fail
    callcc { |sk|
      choices.each { |choice|
        callcc { |fk|
          @fail = proc {
            @fail = prev_fail
            fk.call(:fail)
          }
          if choice.respond_to? :call
            sk.call(choice.call)
          else
            sk.call(choice)
          end
        }
      }
      @fail.call
    }
  end

  def search
    choose
  end

  def failure
    choose
  end

  def assert(cond)
    failure unless cond
  end

end


# ## Wrap out the cruft.
# class AmbiguousSet

#   def initialize
#     @a = Amb.new
#     @variables = []
#     @assertions = []
#     @solution_set = []
#   end
  
#   def variable_set (symbol, *set)
#     @variables.push([symbol, set])
#   end
  
#   def assertion (cond_str)
#     @assertions.push(cond_str)
#   end
  
#   def search

#     begin

#       @variables.each do |sym, set|
#         ## a = @a.choose(*(0..4))
#         sym = @a.choose(set)
#       end

#       @assertions.each do |assertion|
#         ## @a.assert( a < b )
#         @a.assert( Kernel.eval(assertion) )
#       end

#       ## TODO: add explosion failsafe here.
      
#       @a.failure

#     rescue Amb::ExhaustedError
#       puts "No More Solutions"
#     end

#   end

# end


###
### My unit tests for it (in case of reimp).
###


if __FILE__ == $0

  require 'test/unit'
  #include Amb

  ## http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-16.html#node_chap_14  
  class Amb_Test_Suite < Test::Unit::TestCase

    ## Run before every test.
    def setup
      @a = Amb.new
    end

    ## Run after every test.
    def teardown
    end

    def test_one

      solutions = []

      begin
        a = @a.choose(*(0..4))
        b = @a.choose(*(0..4))
        c = @a.choose(*(0..4))

        @a.assert( a < b )
        @a.assert( a + b == c )
        assert( (a < b) && (a + b == c) )
        solutions.push({:a => a, :b => b, :c => c})
        @a.search

      rescue Amb::ExhaustedError
        #puts "No More Solutions"
      end

      assert(solutions.length == 6)

    end

  end


#   class AmbiguousSet_Test_Suite < Test::Unit::TestCase

#     ## Run before every test.
#     def setup
#       @as = AmbiguousSet.new
#     end

#     ## Run after every test.
#     def teardown
#     end

#     def test_one

#       @as.variable_set(:a, *(0..4))
#       @as.variable_set(:b, *(0..4))
#       @as.variable_set(:c, *(0..4))
      
#       @as.assertion( ":a < :b" )
#       @as.assertion( ":a + :b == :c" )

#       @as.search

#     end

#  end

end

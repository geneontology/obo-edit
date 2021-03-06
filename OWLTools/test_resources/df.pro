 % -- ********************************************************* --
 % -- Autogenerated Prolog factfiles 
 % -- see http://www.blipkit.org for details 
 % -- ********************************************************* --


 % -- Property/Slot --
property('develops_from').
is_transitive('develops_from').

 % -- Property/Slot --
property('directly_develops_from').
subclass('directly_develops_from', 'develops_from').

class('a').
subclass('a', 'IndependentContinuant').
restriction('a', 'directly_develops_from', 'b').

class('b').
subclass('b', 'IndependentContinuant').
restriction('b', 'directly_develops_from', 'c').

class('directly_develops_from_c').
genus('directly_develops_from_c', 'IndependentContinuant').
differentium('directly_develops_from_c', 'directly_develops_from', 'c').

class('develops_from_c').
genus('develops_from_c', 'IndependentContinuant').
differentium('develops_from_c', 'develops_from', 'c').


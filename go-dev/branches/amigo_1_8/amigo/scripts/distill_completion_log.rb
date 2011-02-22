#!/usr/bin/ruby -w
####
#### Outputs the useful information from the completion log file.
####
#### Usage:
####    distill_completion_log.rb term /log/file
####    distill_completion_log.rb gene_product /log/file
####

## Pre-compile IP REs.
ips_to_ignore = ['131.243','64.81.54.176']
regexps_to_ignore = []
ips_to_ignore.each do |ip|
  cleaned_ip = Regexp.escape(ip)
  regexps_to_ignore.push(Regexp.new("^" + cleaned_ip))
end

## Check type.
type = ARGV[0]
if ! type
  exit(-1)
end

## Pre-compile other REs.
target_re = Regexp.new("completion") # TODO: make this more unique
type_re = Regexp.new("type\=" + type)
query_re = Regexp.new("query\=.*$")

## Target directory.
target_dir = ARGV[1]
if ! target_dir || ! File.readable?(target_dir)
    exit(-1)
end

## Results container.
res_hash = {}

## Processing loop.
f = File.new(target_dir, "r")
while line = f.gets

  ## Is it a line that we want to ignore?
  line_is_okay_p = true # looking good...
  regexps_to_ignore.each do |ipre|
    if ipre.match(line)
      line_is_okay_p = false # nope!
      break
    end
  end

  ## If the line is okay, get the meat out.
  if line_is_okay_p && target_re.match(line) && type_re.match(line)

    str = (line.split(' '))[6]
    
    query_re.match(str)
    final = Regexp.last_match(0)[6..-1]
    if res_hash.has_key?(final)
      res_hash[final] += 1
    else
      res_hash[final] = 1
    end

  end

end
f.close

## Output

##
puts "Completion queries:"
term_sum = 0
res_hash.sort.each do |item|
  key = item[0]
  value = item[1]
  puts "(#{value}) #{key}"
  term_sum += value.to_i
end
puts "Sum: #{term_sum}"

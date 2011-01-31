#!/usr/bin/ruby -w
####
#### Put the lucene indexes onto another machine using scp.
####
#### Usage: scripts/push_indexes.rb /local/sjcarbon/tmp/cgi-bin/amigo/index sjcarbon@ash.lbl.gov:/local/www/cgi-bin/amigo/index
####
#### TODO: target directory needs to be cleaned too.
####

require 'find'

## Source and target directories.
source_dir = ARGV[0]
target_dir = ARGV[1]

## Two args.
if ! source_dir || ! target_dir
    exit(-1)
end

## Source is a readable dir.
if ! File.directory?(source_dir) || ! File.readable?(source_dir)
  exit(-1)
end

## Traverse our neck of the woods and find the index files.
Find.find(source_dir) do |path|

  if FileTest.file?(path)

    #puts "found: #{path}"

    ## Cut it down to size...this is hacky.
    chopped_path = path.slice(source_dir.length, 1000000000)
    #puts "\t do: #{chopped_path}"

    target_file = target_dir + chopped_path
    puts "scp #{path} #{target_file}"
    if system("scp", path, target_file)
      puts "Success!"  
    else
      puts "Failure!"
      exit(-1)
    end
  end
end

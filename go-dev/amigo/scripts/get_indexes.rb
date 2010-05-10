#!/usr/bin/ruby -w
####
#### Pull the lucene index tarball from another machine and unpack it
#### in a specified directory.
####
#### Usage: scripts/get_indexes.rb http://localhost/go_lucene_index.tar.gz /local/www/cgi-bin/amigo
####
#### Note: makes use of "curl", "tar", and "gzip"--they must be in path.
####

## Source and target directories.
source_url = ARGV[0]
target_dir = ARGV[1]

## Two args.
if ! source_url || ! target_dir
    exit(-1)
end

## Target is a readable dir.
if ! File.directory?(target_dir) ||
    ! File.readable?(target_dir) ||
    ! (/amigo$/ === target_dir) then
  puts "ERROR: bad target directory."
  exit(-1)
end

## cd to target dir
Dir.chdir(target_dir)

## TODO:
if system("curl #{source_url} | tar -zxvf -") then
  puts "Created index from tarball."
else
  puts "Failed to create index from tarball."
  exit(-1);
end

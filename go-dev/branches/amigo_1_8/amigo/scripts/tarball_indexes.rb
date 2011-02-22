#!/usr/bin/ruby -w
####
#### Tarball lucene indexes onto a file.
####
#### Usage: scripts/push_indexes.rb /local/sjcarbon/tmp/cgi-bin/amigo /tmp/lucene_index.tar.gz
####
#### Note: makes use of "tar" and "gzip"--they must be in path.
####

## Source and target directories.
source_dir = ARGV[0]
target_file = ARGV[1]

## Two args.
if ! source_dir || ! target_file
  puts "ERROR: must define the two args: source and target."
  exit(-1)
end

## Source is a readable dir and looks to be amigo install/luigi output.
if ! File.directory?(source_dir) ||
    ! File.readable?(source_dir) ||
    ! File.readable?("#{source_dir}/index") ||
    ! (/amigo$/ === source_dir) then
  puts "ERROR: bad source directory."
  exit(-1)
end

## Target does not exist.
if File.exists?(target_file)
  puts "ERROR: target file already exists."
  exit(-1)
end

## cd to source dir
Dir.chdir(source_dir)

##
if system("tar", "-cvzf", target_file, "index") then
  puts "Created index tarball at #{target_file}"
else
  puts "Failed to create index tarball at #{target_file}"
  exit(-1)
end

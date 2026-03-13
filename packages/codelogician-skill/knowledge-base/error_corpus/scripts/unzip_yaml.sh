#!/bin/bash

################################################################################
# Script: unzip_yaml.sh
# Description: Reads a YAML file containing a list of items and creates a
#              directory for each item using its 'name' field. Inside each
#              directory, creates file for repro_iml, solution, and
#              solution_* fields
#
# Usage: ./unzip_yaml.sh <yaml-file>
#
# Arguments:
#   yaml-file    Path to the YAML file containing the list of items
#
# Requirements:
#   - yq (YAML processor)
#
################################################################################

# Check if file argument is provided
if [ $# -eq 0 ]; then
    echo "Usage: $0 <yaml-file>"
    exit 1
fi

yaml_file="$1"

# Check if file exists
if [ ! -f "$yaml_file" ]; then
    echo "Error: File '$yaml_file' not found"
    exit 1
fi

# Get the directory where the yaml file is located
yaml_dir=$(dirname "$yaml_file")

# Get the number of items
count=$(yq 'length' "$yaml_file")

# Loop through each item by index
for i in $(seq 0 $((count - 1))); do
    name=$(yq ".[$i].name" "$yaml_file")
    repro_iml=$(yq ".[$i].repro_iml" "$yaml_file")
    solution=$(yq ".[$i].solution" "$yaml_file")

    # Create directory alongside the yaml file
    target_dir="$yaml_dir/$name"
    mkdir -p "$target_dir"

    # Write repro file
    echo "$repro_iml" > "$target_dir/repro.iml"

    # Write main solution file
    echo "$solution" > "$target_dir/solution.iml"

    # Extract and write all solution_* fields (solution_2, solution_3, etc.)
    solution_keys=$(yq ".[$i] | keys | .[] | select(test(\"^solution_[0-9]+$\"))" "$yaml_file")

    # Build the del() arguments dynamically
    non_other_args=".repro_iml, .solution, .msg_str, .err_msg, .is_po_err"
    for key in $solution_keys; do
        solution_content=$(yq ".[$i].$key" "$yaml_file")
        echo "$solution_content" > "$target_dir/$key.iml"
        del_args="$del_args, .$key"
    done

    # Extract fields other than name, kind, repro_iml, solution, solution_*, err_msg, is_po_err to item.yaml
    yq ".[$i] | del($non_other_args)" "$yaml_file" > "$target_dir/item.yaml"

    echo "Created $target_dir"
done

echo "Done!"

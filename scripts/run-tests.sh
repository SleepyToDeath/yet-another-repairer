#! /bin/bash

this_dir=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )
working_dir="$this_dir/.."

docker build -t repairer "$working_dir"
docker run -it --rm repairer


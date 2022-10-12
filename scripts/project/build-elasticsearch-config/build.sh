#!/bin/bash
{
bundle config set path /tmp/gems
gem install stringio:3.0.2
gem install psych:4.0.6
rm out.json

}&> /dev/null
rm /data/app/config/search/config.json
mkdir -p /data/app/config/search
ruby ./script.rb  >> /data/app/config/search/config.json

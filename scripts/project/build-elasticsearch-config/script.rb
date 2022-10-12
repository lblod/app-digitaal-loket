require 'bundler/inline'
$stdout.sync = true
gemfile do
  source 'https://rubygems.org'
  gem 'stringio'
  gem 'linkeddata'
end

ROLE = "LoketLB-toezichtGebruiker";
SPEC_NAME = "o-toez-rwf";
SPARQL_CLIENT = "http://virtuoso:8890/sparql"
DEFAULT_SETTINGS = %|
                        {
                            "analysis": {
                                "normalizer": {
                                "custom_sort_normalizer": {
                                    "type": "custom",
                                    "char_filter": [],
                                    "filter": ["lowercase", "trim", "asciifolding"]
                                }
                                }
                            }
                        }
                    |

                     
def client
    @client ||= SPARQL::Client.new(SPARQL_CLIENT)
end

def query(role) 
    q = %|
            PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
            PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
            SELECT DISTINCT ?session_group  WHERE {
            ?sessionId ext:sessionGroup/mu:uuid ?session_group;
                        ext:sessionRole ?session_role.
            FILTER( ?session_role = "#{role}" )
            }
        |   
    return  client.query(q)
end

def get_eager_index_groups() 
    data = query(ROLE);
    search_conf = []
    session_group = data.map { |b| b.session_group.value };

    session_group.each do |sg|
        json = %|
            [
                {
                "variables": [],
                "name": "clean"
                },
                {
                "variables": [],
                "name": "public"
                },
                {
                "name": "#{SPEC_NAME}",
                "variables": ["#{sg}", "#{ROLE}"]
                }
            ]

        |
        search_conf << json
      
   end
    return search_conf.join(',');
end


def make_config() 
    groups = get_eager_index_groups();

    config = %| 
        {
        "automatic_index_updates": true,
        "persist_indexes": true,
        "eager_indexing_groups": [#{groups}],
        "default_settings": #{DEFAULT_SETTINGS},
        "types": []
        }
    |
  
    puts config
end

make_config()
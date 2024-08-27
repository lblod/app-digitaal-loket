This script removes all LMB Graphs from the virtuoso triplestore by using `DROP SILENT GRAPH` statements (bypassing mu-auth)

To connect this compose to your virtuoso either:

Add your virtuoso to the script network in your docker-compose.override.yml

```
  virtuoso:
    networks:
      - default
      - script

networks:
  script:
    driver: bridge
    name: debug
```

OR

change the `sparqlEndpoint` in `app.ts` to your exposed sparql endpoint on your host machine: `http://172.17.0.1:8890/sparql` or use `http://host.docker.internal:8890/sparql` if you use docker-desktop

OR

copy this docker-compose.script.yml (without the network) and add it to your docker-compose.override.yml (updating the app volume )
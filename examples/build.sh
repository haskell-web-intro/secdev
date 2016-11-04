#/bin/bash

CMN=common
CLI=reflex-ghcjs-client
SRV=reflex-ghcjs-server

# Build the client
stack build --stack-yaml=${CLI}/stack.yaml

# Build the server
stack build --stack-yaml=${SRV}/stack.yaml

# Copy over the javascript
rm -f ${SRV}/static/js/all-*.js

for js in blobs xss-unsafe xss-safe serverside-xss-safe
do
 cp $(stack path --stack-yaml=${CLI}/stack.yaml --local-install-root)/bin/${js}.jsexe/all.js ${SRV}/static/js/all-${js}.js
done

# Build the frontend free examples
stack build --stack-yaml=${CMN}/stack.yaml

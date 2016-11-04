# Code Examples

This directory contains the code examples that go along with the talk.

# Setup Instructions Without Docker

Most attendees will use the provided Docker containers, so **please skip these instructions!** If you do not wish to install Docker, you can proceed, but there are caveats: Windows users should use an Ubuntu 16.04 VM. Note that these instructions compile [GHCJS](https://github.com/ghcjs/ghcjs) (the Haskell-to-Javascript compiler) from source, which takes hours! (Use the Docker instructions instead, which has everything precompiled.)

Install [`stack`](https://docs.haskellstack.org/en/stable/README/#how-to-install) first:

`curl -sSL https://get.haskellstack.org/ | sh`

or:

`wget -qO- https://get.haskellstack.org/ | sh`


Install [`node.js`](https://github.com/creationix/nvm/):

`curl -sSL https://raw.githubusercontent.com/creationix/nvm/v0.32.1/install.sh | sh`

or:

`wget -qO- https://raw.githubusercontent.com/creationix/nvm/v0.32.1/install.sh | bash`

Then run:

```bash
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" # This loads nvm
nvm install 4.6.0
nvm alias default 4.6.0
nvm use 4.6.0
```

* Go to the correct directory:
```bash
cd examples
```

* The first time you run them (these will take a long time, #3 in particular)

     1. `stack setup --stack-yaml=common/stack.yaml`
     2. `stack setup --stack-yaml=reflex-ghcjs-server/stack.yaml`
     3. `stack setup --stack-yaml=reflex-ghcjs-client/stack.yaml`

* To build the software
```bash
./build.sh
```

## Command Line Examples

* Open an editor to the appropriate file

     * *Intro:*  common/app/Intro.hs
     * *Phantom Types:*  common/app/PhantomTypes.hs
          * _Libraries in:_ common/src/MyStore.hs and common/src/MySafeStore.hs
     * *Servant API:*  common/app/LogApi.hs
     * *Type Families:*  common/app/TypeFamilies.hs
     * *Yuge (saved state):*  common/app/SavedState.hs

## Reflex Examples

* Start the server
```bash
cd reflex-ghcjs-server
stack exec server
```

* Load the web page (browse to http://localhost:8080)

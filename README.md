# Tutorial Attendee Instructions

The exercises in this tutorial require several Haskell libraries and dependencies. We have pre-compiled and bundled up all these dependencies for you. We have also included an editor for Haskell files. We have sandboxed everything, so that you don't have to modify your system libraries or dependencies to follow along with our tutorial.

First, we request that attendees install [Docker](https://www.docker.com/products/docker), which is a tool for running isolated virtual-machine-like environments called images. We provide an image with all the Haskell dependencies pre-placed. It is not necessary to understand any details regarding Docker for the tutorial (but if you are unfamiliar with Docker and want to learn more, [here is a 12 minute youtube video explaining Docker](https://www.youtube.com/watch?v=YFl2mCHdv24)). (Just to clarify, Haskell is unrelated to Docker, we're only using Docker to distribute the compiler and built source)

Now that you have `docker` installed, please fetch the image with all our dependencies.

If you want to install everything to follow along with the whole tutorial, then at the terminal run:

`docker pull mmaz/secdev`

This is a big image (9 gigs) so prepare to wait a while! It contains two things:

* GHC: the Haskell compiler, and serverside dependencies (~5.2 GB)
* GHCJS: the Haskell-to-Javascript compiler, and clientside dependencies (~4 GB)

**NOTE:** If you are low on disk space, you can still follow along with only the serverside examples by running:

`docker pull mmaz/server-secdev`

which does not contain the GHCJS compiler.

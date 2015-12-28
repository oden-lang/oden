FROM haskell:7.10

RUN apt-get update && apt-get install make

RUN cabal update

WORKDIR /src/oden
ADD oden.cabal /src/oden/

RUN cabal sandbox init && cabal install --only-dependencies

ADD . /src/oden

CMD make dist

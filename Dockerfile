FROM haskell:7.10

RUN apt-get update && apt-get install -y make wget

RUN cabal update

WORKDIR /tmp
RUN wget https://storage.googleapis.com/golang/go1.5.2.linux-amd64.tar.gz \
    && tar -C /usr/local -xzf go1.5.2.linux-amd64.tar.gz
ENV PATH $PATH:/usr/local/go/bin

WORKDIR /src/oden
ADD oden.cabal /src/oden/

RUN cabal sandbox init && cabal install --only-dependencies

ADD . /src/oden

CMD make dist

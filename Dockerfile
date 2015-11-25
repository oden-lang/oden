FROM slogsdon/racket

RUN apt-get update && \
  apt-get install -y make git

RUN raco pkg install git://github.com:miniKanren/Racket-miniKanren.git
RUN raco pkg install git://github.com:oden-lang/graph.git

WORKDIR /src/oden
ADD . /src/oden

CMD make clean dist

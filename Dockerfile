FROM ubuntu:20.04 as build
RUN apt-get update && apt-get install -y \
  ghc cabal-install libghc-zlib-dev
RUN cabal new-update
RUN cabal install split parallel random vector
RUN cabal install hip line-drawing

RUN mkdir /usr/src/app
WORKDIR /usr/src/app
COPY . .
RUN ghc -O2 -o main.out main.hs -fprof-auto  -fprof-cafs -fforce-recomp



FROM ubuntu:20.04
RUN apt-get update && apt-get install -y libatomic1
RUN mkdir /usr/src/run
COPY --from=build /usr/src/app /usr/src/run
WORKDIR /usr/src/run
CMD ["/usr/src/run/main.out"]

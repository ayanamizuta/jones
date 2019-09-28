# cloud build用
FROM haskell:8.6.5

WORKDIR ./jones

RUN stack setup && stack build
RUN mkdir bin && stack --local-bin-path ./bin install

EXPOSE 8080
ENV PORT 8080
CMD ["bin/jones"]

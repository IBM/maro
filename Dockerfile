FROM racket/racket:8.5

RUN raco pkg install --auto https://github.com/Quetzal-RDF/rosette.git
RUN raco pkg install --auto json-parsing
RUN raco pkg install --auto https://github.com/dmac/spin.git

COPY solver/*.rkt /usr/src/solver/
EXPOSE 8000

CMD ["racket", "/usr/src/solver/solver_server.rkt"]

FROM swipl

LABEL description="Indexed Prolog Predicates" \
      version="1.0.0" \
      license="GPL-3.0" \
      copyright="Copyright (C) 2021 Alexander Diemand" \
      maintainer="codieplusplus@apax.net" \
      homepage="https://github.com/CodiePP/predidx"

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      automake \
      autoconf \
      pkg-config \
      git \
      build-essential \
      gcc

RUN mkdir -vp /SRC && cd /SRC && git clone https://github.com/CodiePP/predidx.git predidx.git

WORKDIR /SRC/predidx.git

RUN aclocal --force && autoheader --force && autoconf --force

RUN ./configure

RUN make swi

RUN mkdir -v -p ${HOME}/lib/sbcl

RUN mkdir -v -p ${HOME}/.config/swi-prolog

RUN cp -v predidx-Linux ${HOME}/lib/sbcl/predidx

RUN cp -v src/predidx.qlf ${HOME}/lib/sbcl/

RUN echo ":- assertz(file_search_path(sbcl,'${HOME}/lib/sbcl'))." >> ${HOME}/.config/swi-prolog/init.pl

CMD ["swipl","-l","test/t1.pl","-g","test."]
CMD ["swipl","-l","test/t2.pl","-g","test."]
CMD ["swipl","-l","test/t3.pl","-g","test."]


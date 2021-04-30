#!/bin/sh

if [ $# -lt 1 ]; then
  echo "usage: $0 <cmd>"
  exit 1
fi

DOCKERNAME=codieplusplus/predidx

set -e

case $1 in

  build)
    docker build -t ${DOCKERNAME} .
    ;;

  interactive)
    docker run -it --rm ${DOCKERNAME} swipl
    ;;

  *)
    echo "command '$1' not understood."
    ;;

esac

# update docker hub : https://hub.docker.com/r/codieplusplus/predidx

# docker tag ${DOCKERNAME} ${DOCKERNAME}:v1
# docker push ${DOCKERNAME}:v1

FROM agarbuno/simulacion:notas
MAINTAINER Alfredo Garbuno Iñigo "alfredo.garbuno@itam.mx"

RUN install2.r --error --skipinstalled -n -1 \
        patchwork \
        paletteer \
        here \
        doParallel \
        janitor \
        markdown \
        rmarkdown \
        languageserver \
    && rm -rf /tmp/downloaded_packages

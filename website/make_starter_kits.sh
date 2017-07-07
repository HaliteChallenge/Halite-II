#!/usr/bin/env bash

for directory in ../airesources/*; do
    language=$(basename ${directory})
    if [[ "${language}" = "sample_bots" ]]; then
        continue
    fi

    FILES=(${directory}/MyBot.*)
    EXTENSION=${FILES##*.}

    if [[ -d "../airesources/sample_bots/${language}" ]]; then
        MOREFILES=../airesources/sample_bots/${language}/*.${EXTENSION}
    fi

    echo ${language}: ${FILES} ${MOREFILES}

    zip ./assets/starter_kits/${language}.zip ${FILES} ${MOREFILES}
done
#!/bin/bash
set -x

INCLUDES=-Fi/root/ultibo/core/fpc/source/packages/fv/src

function build {
    echo ......................... building $1 *.lpr
    rm -f *.o
    set -x
    docker run --rm -v $(pwd):/workdir markfirmware/ufpc \
     -B \
     -Tultibo \
     -O2 \
     -Parm \
     $2 \
     $INCLUDES \
     @/root/ultibo/core/fpc/bin/$3 \
     *.lpr |& tee build.log
    EXIT_STATUS=$?
    set +x
    if [ "$EXIT_STATUS" != 0 ]
    then
        exit 1
    fi
    grep -i warning build.log
    EXIT_STATUS=$?
    rm build.log
    if [ "$EXIT_STATUS" == 0 ]
    then
        exit 1
    fi
}

function build-QEMU {
    build $1 "-CpARMV7A -WpQEMUVPB" qemuvpb.cfg
}

function build-RPi {
    build $1 "-CpARMV6 -WpRPIB" rpi.cfg
}

function build-RPi2 {
    build $1 "-CpARMV7A -WpRPI2B" rpi2.cfg
}

function build-RPi3 {
    build $1 "-CpARMV7A -WpRPI3B" rpi3.cfg
}

function example {
    EXAMPLE="$1"
    cd $EXAMPLE
    echo
    echo $EXAMPLE
    for TARGET in *
    do
        cd $TARGET
        build-$TARGET $EXAMPLE/$TARGET
        mkdir -p $CIRCLE_ARTIFACTS/$EXAMPLE/$TARGET
        cp -a kernel* $CIRCLE_ARTIFACTS/$EXAMPLE/$TARGET
        cd ..
    done
    cd ..
}

for EXAMPLE in [0-9][0-9]-*
do
    example $EXAMPLE
done

cd Advanced
for EXAMPLE in *
do
    if [ "$EXAMPLE" != "README.md" ]
    then
        example $EXAMPLE
    fi
done
cd ..

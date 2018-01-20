function linux_before_install()
{
    cp -r tools/travis /tmp/
    cd ..
    tar --exclude=.git \
        -c -z -f /tmp/travis/adagl.tar.gz adagl
    docker build --tag adagl /tmp/travis/
}

function linux_script()
{
    docker run -i -t --user=max adagl /bin/bash -c \
           'tar xzvf /src/adagl.tar.gz -C ~ && make -C ~/adagl '

}

${TRAVIS_OS_NAME}_$1

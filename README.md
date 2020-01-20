# adagl

[![Build Status](https://travis-ci.org/godunko/adagl.svg?branch=master)](https://travis-ci.org/godunko/adagl)
[![reuse compliant](https://img.shields.io/badge/reuse-compliant-green.svg)](https://reuse.software/)

> Ada binding for OpenGL/WebGL

This project provides a generic interface OpenGL drawing and two
implementation one for native OpenGL library and another for WebGL.
WebGL implementation leverages a2js (an Ada to JavaScript translator)
and Web API binding to launch the code in a browser.

## Install

Download sources and build:

```
git clone https://github.com/godunko/adagl.git
cd adagl
make all example
# Run native example
./.objs/x86_64-linux/pyramid
```

### Dependencies

It depends on [Matreshka](https://forge.ada-ru.org/matreshka) library
with a2js enabled. Note, to enable a2js you need ASIS installed.

WASM/WebGL port depends on [AdaWebPack](https://github.com/godunko/adawebpack).

Native implementation depends on glew and glfw libraries.

### Install dependecies from RPM on Fedora
Just add our repository then install as usual RPM:

```
curl -o /etc/yum.repos.d/bintray-reznikmm-matreshka.repo \
 https://bintray.com/reznikmm/matreshka/rpm
dnf --assumeyes install --repo bintray--reznikmm-matreshka 
dnf --assumeyes install --repo bintray--reznikmm-matreshka matreshka-devel
dnf --assumeyes install --repo bintray--reznikmm-matreshka libgnatutil
dnf --assumeyes install --repo bintray--reznikmm-matreshka asis
dnf --assumeyes install --repo bintray--reznikmm-matreshka matreshka-a2js
dnf --assumeyes install glew-devel
dnf --assumeyes install glfw-devel
```

## Maintainer

[@Vadim Godunko](https://github.com/godunko).

## Contribute

Feel free to dive in!
[Open an issue](https://github.com/godunko/adagl/issues/new)
or submit PRs.

## License

[BSD](LICENSE) © Vadim Godunko


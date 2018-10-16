# formura [![Build Status](https://travis-ci.org/formura/formura.svg?branch=master)](https://travis-ci.org/formura/formura) [![Download](https://img.shields.io/github/downloads/nushio3/formura/latest/total.svg)](https://github.com/nushio3/formura/releases/download/test/formura)


Formura = Formula Translator - Translator + RA (Reserach Assistant)

- [Download Formura Language Specification (pdf)](https://github.com/nushio3/formura/raw/master/specification/formura-specification.pdf)

- Also c.f. [The paper](https://github.com/nushio3/formura/blob/master/reference/muranushi%2B-fhpc2016.pdf) and [The slides](https://github.com/nushio3/formura/blob/master/reference/slides-fhpc2016.pdf) on Formura @ FHPC2016.

In Formura, you can describe stencil computations just as if you would write the discretized formulae in the papers, without even translating them into Fortran.

The syntax of Formura is kept simple, so that you can easily learn and write simple Formura programs, and you can generate more complex Formura programs from your favorite programming languages.

![really wanted](http://www.projectcartoon.com/cells/cell_13.jpg)


Note: The originary repo is <https://github.com/nushio3/formura>.

# Quick Start

OS: Ubuntu 18.04

## Using docker
### Pull the image of formura

```
docker pull formura/formura
```

### Execute examples

```
git clone git@github.com:formura/formura.git
cd formura/examples/diffusion1
docker run -it --rm -u $UID:$GID -v $PWD:/work formura/formura make run
```

## Build from source
### Preparation for compiling Formura

```
apt install build-essential git mpi-default-dev libtinfo-dev
wget -qO- https://get.haskellstack.org/ | sh # installation of the Haskell tool Stack
```

### Compile Formura

```
git clone git@github.com:formura/formura.git
cd formura
stack install
```

Add the path of the binary execution file of Formura to the `$PATH` variable of your shell.
The path of the binary execution file is, for example, `${HOME}/.local/bin`.

### Execute examples

```
cd formura/examples/diffusion1
make
make run
```

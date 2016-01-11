# Numeric

## Compiling

First you'll need the following dependencies

- Clang 3.5 and its libraries
- LLVM 3.5 and its libraries
- GNU make
- libblas
- liblapack
- liblapacke
- libboost
- A Java JDK

Moreover the following optional dependence is recommended to get good performance: `libopenblas-dev`.

On Debian Jessie it is sufficient to install the following packages:
- build-essential
- clang-3.5
- clang
- libclang-3.5-dev
- llvm-3.5
- libllvm-3.5-dev
- libblas-dev
- liblapack-dev
- liblapacke-dev
- libboost-dev
- zlib1g-dev
- libncurses5-dev
- libopenblas-dev
- openjdk-7-jdk

Once those dependencies installed you can compile the binding generator and
generate the code. Depending on the configuration of the dynamic loader of
your system you may have to set the `LD_LIBRARY_PATH` in such a way that the loader
will find the libraries of LLVM 3.5. Typically the following commands will do the job:

```sh
cd ./lapack-jni/bindgen
LD_LIBRARY_PATH=/usr/lib/llvm-3.5/lib make
cd ..
LD_LIBRARY_PATH=/usr/lib/llvm-3.5/lib make
```

## Getting started

Use the following imports:
```scala
import scala.offheap._
import scala.offheap.numeric._
```

When compiling and running your code, the `.so` file previously compiled in the
`lapack-jni` directory must be made available to the JVM. To achieve this use
the `flag -Djava.library.path=./lapack-jni/build`. (You'll have to adapt the
path depending on where your project is relative to that folder.)

To optimize your code at compile time you may use the `opt` macros as follow:

```scala
import scala.offheap._
import scala.offheap.numeric._
import scala.offheap.internal.macros._

val m = DenseMatrix.rand(2, 2)
val res = opt(allocator, {
  4 * m * m
})
```

Currently the block can contain any number val declarations followed by an
expression. The supported expression are matrices expression limited to:

- Matrix multiplication
- Matrix multiplication by a scalar
- Matrix addition
- Matrix transposition
- Matrix constructor

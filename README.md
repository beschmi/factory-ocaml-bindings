# C++ libraries

To get the required C++ libraries, follow these instructions.

1) download and install NTL from
http://www.shoup.net/ntl/ntl-9.7.1.tar.gz 

**On Linux:**
```
cd src
./configure NTL_GMP_LIP=on SHARED=on
make
sudo make install
```

**On OS X:**
```
cd src
./configure NTL_GMP_LIP=on SHARED=on LIBTOOL="glibtool --tag=CC"
make
sudo make install
```

2) download and install libfactory from
http://www.mathematik.uni-kl.de/ftp/pub/Math/Factory/factory-4.0.2.tar.gz

**On Linux and OS X:**
```
./configure --disable-streamio --without-Singular --disable-static
make
sudo make install
```

# Installation

```
oasis setup
./configure
make
make install
```

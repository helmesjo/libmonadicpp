# libmonadicpp - A C++ library

The `libmonadicpp` C++ library provides <SUMMARY-OF-FUNCTIONALITY>.


## Usage

To start using `libmonadicpp` in your project, add the following `depends`
value to your `manifest`, adjusting the version constraint as appropriate:

```
depends: libmonadicpp ^<VERSION>
```

Then import the library in your `buildfile`:

```
import libs = libmonadicpp%lib{<TARGET>}
```


## Importable targets

This package provides the following importable targets:

```
lib{<TARGET>}
```

<DESCRIPTION-OF-IMPORTABLE-TARGETS>


## Configuration variables

This package provides the following configuration variables:

```
[bool] config.libmonadicpp.<VARIABLE> ?= false
```

<DESCRIPTION-OF-CONFIG-VARIABLES>

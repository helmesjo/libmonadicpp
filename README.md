# monadicpp - A C++ library

The `monadicpp` C++ library provides <SUMMARY-OF-FUNCTIONALITY>.


## Usage

To start using `monadicpp` in your project, add the following `depends`
value to your `manifest`, adjusting the version constraint as appropriate:

```
depends: monadicpp ^<VERSION>
```

Then import the library in your `buildfile`:

```
import libs = monadicpp%lib{<TARGET>}
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
[bool] config.monadicpp.<VARIABLE> ?= false
```

<DESCRIPTION-OF-CONFIG-VARIABLES>

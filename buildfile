./: {*/ -build/} doc{README.md} manifest

# Install into the monadicpp/ subdirectory of, say, /usr/include/
# recreating subdirectories.
#
{doc legal}{*}:
{
  install         = share/doc/monadicpp/
  install.subdirs = true
}

# Don't install tests.
#
tests/: install = false

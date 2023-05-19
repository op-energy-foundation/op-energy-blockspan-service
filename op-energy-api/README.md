# Brief

op-energy-api is just a DSL definition of Swagger Specification for Op-Energy. The use case is a help provided by GHC with type-checking in order not to handle some subset specification errors manually

# How to use

If you have Nix package manager:

```
nix-shell shell.nix
```

then build it and run:

```
cabal run --verbose=0 | jq > /path/to/op-energy/backend/src/oe/api/swagger.json
```



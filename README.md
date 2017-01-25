# yamlua
Lua binding of libYAML (based on lyaml)

## build
```
ninja
```
The output will be in the `./build` folder.

## Motivation
The build system used by `lyaml` repository is 
very complex and no easy way to accommodate 
special needs, such as using `musl-gcc`. 
`Ninja` is very nice build system and platform
independent. This `yamlua` package relies on the  
simple `Ninja` build system together will  
`ninja` (a YAML-to-ninja compiler that I created) 
for every stage of  
the installation.

Note: this package is just a very thin wrapper of the
the great package `lyaml`
which I would like acknowledge.


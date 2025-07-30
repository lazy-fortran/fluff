## Project Purpose

The purpose of this project is to provide a tool like "ruff" for Python in the form of "fluff" for Fortran. We will also support lazy fortran in addition - a dialect that can be converted to usual Fortran via the fortfront CLI or API. fortfront can generate an AST and do semantic analysis for type inference for a typed AST, meaning that we just have to use this information and do further static analysis and advanced code formatting.

## Development

- Build with `fpm build`
- Test with `fpm test`
- Run tests early and often

# QuickBooks for Communards - Halogen Prototype

This is a test frontend for our QuickBooks for Communes server written in
Purescript/Halogen.


## Build

To build the Purescript app:

```sh
npm install
npx spago install
npx spago build
```

Add the `-w` option to re-build on file changes:

```sh
npx spago build -w
```

You can start a dev server with `parcel`:

```sh
npx parcel server index.html
```

Or build for production:

```sh
npx parcel build index.html
```


## License

GPL-3.0

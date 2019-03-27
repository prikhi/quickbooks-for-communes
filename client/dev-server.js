const proxy = require('http-proxy-middleware');
const Bundler = require('parcel-bundler');
const express = require('express');

const bundler = new Bundler('src/index.html');

const app = express();

app.use(
  '/api',
  proxy({
    target: 'https://localhost:3000',
    secure: false,
    pathRewrite: { '^/api': '' },
  })
)

app.use(bundler.middleware())

app.listen(Number (process.env.PORT || 1234))

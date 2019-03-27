function main() {
  require("../output/Main").main();
}


if (module.hot) {
  module.hot.dispose(function() {
  });

  module.hot.accept(function() {
    document.body.innerHTML = '';
    main();
  });
}


main();

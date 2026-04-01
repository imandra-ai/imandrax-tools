//Provides: caml_thread_initialize
function caml_thread_initialize(unit) {
  return 0;
}

//Provides: caml_get_system_rng
function caml_get_system_rng(unit) {
  // Return a function that fills a Uint8Array with random bytes
  return function(buf) {
    if (typeof globalThis.crypto !== 'undefined') {
      globalThis.crypto.getRandomValues(buf);
    } else {
      var crypto = require('crypto');
      var bytes = crypto.randomBytes(buf.length);
      for (var i = 0; i < buf.length; i++) buf[i] = bytes[i];
    }
  };
}

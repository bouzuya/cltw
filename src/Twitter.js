"use strict";

exports.encodeBase64 = function (s) {
  return require("buffer").Buffer.from(s).toString("base64");
};

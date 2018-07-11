"use strict";

exports.fetchImpl = function (options) {
  return function () {
    return require('axios')({
      data: options.body,
      headers: options.headers,
      method: options.method.toLowerCase(),
      responseType: 'text',
      url: options.url
    });
  };
};

exports.textImpl = function (response) {
  return response.data;
};

exports.statusImpl = function (response) {
  return response.status;
};

"use strict";

exports.fetchImpl = function (options) {
  return function () {
    return require('axios')({
      data: options.body,
      headers: options.headers,
      maxContentLength: 1000000,
      method: options.method.toLowerCase(),
      responseType: 'text',
      url: options.url
    });
  };
};

exports.textImpl = function (response) {
  return typeof response.data !== 'string'
    ? JSON.stringify(response.data)
    : response.data;
};

exports.statusImpl = function (response) {
  return response.status;
};

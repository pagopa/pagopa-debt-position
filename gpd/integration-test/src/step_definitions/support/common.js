const axios = require("axios");
const fs = require('fs');

function get(url) {
  return axios.get(url)
  .then(res => {
    return res;
  })
  .catch(error => {
    return error.response;
  });
}

function post(url, body) {
  return axios.post(url, body)
  .then(res => {
    return res;
  })
  .catch(error => {
    return error.response;
  });
}

function put(url, body) {
  return axios.put(url, body)
  .then(res => {
    return res;
  })
  .catch(error => {
    return error.response;
  });
}

function del(url) {
  return axios.delete(url)
  .then(res => {
    return res;
  })
  .catch(error => {
    return error.response;
  });
}

function call(method, url, body) {
  if (method === 'GET') {
    return get(url)
  }
  if (method === 'POST') {
    return post(url, body)
  }
  if (method === 'PUT') {
    return put(url, body)
  }
  if (method === 'DELETE') {
    return del(url)
  }

}

module.exports = {get, post, put, del, call}

const axios = require("axios");
const fs = require('fs');

let properties = JSON.parse(rawdata);

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

module.exports = {get, post, put, del}

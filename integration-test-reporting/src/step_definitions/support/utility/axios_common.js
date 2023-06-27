const axios = require("axios");

function get(url, config) {
    return axios.get(url, config)
         .then(res => {
             return res;
         })
         .catch(error => {
             return error.response;
         });
}

function post(url, body, config) {
    return axios.post(url, body, config)
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


function del(url, config) {
    return axios.delete(url, config)
        .then(res => {
            return res;
        })
        .catch(error => {
            return error.response;
        });
}

module.exports = {get, post, put, del}

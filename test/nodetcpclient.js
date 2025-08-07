"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var Net = require("net");
var rlp = require("readline-promise");
var client = new Net.Socket();
client.connect(5959, 'localhost', function () {
    // console.log("connected");
    // client.write("start.\n");
    // while (true) {
    // }
});
client.on('data', function (data) {
    console.log(data.toString());
});
client.on('close', function (_) {
    console.log('connection closed');
    client.destroy();
});
rlp
    .createInterface({
    terminal: false,
    input: process.stdin,
    // output: client
})
    .each(function (line) {
    client.write(line + '\n');
})
    .then(function (_) {
    console.log('over');
})
    .catch(function (err) {
    throw err;
});

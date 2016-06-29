var calc = require('./calc.js');

var skel_json = calc.skel('(fun x -> x) (fun y -> y y);;');
console.debug(skel_json);
console.debug(JSON.parse(skel_json));
